/* Endre RAvdNr for artikler 
   Parametere:  temp-tabell med feltet Artikkelnr 
              eller
                 Liste over rowid's med artikler i parameterstreng:
                   <"ROWID">,<Rowid1,Rowid2..>
              eller
                   Liste over artikkelnr i parameterstreng:
                   <"ARTNR">,<Artnr1,Artnr2..>
   Kommentar: Benytter eksisterende rutiner, varetilvpielogg.p og (evt) eloggtilvpivare.p
   
   Opprettet: 7.01.08 av BHa                  
-----------------------------------------------------------------------------------*/

DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR hQuery      AS HANDLE NO-UNDO.
DEF VAR ix          AS INT NO-UNDO.
DEF VAR httTable    AS HANDLE NO-UNDO.
DEF VAR cGenEan     AS CHAR NO-UNDO.
DEF VAR bHk       AS LOG  NO-UNDO.
DEF VAR cTekst    AS CHAR NO-UNDO.

{syspara.i 2 4 8 cGenEan} 

/* Stopper ugyldige verdier. */
IF entry(1,icParam) = "?" THEN
    RETURN.

{syspara.i 1 1 18 cTekst}
IF CAN-DO("ja,1,true,yes",cTekst) THEN
    bHK = TRUE.
ELSE
    bHK = FALSE.

IF NOT VALID-HANDLE(ihBuffer) AND NUM-ENTRIES(icParam) > 1 THEN DO:
  CREATE TEMP-TABLE httTable.
  httTable:ADD-LIKE-FIELD("ArtikkelNr","ArtBas.ArtikkelNr").
  httTable:TEMP-TABLE-PREPARE("ttArtBas").
  ihBuffer = httTable:DEFAULT-BUFFER-HANDLE.
  IF ENTRY(2,icParam) = "ROWID" THEN
    DO ix = 3 TO NUM-ENTRIES(icParam):
      FIND ArtBas WHERE ROWID(ArtBas) = TO-ROWID(ENTRY(ix,icParam)) NO-LOCK NO-ERROR.
      IF AVAIL ArtBas THEN DO:
        ihBuffer:BUFFER-CREATE().
        ihBuffer:BUFFER-COPY(BUFFER ArtBas:HANDLE).
      END.
    END.
  ELSE
    DO ix = 3 TO NUM-ENTRIES(icParam):
      FIND ArtBas WHERE ArtBas.ArtikkelNr = DEC(ENTRY(ix,icParam)) NO-LOCK NO-ERROR.
      IF AVAIL ArtBas THEN DO:
        ihBuffer:BUFFER-CREATE().
        ihBuffer:BUFFER-COPY(BUFFER ArtBas:HANDLE).
      END.
    END.
END.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " NO-LOCK").
hQuery:QUERY-OPEN().


DO:
  hQuery:GET-FIRST().
  REPEAT WHILE NOT hQuery:QUERY-OFF-END TRANSACTION:

    FIND ArtBas 
         WHERE ArtBas.ArtikkelNr = DECI(STRING(ihBuffer:BUFFER-FIELD("Artikkelnr"):BUFFER-VALUE))
         NO-LOCK NO-WAIT NO-ERROR.
    IF AVAIL ArtBas THEN DO:
        /* Artikkel skal ikke logges før prisrecord er opprettet.      */
        /* Logging for overføring til VPI register og deretter til HK. */
        IF (bHK = FALSE AND (ArtBas.ArtikkelNr > 0 and
                             ArtBas.ArtikkelNr < 8500000)) THEN
        RAPPORT-LOKALE-ARTIKLER:
        DO:
            FIND ELogg WHERE 
                 ELogg.TabellNavn     = "ArtBas"  AND
                 ELogg.EksterntSystem = "TILKORRPOS" AND
                 ELogg.Verdier        = STRING(ArtBas.ArtikkelNr) NO-ERROR.
            IF NOT AVAIL Elogg THEN DO:
                CREATE Elogg.
                ASSIGN ELogg.TabellNavn     = "ArtBas"
                       ELogg.EksterntSystem = "TILKORRPOS"   
                       ELogg.Verdier        = STRING(ArtBas.ArtikkelNr).
            END.
            ASSIGN ELogg.EndringsType = 1
                   ELogg.Behandlet    = FALSE.
            RELEASE ELogg.
        END. /* RAPPORT-LOKALE-ARTIKLER */
    END.
    ELSE ocReturn = ocReturn + "Art " + STRING(ihBuffer:BUFFER-FIELD("Artikkelnr"):BUFFER-VALUE) +
                               " ikke tilgj. for logging" + CHR(10).
    hQuery:GET-NEXT().
  END.
END.

DELETE OBJECT hQuery.

IF ocReturn = "" THEN obOk = TRUE.

