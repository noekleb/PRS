/* Endre webleveringstid for artikler 
   Parametere:  temp-tabell med feltet Artikkelnr 
              eller
                 Liste over rowid's med artikler i parameterstreng:
                   <"ROWID">,<Rowid1,Rowid2..>
              eller
                   Liste over artikkelnr i parameterstreng:
                   <"ARTNR">,<Artnr1,Artnr2..>
   Kommentar: Benytter eksisterende rutiner, varetilvpielogg.p og (evt) eloggtilvpivare.p
   
   Opprettet: 30.05.09 av TN                  
-----------------------------------------------------------------------------------*/

DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR hQuery      AS HANDLE NO-UNDO.
DEF VAR ix          AS INT NO-UNDO.
DEF VAR httTable    AS HANDLE NO-UNDO.
DEFINE VARIABLE cParaListe AS CHARACTER NO-UNDO.
DEF VAR cUnderKatListe AS CHAR NO-UNDO.
DEF VAR piLoop AS INT NO-UNDO.

ASSIGN
  cParaListe     = entry(1,icParam)
  cUnderKatListe = entry(2,cParaListe,'|').
  
/* Stopper ugyldige verdier. */
IF entry(1,cParaListe,'|') = "?" THEN
    RETURN.
    
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
         EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
    IF AVAIL ArtBas THEN
    DO:
        ASSIGN 
        ArtBas.HovedKatNr = INTEGER(entry(1,cParaListe,'|')).
        .
        FOR EACH ArtBasUnderkategori EXCLUSIVE-LOCK WHERE
            ArtBasUnderKategori.ArtikkelNr = ArtBas.ArtikkelNr:
            DELETE ArtBasUnderkategori.
        END.
        IF TRIM(cUnderKatListe) <> '' THEN
        DO piLoop = 1 TO NUM-ENTRIES(cUnderKatListe,CHR(1)):
            CREATE ArtBAsUnderkategori.
            ASSIGN
                ArtBasUnderKategori.ArtikkelNr = ArtBas.ArtikkelNr
                ArtBasUnderKategori.UnderKatNr = INT(ENTRY(piLoop,cUnderKatListe,CHR(1))).
        END.
        ocReturn = ''.
    END.
    ELSE ocReturn = ocReturn + "Art " + STRING(ihBuffer:BUFFER-FIELD("Artikkelnr"):BUFFER-VALUE) +
                               " ikke tilgj. for oppdatering" + CHR(10).
    hQuery:GET-NEXT().
  END.
END.

DELETE OBJECT hQuery.

IF ocReturn = "" THEN obOk = TRUE.

