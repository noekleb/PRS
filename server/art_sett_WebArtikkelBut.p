/* Sett WebButikkArtikkel for av artikler 
   Parametere:  temp-tabell med feltet Artikkelnr 
              eller
                 Liste over rowid's med artikler i parameterstreng:
                   <"ROWID">,<Rowid1,Rowid2..>
              eller
                   Liste over artikkelnr i parameterstreng:
                   <"ARTNR">,<Artnr1,Artnr2..>
   Kommentar: Benytter eksisterende rutiner, varetilvpielogg.p og (evt) eloggtilvpivare.p
   
   Opprettet: 29.07.04 av BHa                  
-----------------------------------------------------------------------------------*/

DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR hQuery      AS HANDLE NO-UNDO.
DEF VAR ix          AS INT NO-UNDO.
DEF VAR httTable    AS HANDLE NO-UNDO.
DEF VAR cTekst      AS CHAR NO-UNDO.
DEFINE VARIABLE ii AS INTEGER     NO-UNDO.
DEFINE VARIABLE cButiker AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lLagerut AS LOGICAL     NO-UNDO.
cButiker = ENTRY(1,icParam).
ASSIGN
    cTekst = ENTRY(1,icParam) + '||'.

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
         NO-LOCK NO-ERROR.
    IF AVAIL ArtBas THEN DO:
        DO:
            IF cButiker = "DELETE" THEN DO:
                FOR EACH artbut WHERE artbut.artikkelnr = artbas.artikkelnr:
                    artbut.deleted = TRUE NO-ERROR.
                END.
            END.
            ELSE DO:
                cButiker = REPLACE(cButiker,";",",").
                FOR EACH artbut WHERE artbut.artikkelnr = artbas.artikkelnr AND NOT CAN-DO(cButiker,STRING(artbut.butik)):
                    artbut.deleted = TRUE.
                END.
                IF ArtBas.WebButikkArtikkel THEN DO ii = 1 TO NUM-ENTRIES(cButiker):
                    IF NOT CAN-FIND(artbut WHERE artbut.artikkelnr = artbas.artikkelnr AND artbut.butik = INT(ENTRY(ii,cButiker))) THEN DO:
                        CREATE artbut.
                        ASSIGN artbut.artikkelnr = artbas.artikkelnr
                               artbut.butik      = INT(ENTRY(ii,cButiker)).
                    END.
                    ELSE DO:
                        FIND artbut WHERE artbut.artikkelnr = artbas.artikkelnr AND artbut.butik = INT(ENTRY(ii,cButiker)) NO-ERROR.
                        IF AVAIL artbut AND artbut.deleted = TRUE THEN
                            artbut.deleted = FALSE.
                    END.
                    RELEASE artbut.
                END.
            END.
            FIND ELogg WHERE 
                 ELogg.TabellNavn     = "ArtBas" AND
                 ELogg.EksterntSystem = "WEBBUT"    AND
                 ELogg.Verdier        = STRING(ArtBas.ArtikkelNr) NO-ERROR NO-WAIT.
            IF NOT LOCKED ELogg THEN 
            DO:
              IF NOT AVAIL ELogg THEN DO:
                  CREATE ELogg.
                  ASSIGN ELogg.TabellNavn     = "ArtBas"
                         ELogg.EksterntSystem = "WEBBUT"   
                         ELogg.Verdier        = STRING(ArtBas.ArtikkelNr).
              END.
              ASSIGN ELogg.EndringsType = IF can-find(FIRST artbut WHERE artbut.artikkelnr = artbas.artikkelnr AND artbut.deleted = FALSE) THEN 1 ELSE 3
                     ELogg.Behandlet    = FALSE.
                     lLagerUt = ELogg.EndringsType = 1.
              RELEASE ELogg.
            END.
            IF lLagerUt = TRUE THEN DO:
                FOR EACH Lager NO-LOCK WHERE
                    Lager.ArtikkelNr = ArtBas.ArtikkelNr:
                    FIND ELogg WHERE 
                         ELogg.TabellNavn     = "Lager" AND
                         ELogg.EksterntSystem = "WEBBUT"    AND
                         ELogg.Verdier        = STRING(Lager.ArtikkelNr)
                                                + chr(1) + string(Lager.butik) NO-ERROR NO-WAIT.
                    IF LOCKED ELogg THEN NEXT.
                    ELSE DO:
                      IF NOT AVAIL ELogg THEN DO:
                          CREATE ELogg.
                          ASSIGN ELogg.TabellNavn     = "Lager"
                                 ELogg.EksterntSystem = "WEBBUT"   
                                 ELogg.Verdier        = STRING(Lager.ArtikkelNr)
                                                  + chr(1) + string(Lager.butik).
                      END.
                      ASSIGN ELogg.EndringsType = 1 
                             ELogg.Behandlet    = FALSE.
                      RELEASE ELogg.
                    END.
                END.
                lLagerUt = FALSE.
            END.
        END.
    END.
    ELSE ocReturn = ocReturn + "Art " + STRING(ihBuffer:BUFFER-FIELD("Artikkelnr"):BUFFER-VALUE) +
                               " ikke tilgj. for oppdatering" + CHR(10).
    hQuery:GET-NEXT().
  END.
END.

DELETE OBJECT hQuery.

IF ocReturn = "" THEN obOk = TRUE.

