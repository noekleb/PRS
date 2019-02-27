/* Import av bilder på artikler 
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

DEF INPUT  PARAM icParam    AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer   AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn   AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK       AS LOG NO-UNDO.

DEF VAR hQuery        AS HANDLE NO-UNDO.
DEF VAR ix            AS INT    NO-UNDO.
DEF VAR httTable      AS HANDLE NO-UNDO.
DEF VAR hwbildeimport AS HANDLE NO-UNDO.
DEF VAR cFilkatalog   AS CHAR   NO-UNDO.

ASSIGN
    cFilkatalog = ENTRY(2,icParam)
    .

IF NOT VALID-HANDLE(ihBuffer) AND NUM-ENTRIES(icParam) > 1 THEN DO:
  CREATE TEMP-TABLE httTable.
  httTable:ADD-LIKE-FIELD("ArtikkelNr","ArtBas.ArtikkelNr").
  httTable:TEMP-TABLE-PREPARE("ttArtBas").
  ihBuffer = httTable:DEFAULT-BUFFER-HANDLE.
  IF ENTRY(3,icParam) = "ROWID" THEN
    DO ix = 4 TO NUM-ENTRIES(icParam):
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

/* Starter bildeimportrutinen som inneholder bildeocx'en. */
IF NOT VALID-HANDLE(hwbildeimport) THEN
    RUN wbildeimport.w PERSISTENT SET hwbildeimport (?).

DO:
  hQuery:GET-FIRST().
  REPEAT WHILE NOT hQuery:QUERY-OFF-END:

    FIND ArtBas 
         WHERE ArtBas.ArtikkelNr = DECI(STRING(ihBuffer:BUFFER-FIELD("Artikkelnr"):BUFFER-VALUE))
         EXCLUSIVE-LOCK NO-WAIT NO-ERROR.

    IF AVAILABLE ArtBas THEN
    DO:

        IF ArtBas.VPIBildeKode <> "" THEN DO:
            IF index(ArtBas.VPIBildeKode,'.jpg') = 0 
                THEN ArtBas.VPIBildeKode = REPLACE(ArtBas.VPIBildeKode,'.jpg','') + ".jpg".
            RUN NyttBilde IN hwbildeimport (INPUT ArtBas.ArtikkelNr, INPUT cFilkatalog, INPUT ArtBas.VPIBildeKode).
        END.
    END.

    hQuery:GET-NEXT().
  END.
END.

IF VALID-HANDLE(hwbildeimport) THEN
    RUN avsluttProcedure IN hwbildeimport.
    /*
    DELETE PROCEDURE hwbildeimport.
    */
DELETE OBJECT hQuery.

IF ocReturn = "" THEN obOk = TRUE.

