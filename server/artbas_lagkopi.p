/* Kopier artikkel 
   Parametere: Fra artikkelnr
               temp-tabell med overstyrte verdier for ny artikkel
   
   Opprettet: 17.11.04 av BHa                  
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR hQuery        AS HANDLE NO-UNDO.
DEF VAR hBuffArtBas   AS HANDLE NO-UNDO.
DEF VAR hBuffArtPris  AS HANDLE NO-UNDO.
DEF VAR hField        AS HANDLE NO-UNDO.
DEF VAR iLopNr        AS INT NO-UNDO.

DEF BUFFER bFromArtBas  FOR ArtBas.
DEF BUFFER bFromArtPris FOR ArtPris.
CREATE BUFFER hBuffArtBas  FOR TABLE "ArtBas".
CREATE BUFFER hBuffArtPris FOR TABLE "ArtPris".

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME).
hQuery:QUERY-OPEN().

FIND bFromArtBas  WHERE bFromArtBas.ArtikkelNr  = DEC(icParam) NO-LOCK NO-ERROR.
FIND bFromArtPris WHERE bFromArtPris.ArtikkelNr = DEC(icParam) NO-LOCK NO-ERROR.

IF AVAIL bFromArtBas AND AVAIL bFromArtPris AND hQuery:IS-OPEN THEN DO TRANSACTION ON ERROR UNDO, LEAVE:

  hBuffArtBas:BUFFER-CREATE().
  hBuffArtBas:BUFFER-COPY(BUFFER bFromArtBas:HANDLE,"ArtikkelNr,lopnr").
  hBuffArtPris:BUFFER-CREATE().
  hBuffArtPris:BUFFER-COPY(BUFFER bFromArtPris:HANDLE,"ArtikkelNr").
  hBuffArtPris:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE = hBuffArtBas:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE.

  hQuery:GET-FIRST().
  REPEAT WHILE NOT hQuery:QUERY-OFF-END:

    hField = hBuffArtBas:BUFFER-FIELD(ihBuffer:BUFFER-FIELD("cNavn"):BUFFER-VALUE) NO-ERROR.
    IF NOT VALID-HANDLE(hField) THEN
      hField = hBuffArtPris:BUFFER-FIELD(ihBuffer:BUFFER-FIELD("cNavn"):BUFFER-VALUE) NO-ERROR.

    IF VALID-HANDLE(hField) THEN DO:
      CASE hField:DATA-TYPE:
        WHEN "DECIMAL" THEN hField:BUFFER-VALUE[1] = DEC(ihBuffer:BUFFER-FIELD("cVerdi"):BUFFER-VALUE).
        WHEN "DATE"    THEN hField:BUFFER-VALUE = DATE(ihBuffer:BUFFER-FIELD("cVerdi"):BUFFER-VALUE).
        WHEN "INTEGER" THEN hField:BUFFER-VALUE = INT(ihBuffer:BUFFER-FIELD("cVerdi"):BUFFER-VALUE).
        WHEN "LOGICAL" THEN hField:BUFFER-VALUE = LOGICAL(ihBuffer:BUFFER-FIELD("cVerdi"):BUFFER-VALUE).
        OTHERWISE hField:BUFFER-VALUE = ihBuffer:BUFFER-FIELD("cVerdi"):BUFFER-VALUE.
      END CASE.
      IF hField:NAME = "Vg" THEN DO:
        RUN SettLopNr.p (hField:BUFFER-VALUE,"N",OUTPUT iLopNr).
        hBuffArtBas:BUFFER-FIELD("lopnr"):BUFFER-VALUE = iLopNr.
      END.
      ELSE IF hField:NAME = "Beskr" THEN
        hBuffArtBas:BUFFER-FIELD("BongTekst"):BUFFER-VALUE = SUBSTR(hField:BUFFER-VALUE,1,20).
    END.
    hQuery:GET-NEXT().
  END.
END.

IF ocReturn = "" THEN 
  ASSIGN obOk = TRUE
         ocReturn = STRING(hBuffArtBas:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE).
         .

DELETE OBJECT hQuery.
DELETE OBJECT hBuffArtBas.
DELETE OBJECT hBuffArtPris.
