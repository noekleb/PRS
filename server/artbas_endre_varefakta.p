/* Oppdatering av varefakta for artikkel
   Parametere: <Artikkelnr>|<VareFakta>
   Opprettet: 29.07.05 av BHa                  
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

FIND ArtBas WHERE ArtikkelNr = DEC(ENTRY(1,icParam,"|")) EXCLUSIVE-LOCK NO-ERROR.
IF AVAIL ArtBas THEN
  ArtBas.VareFakta = ENTRY(2,icParam,"|").
ELSE ocReturn = "Artikkel ikke tilgjengelig for oppdatering av varefakta".

IF ocReturn = "" THEN obOk = TRUE.
