/* Sjekk om artikkel har leverandørinndeling 
   Opprettet 04.01.06 av BHa
------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR fArtikkelnr AS DEC NO-UNDO.

fArtikkelNr = DEC(ENTRY(1,icParam)).

FIND ArtBas NO-LOCK
     WHERE ArtBas.ArtikkelNr = fArtikkelNr
     NO-ERROR.
IF AVAIL ArtBas AND CAN-FIND(FIRST LevSort WHERE LevSort.LevNr > 0 AND LevSort.StrTypeId = ArtBas.StrTypeId
                             USE-INDEX StrType) THEN
  obOk = TRUE.

