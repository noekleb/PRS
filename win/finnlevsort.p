DEF INPUT  PARAMETER cKonvert     AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER cBeskrivelse AS CHAR NO-UNDO.

DEF VAR iLevNr  LIKE LevBas.LevNr NO-UNDO.
DEF VAR iSortId LIKE LevSort.SortId NO-UNDO.

ASSIGN
    iLevNr  = int(ENTRY(1,cKonvert))
    iSortId = ENTRY(2,cKonvert)
    .

FIND LevSort NO-LOCK WHERE
    LevSort.LevNr     = iLevNr AND
    LevSort.SortId    = iSortId NO-ERROR.
IF AVAILABLE LevSort THEN
    cBeskrivelse = LevSort.Beskrivelse.
ELSE
    cBeskrivelse = "** Ugyldig kode **".
