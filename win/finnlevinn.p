DEF INPUT  PARAMETER cKonvert     AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER cBeskrivelse AS CHAR NO-UNDO.

FIND LevSort NO-LOCK WHERE
    LevSort.LevNr  = INT(entry(1,cKonvert,";")) AND 
    LevSort.SortId = entry(2,cKonvert,";")
    NO-ERROR.
IF AVAILABLE LevSort THEN
    cBeskrivelse = string(LevSort.LevNr) + " / " + 
                   string(LevSort.SortId) + " / " +
                   LevSort.Beskrivelse.
ELSE
    cBeskrivelse = "** Ugyldig kode **".
