DEF INPUT  PARAMETER cKonvert     AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER cBeskrivelse AS CHAR NO-UNDO.

FIND LevBas NO-LOCK WHERE
    LevBas.LevNr = INT(cKonvert) NO-ERROR.
IF AVAILABLE LevBas THEN
    cBeskrivelse = LevBas.LevNamn.
ELSE
    cBeskrivelse = "** Ugyldig kode **".
