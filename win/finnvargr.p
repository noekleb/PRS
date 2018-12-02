DEF INPUT  PARAMETER cKonvert     AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER cBeskrivelse AS CHAR NO-UNDO.

FIND VarGr NO-LOCK WHERE
    VarGr.Vg = INT(cKonvert) NO-ERROR.
IF AVAILABLE VarGr THEN
    cBeskrivelse = VarGr.VgBeskr.
ELSE
    cBeskrivelse = "** Ugyldig kode **".
