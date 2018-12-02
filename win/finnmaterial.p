DEF INPUT  PARAMETER cKonvert     AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER cBeskrivelse AS CHAR NO-UNDO.

FIND Material NO-LOCK WHERE
    Material.MAtKod = INT(cKonvert) NO-ERROR.
IF AVAILABLE Material THEN
    cBeskrivelse = Material.MatBeskr.
ELSE
    cBeskrivelse = "** Ugyldig kode **".
