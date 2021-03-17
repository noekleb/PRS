DEF INPUT  PARAMETER cKonvert     AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER cBeskrivelse AS CHAR NO-UNDO.

FIND SaSong NO-LOCK WHERE
    SaSong.SaSong = INT(cKonvert) NO-ERROR.
IF AVAILABLE Sasong THEN
    cBeskrivelse = SaSong.SasBeskr.
ELSE
    cBeskrivelse = "** Ugyldig kode **".
