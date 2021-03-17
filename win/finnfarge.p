DEF INPUT  PARAMETER cKonvert     AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER cBeskrivelse AS CHAR NO-UNDO.

FIND Farg NO-LOCK WHERE
    Farg.Farg = INT(cKonvert) NO-ERROR.
IF AVAILABLE Farg THEN
    cBeskrivelse = Farg.FArBeskr.
ELSE
    cBeskrivelse = "** Ugyldig kode **".
