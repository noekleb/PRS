DEF INPUT  PARAMETER cKonvert     AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER cBeskrivelse AS CHAR NO-UNDO.

FIND HuvGr NO-LOCK WHERE
    HuvGr.Hg = INT(cKonvert) NO-ERROR.
IF AVAILABLE HuvGr THEN
    cBeskrivelse = HuvGr.HgBeskr.
ELSE
    cBeskrivelse = "** Ugyldig kode **".
