DEF INPUT  PARAMETER cKonvert     AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER cBeskrivelse AS CHAR NO-UNDO.

FIND StrType NO-LOCK WHERE
    StrType.StrTypeId = INT(cKonvert) NO-ERROR.
IF AVAILABLE StrType THEN
    cBeskrivelse = StrType.Beskrivelse.
ELSE
    cBeskrivelse = "** Ugyldig kode **".
