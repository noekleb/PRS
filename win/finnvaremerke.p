DEF INPUT  PARAMETER cKonvert     AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER cBeskrivelse AS CHAR NO-UNDO.

FIND VareMerke NO-LOCK WHERE
    VareMerke.VmId = INT(cKonvert) NO-ERROR.
IF AVAILABLE VareMerke THEN
    cBeskrivelse = VareMErke.Beskrivelse.
ELSE
    cBeskrivelse = "** Ugyldig kode **".
