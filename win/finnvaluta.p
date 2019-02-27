DEF INPUT  PARAMETER cKonvert     AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER cBeskrivelse AS CHAR NO-UNDO.

FIND Valuta NO-LOCK WHERE
    Valuta.ValKod = (cKonvert) NO-ERROR.
IF AVAILABLE Valuta THEN
    cBeskrivelse = Valuta.ValLand.
ELSE
    cBeskrivelse = "** Ugyldig kode **".
