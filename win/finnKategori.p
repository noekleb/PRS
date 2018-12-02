DEF INPUT  PARAMETER cKonvert     AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER cBeskrivelse AS CHAR NO-UNDO.

FIND Kategori NO-LOCK WHERE
    Kategori.KatNr = INT(cKonvert) NO-ERROR.
IF AVAILABLE Kategori THEN
    cBeskrivelse = Kategori.Beskrivelse.
ELSE
    cBeskrivelse = "** Ugyldig kode **".
