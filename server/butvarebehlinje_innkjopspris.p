DEF INPUT  PARAM irVarebehLinje   AS ROWID NO-UNDO.
DEF INPUT  PARAM icBestHodeFilter AS CHAR NO-UNDO.
DEF INPUT  PARAM icSessionId      AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocValue          AS CHAR NO-UNDO.

DEF VAR iCl AS INT NO-UNDO.

{syspara.i 5 1 1 iCL INT}

FIND Butiker NO-LOCK WHERE
    Butiker.Butik = iCl NO-ERROR.
IF NOT AVAILABLE Butiker OR Butiker.Butik = 0 THEN
DO:
    ocValue = "".
    RETURN.
END.

FIND VareBehLinje NO-LOCK WHERE
    rowid(VareBehLinje) = irVareBehLinje NO-ERROR.
IF NOT AVAILABLE VareBehLinje THEN
DO:
    ocValue = "".
END.
ELSE DO:
    FIND ArtPris NO-LOCK WHERE
        ArtPris.ArtikkelNr = VareBehLinje.ArtikkelNr AND
        Artpris.ProfilNr   = butiker.ProfilNr NO-ERROR.
    IF AVAILABLE ArtPris THEN
        ocValue = string(ArtPris.InnkjopsPris[IF ArtPris.Tilbud THEN 2 ELSE 1]).
    ELSE
        ocValue = "".
END.
