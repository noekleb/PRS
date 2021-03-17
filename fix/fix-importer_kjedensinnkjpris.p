CURRENT-WINDOW:WIDTH = 200.

/* KjedeInnkPris */

DEF VAR cFilNavn       AS CHAR NO-UNDO.
DEF VAR cLinje         AS CHAR FORMAT "x(100)" NO-UNDO.
DEF VAR iLevNr         AS INT NO-UNDO.
DEF VAR cLevKod        AS CHAR NO-UNDO.
DEF VAR lKjedeInnkPris AS DEC NO-UNDO.
DEF VAR lVAreBokNr     AS DEC NO-UNDO.

ASSIGN
    iLevNr     = 14
    lVareBokNr = 10000001
    cFilNavn = "C:\Appdir\Sport1HK\ODLO Pricelist_010_Norway_EUR_Wi08_20071122.csv"
    .

DEF STREAM Inn.

FIND VareBokHode NO-LOCK WHERE
    VareBokHode.VareBokNr = lVareBokNr NO-ERROR.
IF NOT AVAILABLE VareBokHode THEN
DO:
    MESSAGE "Ukjent varebok"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
END.
INPUT STREAM inn FROM VALUE(cFilNavn) NO-ECHO.
LOOPEN:
REPEAT:
    IMPORT STREAM Inn UNFORMATTED cLinje.

    ASSIGN
        cLevKod        = ENTRY(1,cLinje,";")
        lKjedeInnkPris = dec(ENTRY(3,cLinje,";"))
        NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        NEXT LOOPEN.
    IF lKjedeInnkPris = 0 THEN
        NEXT LOOPEN.

    FIND FIRST VareBokLinje exclusive-LOCK WHERE
        VareBokLinje.VareBokNr = lVareBokNr AND
        VareBokLinje.LevNr     = iLevNr     AND
        VareBokLinje.LevKod    = cLevKod NO-ERROR.

    IF AVAILABLE VAreBokLinje THEN
        ASSIGN
        VareBokLinje.KjedeInnkPris = lKjedeInnkPris
        .
    DISPLAY
        VareBokHode.VareBokNr
        iLevNr
        cLevKod
        lKjedeInnkPris
        VareBokLinje.LevKod WHEN AVAILABLE VareBokLinje
        VareBokLinje.Besk WHEN AVAILABLE VareBokLinje
        WITH WIDTH 200.

END. /* LOOPEN */
INPUT STREAM Inn CLOSE.
