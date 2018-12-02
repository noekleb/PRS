CURRENT-WINDOW:WIDTH = 320.

DEF VAR X AS INT NO-UNDO.
DEF VAR cFilNavn   AS CHAR NO-UNDO.
DEF VAR cSep       AS CHAR NO-UNDO.
DEF VAR iSumant    AS INT  FORMAT "->>,>>>,>>9" NO-UNDO.
DEF VAR lSumVerdi  AS DEC  FORMAT "->>>,>>>,>>9.99" NO-UNDO.

DEF VAR lVareBokNr1  AS DEC NO-UNDO.
DEF VAR lVareBokNr2  AS DEC NO-UNDO.
DEF VAR lbufVareBok  AS DEC NO-UNDO.
DEF VAR lVareBehNr  AS DEC  NO-UNDO.

DEF BUFFER bufVareBokLinje FOR VareBokLinje.

ASSIGN
    lVareBokNr1 = 9000007
    lVareBokNr2 = 9000009
    lVareBehNr  = 9000006
    .
FIND VareBokHode NO-LOCK WHERE
    VareBokHode.VareBokNr = lVareBokNr1 NO-ERROR.

DISPLAY
    VareBokHode.VareBokNr
    VareBokHode.VarebokBeskrivelse
    .

FOR EACH VareBokLinje EXCLUSIVE-LOCK OF VareBokHode WHERE
    VareBokLinje.LagerKode <> "":

    IF VareBokLinje.kjedevare = NO THEN
    DO:
        FIND bufVareBokLinje EXCLUSIVE-LOCK WHERE
            bufVareBokLinje.VareBokNr  = lVareBokNr2 AND
            bufVarebokLinje.ArtikkelNr = VareBokLinje.ArtikkelNr NO-ERROR.

        FIND VarebehLinje EXCLUSIVE-LOCK WHERE
            VareBehLinje.VareBehNr  = lVareBehNr AND
            VareBehLinje.ArtikkelNr = VareBokLinje.ArtikkelNr NO-ERROR.
        X = X + 1.
        DISPLAY
            X
            VarebokLinje.ArtikkelNr
            bufVarebokLinje.ArtikkelNr
            VareBehLinje.ArtikkelNr
            VareBokLinje.LevKod
            VareBokLinje.Beskr
            VareBokLinje.LEvFargKod
            VareBokLinje.LagerKode
            VareBokLinje.Kjedevare
            VareBokLinje.Gjennomfaktureres
            bufVareBokLinje.Kjedevare
            bufVareBokLinje.Gjennomfaktureres
            /*
            bufVareBokLinje.LevKod
            bufVareBokLinje.Beskr
            bufVareBokLinje.LEvFargKod
            */
        WITH WIDTH 320.
        
        ASSIGN
            VareBokLinje.Kjedevare    = TRUE
            bufVarebokLinje.Kjedevare = TRUE
            VareBehLinje.Kjedevare    = TRUE
            .
        
    END.

END.
