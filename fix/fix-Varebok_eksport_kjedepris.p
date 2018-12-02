OUTPUT TO VALUE('varebok_kjederab.csv').
FOR EACH VareBokLinje NO-LOCK WHERE
    VareBokLinje.VareBokNr = 9000013:
    FIND LAST Strekkode NO-LOCK where
        Strekkode.ArtikkelNr = VareBokLinje.ArtikkelNr NO-ERROR.

    IF AVAILABLE Strekkode THEN
    PUT UNFORMATTED
        Strekkode.Kode ';'
        VareBokLinje.InnkjopsPris - ((VareBokLinje.InnkjopsPris * 25) /  100) ';'
        Vareboklinje.InnkjopsPris - ((VareBokLinje.InnkjopsPris * 15) /  100)';'
        Vareboklinje.KjedeRab% ';'
        Vareboklinje.KjedeSupRab% ';'
        SKIP.

END.
