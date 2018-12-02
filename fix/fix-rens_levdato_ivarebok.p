CURRENT-WINDOW:WIDTH = 200.


FOR EACH VareBokLinje EXCLUSIVE-LOCK WHERE
    VareBokLinje.VareBokNr = 9000040:

    
    /*
    IF 
        (VareBokLinje.LEvDato1 <> ? AND VareBokLinje.LevDato1 < 01/01/2011) OR
        (VareBokLinje.LEvDato2 <> ? AND VareBokLinje.LevDato2 < 01/01/2011) OR
        (VareBokLinje.LEvDato3 <> ? AND VareBokLinje.LevDato3 < 01/01/2011) OR
        (VareBokLinje.LEvDato4 <> ? AND VareBokLinje.LevDato4 < 01/01/2011) 
    THEN
    */ 
    DISPLAY
        VareBokLinje.ArtikkelNr
        VareBokLinje.Beskr
        VarebokLinje.LevDato1
        VareBokLinje.LEvDato2
        VareBokLinje.LEvDato3
        VareBokLinje.LEvDato4
        WITH WIDTH 200.
    /*
    IF (VareBokLinje.LEvDato2 <> ? AND VareBokLinje.LevDato2 < 01/01/2011) THEN VareBokLinje.LEvDato2 = ?.
    IF (VareBokLinje.LEvDato3 <> ? AND VareBokLinje.LevDato3 < 01/01/2011) THEN VareBokLinje.LEvDato3 = ?.
    IF (VareBokLinje.LEvDato4 <> ? AND VareBokLinje.LevDato4 < 01/01/2011) THEN VareBokLinje.LEvDato4 = ?.
    */
END.
