DEF VAR lAntallSolgt LIKE Tellelinje.AntallPar NO-UNDO.

CURRENT-WINDOW:WIDTH = 350.

FOR EACH TelleLinje EXCLUSIVE-LOCK WHERE 
    Tellelinje.TelleNr = 28777 /*AND
    Tellelinje.ArtikkelNr = 9816057*/: 


    lAntallSolgt = 0.
    FOR EACH TransLogg NO-LOCK WHERE 
        TransLogg.ArtikkelNr = TelleLinje.ArtikkelNr AND
        TransLogg.Butik      = TelleLinje.butik AND
        TransLogg.Storl      = TelleLinje.Storl AND
        TransLogg.TTId       = 1 AND
        TransLogg.Dato      >= 02/01/2018 AND 
        Translogg.Dato      <= 02/05/2018:

        LAntallSolgt = lAntallSolgt + 1.

    END.

    /*
    IF lAntallSolgt <> 0 THEN 
    DISPLAY
        Tellelinje.TelleNr
        TelleLinje.LinjeNr
        TelleLinje.butik
        TelleLinje.ArtikkelNr
        TelleLinje.Storl
        TelleLinje.AntallPar
        TelleLinje.AntallTalt
        TelleLinje.AntallDiff
        lAntallSolgt
    WITH WIDTH 350.
    */
    
    ASSIGN 
        TelleLinje.AntallPar  = TelleLinje.AntallPar  + lAntallSolgt
        TelleLinje.AntallDiff = TelleLinje.AntallPar  - TelleLinje.AntallTalt
        TelleLinje.VerdiDiff  = TelleLinje.AntallDiff * TelleLinje.VVareKost
        TelleLinje.OpprVerdi  = TelleLinje.AntallPar  * TelleLinje.VVareKost
        .
    
END.
