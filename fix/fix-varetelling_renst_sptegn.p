CURRENT-WINDOW:WIDTH = 300.
FOR EACH TelleLinje WHERE
    TelleLinje.TelleNr = 11 AND
    TelleLinje.OpprVerdi = ?:

    ASSIGN
        TelleLinje.OpprVerdi = AntallPar * VVAreKost
        TelleLinje.OpptVerdi = AntallTalt * VVareKost
        TelleLinje.VerdiDiff = OpprVerdi - OpptVerdi
        .

    DISPLAY 
        TelleLinje.ArtikkelNr
        TelleLinje.Beskr
        TelleLinje.VVareKost
    TelleLinje.OpprVerdi
        TelleLinje.OpptVerdi
        TelleLinje.VerdiDiff
    WITH WIDTH 300.


END.
