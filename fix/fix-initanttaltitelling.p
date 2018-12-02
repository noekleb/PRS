FOR EACH TelleLinje:

    ASSIGN
        TelleLinje.AntallTalt = 200
        TelleLinje.AntallDiff = TelleLinje.AntallPar - TelleLinje.AntallTalt
        TelleLinje.VerdiDiff  = TelleLinje.AntallDiff * TelleLinje.VVareKost
        .
                                  
END.
