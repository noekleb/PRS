FOR EACH TelleLinje EXCLUSIVE-LOCK:
    FIND LAger NO-LOCK WHERE
        Lager.ArtikkelNr = TelleLinje.ArtikkelNR AND
        Lager.Butik      = 550 NO-ERROR.

    IF AVAILABLE Lager THEN
    ASSIGN
        TelleLinje.VVareKost  = IF (Lager.VVAreKost >= 0 AND Lager.VVAreKost <> ?) THEN Lager.VVareKost ELSE TelleLinje.VVareKost
        TelleLinje.OpprVerdi  = TelleLinje.AntallPar  * TelleLinje.VVareKost
        TelleLinje.OpptVerdi  = TelleLinje.AntallTalt * TelleLinje.VVareKost
        TelleLinje.VerdiDiff  = TelleLinje.AntallDiff * TelleLinje.VVareKost
        .
END.
