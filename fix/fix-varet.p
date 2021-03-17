FIND LAST TelleHode WHERE TelleHode.TelleNr = 573 NO-LOCK.
FOR EACH  tellelinje OF TelleHode.
    ASSIGN TelleLinje.AntallTalt = 0
           TelleLinje.AntallDiff = TelleLinje.AntallPar
           TelleLinje.OpptVerdi  = 0
           TelleLinje.VerdiDiff  = TelleLinje.AntallDiff * TelleLinje.VVareKost.
END.
