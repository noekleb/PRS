DEF VAR iTelleNr AS INT NO-UNDO.

ASSIGN
    iTelleNr = 322
    .
    
FOR EACH TelleLinje EXCLUSIVE-LOCK WHERE
    TelleLinje.TelleNr = iTelleNr:
    FIND ArtBas NO-LOCK WHERE
        ArtBas.ArtikkelNr = TelleLinje.ArtikkelNr.
    FIND FIRST ArtPris OF ArtBas NO-LOCK.

    ASSIGN
        TelleLinje.VVareKost = ArtPris.VareKost[1]
        TelleLinje.OpprVerdi = TelleLinje.VVareKost * TelleLinje.AntallPar
        TelleLinje.OpptVerdi = TelleLinje.VVareKost * TelleLinje.AntallTalt
        TelleLinje.VerdiDiff = TelleLinje.VVarekost * TelleLinje.AntallDiff
        .
END.
