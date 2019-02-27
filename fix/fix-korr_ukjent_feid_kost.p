CURRENT-WINDOW:WIDTH = 350.
FOR EACH Lager WHERE 
    Lager.VVarekost = ?:

    FIND butiker OF Lager NO-LOCK NO-ERROR.

    FIND ArtPris NO-LOCK WHERE
        ArtPris.ArtikkelNr = Lager.ArtikkelNr AND
        ArtPris.ProfilNr   = butiker.ProfilNr NO-ERROR.
    IF NOT AVAILABLE ArtPris OR
        ArtPris.VareKost[1] = ? THEN
        FIND FIRST ArtPris NO-LOCK WHERE
            ArtPris.ArtikkelNr = Lager.ArtikkelNr NO-ERROR.

    IF AVAILABLE ArtPris AND ArtPris.VareKost[1] <> ? THEN
        Lager.VVareKost = ArtPris.VareKost[1].
    ELSE IF Lager.VVareKost = ? THEN
        Lager.VVareKost = 0.
        

    DISPLAY
        Lager.buti
        Lager.ArtikkelNr
        Lager.VVareKost
        Lager.EDato
        ArtPris.VareKost[1] WHEN AVAILABLE ArtPris
    WITH WIDTH 350.
END.
