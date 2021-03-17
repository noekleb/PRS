FOR EACH LAger WHERE
    Lager.VVareKost = 0:

    IF AVAILABLE ArtPris THEN
        RELEASE ArtPris.

    FIND Butiker NO-LOCK WHERE 
        Butiker.Butik = Lager.Butik NO-ERROR.
    IF AVAILABLE Butiker THEN
        FIND ArtPRis NO-LOCK WHERE
        ArtPris.ArtikkelNr = LAger.ArtikkelNr AND
        ArtPris.ProfilNR   = Butiker.ProfilNr NO-ERROR.

    IF AVAILABLE ArtPris THEN
        Lager.VVAreKost = ArtPris.VareKost[IF ArtPris.Tilbud THEN 2 ELSE 1].

END.
