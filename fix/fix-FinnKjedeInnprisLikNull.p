CURRENT-WINDOW:WIDTH = 350.

FOR EACH Lager NO-LOCK WHERE 
    Lager.Butik = 16 AND 
    Lager.LagAnt > 0:
    FIND ArtBas NO-LOCK WHERE 
        ArtBas.ArtikkelNr = Lager.ArtikkelNr.
        
    IF ArtBas.KjedeInnkPris = 0 THEN
    DISPLAY
        ArtBas.ArtikkelNr
        ArtBas.Beskr FORMAT "x(60)"
        ArtBas.LevKod
        ArtBas.LevFargKod
        Lager.Lagant
        Lager.VVareKost
        ArtBas.KjedeInnkPris
    WITH WIDTH 350.
        
END.
