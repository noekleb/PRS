DEF BUFFER bufArtPris FOR ArtPris.

CURRENT-WINDOW:WIDTH = 350.
FOR EACH ArtBAs WHERE 
    ArtBas.ForhRab% > 10 /* AND 
    ArtBas.RegistrertDato >= 01/01/2020*/:
    
    
    DISPLAY
    ArtBas.RegistrertDato
    ArtBAs.ArtikkelNr
    ArtBas.Beskr
    ArtBas.LevKod
    ArtBas.LEvFargKod
    ArtBas.AnbefaltPris
    artBas.ForhRab%    
    WITH WIDTH 350.
    /*
    /* forhRab% skal alltid stå til 10%. */
    ASSIGN 
        ArtBas.forhRab% = 10
        .
    */
END.
