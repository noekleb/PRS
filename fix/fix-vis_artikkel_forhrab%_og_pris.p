DEF BUFFER bufArtPris FOR ArtPris.

CURRENT-WINDOW:WIDTH = 350.
FOR EACH ArtBAs WHERE 
    ArtBas.ForhRab% > 10 AND 
    ArtBas.RegistrertDato >= 01/01/2020:
    
    FIND ArtPris WHERE 
        ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND 
        ArtPris.ProfilNR = 1 NO-ERROR.
    FIND bufArtPris WHERE 
        bufArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND 
        bufArtPris.ProfilNR = 2 NO-ERROR.
    
    DISPLAY
    ArtBas.RegistrertDato
    ArtBAs.ArtikkelNr
    ArtBas.Beskr
    ArtBas.LevKod
    ArtBas.LEvFargKod
    ArtBas.AnbefaltPris
    artBas.ForhRab%    
    '|'
    ArtPris.ProfilNr WHEN AVAILABLE ArtPris
    ArtPris.Pris[1] WHEN AVAILABLE ArtPris
    ArtPris.Rab1%[1] WHEN AVAILABLE  ArtPris
    ArtPris.RegistrertDato WHEN AVAILABLE ArtPris
    '|'
    bufArtPris.ProfilNr WHEN AVAILABLE bufArtPris
    bufArtPris.Pris[1] WHEN AVAILABLE bufArtPris
    bufArtPris.Rab1%[1] WHEN AVAILABLE  bufArtPris
    bufArtPris.RegistrertDato WHEN AVAILABLE bufArtPris
    
    WITH WIDTH 350.
    
END.
