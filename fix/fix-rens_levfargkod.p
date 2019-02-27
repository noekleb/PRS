FOR EACH ArtBAs WHERE 
    NUM-ENTRIES(ArtBAs.LevFargKod,'/') > 1 AND
    TRIM(ENTRY(2,ArtBAs.LevFargKod,'/')) = '':

    ArtBas.LevFargKod = TRIM(ENTRY(1,ArtBAs.LevFargKod,'/')).
    DISPLAY
        Artbas.artikkelNr
        ArtBas.Beskr
        ArtBas.LevFargKod.
END.
