CURRENT-WINDOW:WIDTH = 350.
FOR EACH ArtBas EXCLUSIVE-LOCK, 
    FIRST ArtPris OF ArtBas WHERE 
        ArtPris.ProfilNr = 1:
    IF ArtPris.Pris[1] <> ArtBas.AnbefaltPris THEN
    DO:
        ArtBAs.AnbefaltPris = ArtPris.Pris[1].
        /*
        DISPLAY
        ArtBas.ArtikkelNr
        ArtBas.Beskr
        ArtBas.AnbefaltPris
        ArtPris.Pris[1]
        WITH WIDTH 350.
        */
    END.

END.
