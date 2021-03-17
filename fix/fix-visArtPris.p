current-window:WIDTH = 350.
FOR EACH ArtPris NO-LOCK WHERE 
    ArtPris.ArtikkelNr = 9860814:
    DISPLAY
        ArtPris.ArtikkelNr
        ArtPris.ProfilNr
        ArtPris.InnkjopsPris[1]
        ArtPris.Pris[1]
        '|'
        ArtPris.InnkjopsPris[2]
        ArtPris.Pris[2]
    WITH WIDTH 350.
END.
