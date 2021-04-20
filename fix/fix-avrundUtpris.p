CURRENT-WINDOW:WIDTH = 350.
OUTPUT TO 'konv\avrundetOutlet17122020.txt'.
FOR EACH ArtPris EXCLUSIVE-LOCK WHERE 
    ArtPris.ProfilNr = 2 AND 
    ArtPris.Pris[1] <> ROUND(ArtPris.Pris[1],0):
    
    FIND ArtBas OF ArtPris NO-LOCK NO-ERROR.
    
    DISPLAY
    ArtPris.ProfilNr
    ArtPris.Pris[1]
    ArtBas.LevKod
    ArtBas.Sasong
    ArtBas.LevFargKod
    WITH WIDTH 350.
    
    ArtPris.Pris[1] = ROUND(ArtPris.Pris[1],0).
END.
