DEF BUFFER bufArtPris FOR ArtPris.

CURRENT-WINDOW:WIDTH = 350.
FOR EACH ArtPris WHERE ArtPris.VareKost[1] = ?:
    FIND ArtBas OF ArtPris NO-LOCK NO-ERROR.
    FIND FIRST bufArtPris NO-LOCK WHERE 
        bufArtPris.ArtikkelNr = ArtPris.ArtikkelNr AND
        bufArtPris.ProfilNr >= 0 AND
        bufArtPris.VareKost[1] < ? NO-ERROR.
    DISPLAY
        ArtPris.Artikkelnr
        ArtBas.Beskr
        ArtBas.RegistrertDato
        ArtPris.ProfilNr
        ArtPris.InnkjopsPris[1]
        ArtPris.Rab1%[1]
        ArtPris.Pris[1]
        bufArtPris.ProfilNr WHEN AVAILABLE bufArtPris
        bufArtPris.InnkjopsPris[1] WHEN AVAILABLE bufArtPris
        bufArtPris.Rab1%[1] WHEN AVAILABLE bufArtPris
        bufArtPris.Pris[1] WHEN AVAILABLE bufArtPris
    WITH WIDTH 350.
END.
