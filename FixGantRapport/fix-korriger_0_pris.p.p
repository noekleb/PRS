CURRENT-WINDOW:WIDTH = 350.
DEF BUFFER bufArtPris FOR ArtPris.

ON write OF artbas OVERRIDE DO: END.

FOR EACH ArtPris WHERE ArtPris.ProfilNr = 1 AND ArtPris.InnkjopsPris[1] = 0:
    FIND bufArtPris NO-LOCK WHERE 
        bufArtPris.ArtikkelNr = ArtPris.ArtikkelNr AND
        bufArtPris.ProfilNr   = 16 NO-ERROR.
    FIND ArtBas OF ArtPris NO-LOCK NO-ERROR.
    IF AVAILABLE bufArtPris AND bufArtPris.InnkjopsPris[1] <> ? AND 
        bufArtPris.InnkjopsPris[1] > 0 THEN
    DO:
        
        DISPLAY
            ArtPris.ProfilNr
            ArtPris.ArtikkelNr
            ArtPris.RegistrertDato
            ArtBas.Beskr
            ArtBas.LevKod
            ArtBas.LevFargKod

            '|'
            ArtPris.InnkjopsPris[1]
            ArtPris.Pris[1]
            '|'
            bufArtPris.ProfilNr WHEN AVAILABLE bufArtPris
            bufArtPris.InnkjopsPris[1] WHEN AVAILABLE bufArtPris
            bufArtPris.Pris[1] WHEN AVAILABLE bufArtPris
        WITH WIDTH 350.
        
        BUFFER-COPY bufArtPris 
            EXCEPT ProfilNr
            TO ArtPris
            .

    END.
END.
