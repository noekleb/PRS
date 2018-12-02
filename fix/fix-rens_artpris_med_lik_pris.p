CURRENT-WINDOW:WIDTH = 250.
DEF BUFFER bufArtPris FOR ArtPris.

FOR EACH ArtPris EXCLUSIVE-LOCK WHERE
    ArtPris.ProfilNr > 1:

    FIND bufArtPris NO-LOCK WHERE
         bufArtPris.ArtikkelNr = ArtPris.ArtikkelNr AND
         bufArtPris.ProfilNr   = 1 NO-ERROR.

    IF AVAILABLE bufArtPris THEN 
    DO:
        IF ArtPRis.Pris[1] = bufArtPris.Pris[1]             
            THEN
        DO:
            /*
            DISPLAY 
            ArtPris.ArtikkelNR
            ArtPris.ProfilNr
            ArtPRis.Pris[1]
            bufArtPris.Pris[1]
            '*' WHEN ArtPRis.Pris[1] = bufArtPris.Pris[1]
            bufartpris.ProfilNr
            WITH WIDTH 250.
            */

            DELETE ArtPris.
        END.
    END.
END.
