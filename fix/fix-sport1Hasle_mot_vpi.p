CURRENT-WINDOW:WIDTH = 250.
FOR EACH ArtBas WHERE ArtBas.LevNr = 0:
    FIND FIRST VPIArtBas NO-LOCK WHERE
        VPIArtBas.ArtikkelNR = ArtBas.ArtikkelNR NO-ERROR.
    IF AVAILABLE VPIArtBas THEN
        ArtBas.LevNr = VPIArtBas.LevNr. 
    ELSE 
    STREKKODEN:
    DO:
        FOR EACH strekkode OF ArtBas NO-LOCK:
            FIND FIRST VPIStrekkode NO-LOCK WHERE
                VPIStrekkode.Kode = Strekkode.Kode NO-ERROR.
            IF AVAILABLE VPIStrekkode THEN
            DO:
                FIND VPIArtBas OF VPIStrekkode NO-ERROR.
                IF AVAILABLE VPIArtBas THEN
                    ArtBas.LevNr = VPIArtBas.LevNr. 
                LEAVE STREKKODEN.
            END.
        END.
    END. /* STREKKODEN */

    /* VPI leverandør. */
    IF ArtBas.LevNr = 0 THEN
        ArtBas.LevNr = 999999.

/*
    DISPLAY
        ArtBas.LevNr
        ArtBas.ArtikkelNr
        ArtBas.Beskr
        VPIArtBas.LevNr WHEN AVAILABLE VPIArtBas
        WITH WIDTH 250.
*/    

END.
