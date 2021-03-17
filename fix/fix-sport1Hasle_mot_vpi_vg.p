CURRENT-WINDOW:WIDTH = 250.

DEF VAR iVg AS INT NO-UNDO.

FOR EACH ArtBas WHERE ArtBas.Vg = 0:
    FIND FIRST VPIArtBas NO-LOCK WHERE
        VPIArtBas.ArtikkelNR = ArtBas.ArtikkelNR NO-ERROR.
    IF AVAILABLE VPIArtBas THEN
        iVg = VPIArtBas.Vg. 
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
                    iVg = VPIArtBas.Vg. 
                LEAVE STREKKODEN.
            END.
        END.
    END. /* STREKKODEN */

    /* VPI leverandør. */
    IF ArtBas.Vg = 0 THEN
    DO:
        IF iVg > 0 THEN
            ASSIGN
            ArtBas.LopNr = ?
            ArtBas.Vg    = iVg.
        ELSE 
            ASSIGN
            ArtBas.LopNr = ?
            ArtBas.Vg    = 9618. /* Ukjent vare */
        RUN settlopnr.p (ArtBas.Vg,'F',OUTPUT ArtBas.LopNr).
    END.

    
    DISPLAY
        ArtBas.Vg
        ArtBas.ArtikkelNr
        ArtBas.Beskr
        VPIArtBas.LevNr WHEN AVAILABLE VPIArtBas
        WITH WIDTH 250.
    
END.
