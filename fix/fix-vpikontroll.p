DEF VAR X AS INT NO-UNDO.

FOR EACH VPIArtBas WHERE VPIArtBas.ArtikkelNr = 0:

    FIND Strekkode NO-LOCK WHERE
        Strekkode.Kode = VPIArtBas.VAreNr NO-ERROR.
    IF AVAILABLE Strekkode THEN
    DO:
        ASSIGN
            VPIArtBas.ArtikkelNr = Strekkode.ArtikkelNr
            X = X + 1
            .
        /*
        DISPLAY 
            VPIArtBas.ArtikkelNr
            VPIArtBas.VareNr
            Strekkode.Kode WHEN AVAILABLE Strekkode
            Strekkode.ArtikkelNr WHEN AVAILABLE Strekkode
            .
            */
        IF x MODULO 100 = 0 THEN
        DO:
            PAUSE 0 .
            DISPLAY X WITH FRAME g.
        END.
    END.
END.
DISPLAY X.
