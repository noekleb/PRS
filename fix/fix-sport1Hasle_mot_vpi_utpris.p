DEF VAR cFieldList AS CHAR NO-UNDO.
DEF VAR ocReturn   AS CHAR NO-UNDO.
DEF VAR obOk       AS CHAR NO-UNDO.

CURRENT-WINDOW:WIDTH = 250.

DEF BUFFER bufArtPris FOR ArtPris.

FOR EACH ArtPris WHERE Artpris.pris[1] = ?:
    FIND ArtBas OF ArtPris NO-LOCK.

    FIND FIRST VPIArtBas NO-LOCK WHERE
        VPIArtBas.ArtikkelNR = ArtPris.ArtikkelNr NO-ERROR.
    IF NOT AVAILABLE VPIArtBas THEN
    STREKKODEN:
    DO:
        FOR EACH Strekkode OF ArtBas NO-LOCK:
            FIND FIRST VPIStrekkode NO-LOCK WHERE
                VPIStrekkode.Kode = Strekkode.Kode NO-ERROR.
            IF AVAILABLE VPIStrekkode THEN
            DO:
                FIND VPIArtBas OF VPIStrekkode NO-ERROR.
                LEAVE STREKKODEN.
            END.
        END.
    END. /* STREKKODEN */

    IF AVAILABLE VPIArtBas THEN
    DO:
        DISPLAY 
            VPIArtBAs.EkstVPILEvNr
            VPIArtBas.ArtikkelNr
            'HURRA'
            WITH WIDTH 250.
        cFieldList = {tbchooseAll.i}.
        RUN artbas_new.p (STRING(VPIArtBas.EkstVPILevNr) + ';' 
                                 + cFieldList + ';' 
                                 + STRING(VPIArtBas.ArtikkelNr), 
                          ?, 
                          '', 
                          OUTPUT ocReturn, 
                          OUTPUT obOk).      
    END.

    FIND bufArtPris NO-LOCK WHERE
        RECID(bufArtPris) = RECID(ArtPris) NO-ERROR.

     if ArtPris.Pris[1] = ? then ArtPris.Pris[1] = 0.
    DISPLAY
        ArtBas.LevNr
        ArtBas.ArtikkelNr
        ArtBas.Beskr
        ArtPris.Pris[1]
        VPIArtBas.ArtikkelNr WHEN AVAILABLE VPIArtBas
        VPIArtBas.Beskr      WHEN AVAILABLE VPIArtBas
        bufArtPris.Pris[1] WHEN AVAILABLE bufArtPris
        WITH WIDTH 250.
END.
