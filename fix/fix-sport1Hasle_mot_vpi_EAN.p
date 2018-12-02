DEF VAR cFieldList AS CHAR NO-UNDO.
DEF VAR ocReturn   AS CHAR NO-UNDO.
DEF VAR obOk       AS CHAR NO-UNDO.

CURRENT-WINDOW:WIDTH = 250.

DEF BUFFER bufArtPris FOR ArtPris.

DEF VAR cFilNAvn AS CHAR NO-UNDO.
DEF VAR cFilNAvnVPI AS CHAR NO-UNDO.
DEF VAR cFilNAvnMangler AS CHAR NO-UNDO.
DEF VAR cEAN     AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR cLinje AS CHAR NO-UNDO.

DEF STREAM Inn.
DEF STREAM Ut.
ASSIGN
    cFilNavn        = 'c:\home\lindbak\ankommet\HasleLagerverdiEtterTellin_Anders.csv'
    cFilNavnVPI     = 'c:\home\lindbak\ankommet\HasleLagerverdiEtterTellin_AndersVPI.csv'
    cFilNavnMangler = 'c:\home\lindbak\ankommet\HasleLagerverdiEtterTellin_AndersMangler.csv'
    .

INPUT STREAM Inn FROM VALUE(cFilNavn) NO-ECHO.
INNLES:
REPEAT:
    IMPORT STREAM Inn UNFORMATTED
        cLinje.
    cEAN = ENTRY(3,cLinje,';').
    TRIM(cEAN).

    run bibl_chkean.p (input-output cEAN).
    FIND Strekkode NO-LOCK WHERE
        Strekkode.Kode = cEAN NO-ERROR.
    FIND FIRST VPIStrekkode NO-LOCK WHERE
        VPISTrekkode.Kode = cEAN NO-ERROR.

    IF NOT AVAILABLE Strekkode THEN
    DO:
        IF AVAILABLE VPIStrekkode THEN
        DO:
            cFieldList = {tbchooseAll.i}.
            RUN artbas_new.p ('1;' 
                                + cFieldList + ';' 
                                + STRING(VPIStrekkode.VareNr), 
                              ?, 
                              '', 
                              OUTPUT ocReturn, 
                              OUTPUT obOk).      
            OUTPUT STREAM Ut TO VALUE(cFilNavnVPI) APPEND.
            PUT Stream Ut UNFORMATTED cLinje SKIP.
            OUTPUT STREAM Ut CLOSE.
        END.
        ELSE DO:
            OUTPUT STREAM Ut TO VALUE(cFilNavnMangler) APPEND.
            PUT STREAM Ut UNFORMATTED cLinje SKIP.
            OUTPUT STREAM Ut CLOSE.
        END.
        /*
        DISPLAY
            cEAN
            Strekkode.Kode WHEN AVAILABLE Strekkode
            VPIStrekkode.Kode WHEN AVAILABLE VPIStrekkode
            WITH WIDTH 250.
        */    
    END.

END. /* INNLES */
INPUT STREAM Inn CLOSE.




/*
FOR EACH ArtPris WHERE Artpris.Innkjopspris[1] = ?:
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
        
     if ArtPris.InnkjopsPris[1] = ? then ArtPris.InnkjopsPris[1] = 0.
    
    DISPLAY
        ArtBas.LevNr
        ArtBas.ArtikkelNr
        ArtBas.Beskr
        ArtPris.InnkjopsPris[1]
        VPIArtBas.ArtikkelNr WHEN AVAILABLE VPIArtBas
        VPIArtBas.Beskr      WHEN AVAILABLE VPIArtBas
        bufArtPris.InnkjopsPris[1] WHEN AVAILABLE bufArtPris
        WITH WIDTH 250.
    
END.
*/
