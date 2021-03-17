/*
CONNECT -pf q:\db\sport1\clskotex.pf -ld Sp1Skotex.
CONNECT -pf q:\db\sport1\clvpi.pf -ld Sp1VPI. 
*/                           

DEF VAR cEkstVPILevNrlst AS CHAR NO-UNDO.

CURRENT-WINDOW:WIDTH = 250.

/* Kobler ut databasetrigger på VPI artbas (HK databasen). */
ON WRITE OF Sp1VPI.VPIArtBas OVERRIDE DO: END.

DEF BUFFER bufVareBokHode FOR SkoTex.VareBokHode.
DEF BUFFER bufArtBas      FOR Sp1SkoTex.ArtBas.

FIND Skotex.Varebokhode NO-LOCK WHERE
    SkoTex.Varebokhode.VarebokNr = 7000000004.
FOR EACH SkoTex.VarebokLinje OF SkoTex.VareBokHode NO-LOCK /*WHERE
    SkoTex.VareBokLinje.Beskr BEGINS 'birkebeiner'*/:

    FIND SkoTex.LevBas OF SkoTex.VareBokLinje.
    FIND FIRST Sp1SkoTex.EkstVPILev NO-LOCK WHERE
        Sp1SkoTex.EkstVPILev.KortNavn BEGINS 'XON' AND
        Sp1SkoTex.EkstVPILev.LevNr = SkoTex.VareBokLinje.LevNr NO-ERROR.

    FIND Sp1SkoTex.ArtBas WHERE
        Sp1SkoTex.ArtBas.ArtikkelNr = SkoTex.VareBokLinje.ArtikkelNr NO-ERROR.

    /* Hvis artikkelen ikke ligger i ArtBas i HK databasen, lister vi linjene opp på skjermen. */
    IF NOT AVAILABLE Sp1SkoTex.ArtBas THEN 
    DO:
        IF AVAILABLE Sp1SkoTex.EkstVPILev THEN
            FIND Sp1VPI.VPIArtBas EXCLUSIVE-LOCK WHERE
                 Sp1VPI.VPIArtBas.EkstVPILevNr = Sp1SkoTex.EkstVPILEv.EkstVPILevNr AND
                 Sp1VPI.VPIArtBas.VareNr       = string(SkoTex.VareBokLinje.ArtikkelNr) NO-ERROR.

        
        /* Markerer de XON Sport artiklene som skal inn i XON sport varebok med dagens dato som VPI dato. */
        IF AVAILABLE Sp1VPI.VPIArtBas THEN DO:

            /*Sp1VPI.VPIArtBas.VPIDato = TODAY.*/

            IF NOT CAN-DO(cEkstVPILevNrlst,STRING(Sp1VPI.VPIArtBas.EkstVPILevNr)) THEN
                cEkstVPILevNrlst = cEkstVPILevNrlst + (IF cEkstVPILevNrlst = "" THEN "" ELSE ",") +
                                   STRING(Sp1VPI.VPIArtBas.EkstVPILevNr).

            
        END.
        
        DISPLAY
            SkoTex.VareBokLinje.ArtikkelNr
            SkoTex.VareBokLinje.LevKod
            SkoTex.VareBokLinje.Beskr
            SkoTex.VareBokLinje.LevFargKod
            SkoTex.VareBokLinje.LevNr
            SkoTex.LevBAs.LevNamn
            /*'+' WHEN SkoTex.VareBokLinje.LevNr <> 1*/
         Sp1SkoTex.EkstVPILev.EkstVPILevNr WHEN AVAILABLE Sp1SkoTex.EkstVPILev
         Sp1SkoTex.EkstVPILev.KortNavn WHEN AVAILABLE Sp1SkoTex.EkstVPILev
         Sp1VPI.VPIArtBas.VareNr WHEN AVAILABLE Sp1VPI.VPIArtBas
            '*' WHEN AVAILABLE Sp1SkoTex.ArtBas
        WITH WIDTH 250.
    END.
END.


