DEF VAR lArtikkelNr AS DEC FORMAT ">>>>>>>>>>>>>9" NO-UNDO.

DEF BUFFER bufArtBas FOR ArtBas.

CURRENT-WINDOW:WIDTH = 350.

FOR EACH ArtBas EXCLUSIVE-LOCK WHERE 
    /*ArtBas.ArtikkelNr = 9833897 AND*/
    ArtBas.HG = 90 AND 
    /*ArtBas.HovedModellFarge = TRUE:*/
    ArtBas.ModellFarge > 0:
    
    
    /*
    DISPLAY
        ArtBas.ArtikkelNr
        ArtBas.HG
        ArtBas.ModellFarge
        ArtBas.Beskr
        ArtBas.LevKod
        ArtBas.Farg FORMAT ">>>>>9"
        ArtBas.VmId
        ArtBas.HovedModellFarge
        ArtBas.RegistrertDato
        ArtBas.EDato
    WITH WIDTH 350.
    */
    
    RUN opplosModell( ArtBas.ArtikkelNr). 
    ASSIGN 
        ArtBas.HovedModellFarge = FALSE
        ArtBas.ModellFarge = 0
    .
END.
    
PROCEDURE opplosModell:    
    DEF INPUT PARAMETER plArtikkelNr AS DEC NO-UNDO.
    
    FOR EACH bufArtBas EXCLUSIVE-LOCK WHERE 
        bufArtBas.ModellFarge = plArtikkelNr:
        
        ASSIGN 
            bufArtBas.HovedModellFarge = FALSE
            bufArtBas.ModellFarge = 0
        .
    END.
END PROCEDURE.
    
