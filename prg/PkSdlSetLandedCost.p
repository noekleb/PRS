
/*------------------------------------------------------------------------
    File        : PkSdlSetLandedCost.p
    Purpose     : 

    Syntax      :

    Description : Påfører landed kost på pakkseddelhode.

    Author(s)   : Tom Nøkleby
    Created     : Fri Jul 20 11:29:43 CEST 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER lPkSdlId AS DECIMAL FORMAT ">>>>>>>>>>>>9" NO-UNDO.

DEFINE VARIABLE lLandedCost AS DECIMAL FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

ASSIGN 
    cLogg     = 'PkSdlSetLandedCost' + REPLACE(STRING(TODAY),'/','')
    .

FIND PkSdlHode NO-LOCK WHERE 
    PkSdlHode.PkSdlId = lPkSdlId NO-ERROR.
IF AVAILABLE PkSdlHode THEN
DO:
    IF NUM-ENTRIES(PkSdlHode.MeldingFraLEv,CHR(10)) >= 3 THEN 
    DO:
        ASSIGN 
            cTekst      = ''
            lLandedCost = 0
            .
        cTekst = ENTRY(3,PkSdlHode.MeldingFraLev,CHR(10)) NO-ERROR.
        IF cTekst <> '' THEN 
            lLandedCost = DEC(TRIM(ENTRY(2,cTekst,' '))) NO-ERROR.
        /*IF lLandedCost = 0 THEN*/ 

        RUN setLandedCost1.
        
    END.
    ELSE DO:
        ASSIGN 
            cTekst      = ''
            lLandedCost = 0
            .
        RUN setLandedCost2.
    END.
END.



/* **********************  Internal Procedures  *********************** */

PROCEDURE setLandedCost1:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lKjedeInnkPris AS DECIMAL NO-UNDO.
    
    DEFINE BUFFER bufArtBas FOR ArtBas.
    
    ASSIGN 
        lLandedCost = 0.

    DO TRANSACTION:
        FIND CURRENT PkSdlHode EXCLUSIVE-LOCK.
        FOR EACH PkSdlLinje OF PkSdlHode NO-LOCK,
            FIRST ArtBas NO-LOCK WHERE 
                ArtBas.ArtikkelNr = PkSdlLinje.ArtikkelNr:
            
            lKjedeInnkPris = ArtBas.KjedeInnkPris.    
            
            /* Henter LC fra en artikkel med annen sesongkode. */
            IF lKjedeInnkPris = 0 THEN 
            DO:
                BUFLOOP:
                FOR EACH bufArtBas NO-LOCK WHERE 
                    bufArtBas.LevNr = ArtBas.LevNr AND 
                    bufArtBas.LevKod = ArtBas.LevKod AND 
                    bufArtBas.LevFargKod = ArtBas.LevFargKod AND 
                    /*bufArtBas.RegistrertDato >= 01/01/2015 AND*/ 
                    bufArtBas.KjedeInnkPris > 0:
                    ASSIGN 
                        lKjedeInnkPris = bufArtBas.KjedeInnkPris.    
                END. /* BUFLOOP */
            END.         
                  
            IF lKjedeInnkPris > 0 THEN    
                ASSIGN 
                    lLandedCost = lLandedCost + (PkSdlLinje.Antall * lKjedeInnkPris)
                    .
            ELSE
                /* Logger endret ansatt dato. */
                RUN bibl_loggDbFri.p (cLogg, '    Mangler LC: ;' + 
                    STRING(ArtBas.ArtikkelNr) + ';' + 
                    STRING(ArtBas.LevKod) + ';' +
                    STRING(ArtBas.LEvFargKod) + ';' + 
                    STRING(ArtBas.Sasong) 
                    ). 




        END.
        IF lLandedCost = ? OR lLandedCost < 0 THEN 
            lLandedCost = 0.

        ASSIGN 
            ENTRY(2,cTekst,' ')                      = STRING(lLandedCost)
            ENTRY(3,PkSdlHode.MeldingFraLEv,CHR(10)) = cTekst
            .
        FIND CURRENT PkSdlHode NO-LOCK.
    END. /* TRANSACTION */


END PROCEDURE.

PROCEDURE setLandedCost2:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lKjedeInnkPris AS DECIMAL NO-UNDO.
    DEFINE VARIABLE iSasong AS INTEGER NO-UNDO.
    
    DEFINE BUFFER bufArtBas FOR ArtBas.
     
    ASSIGN 
        lLandedCost = 0.

    FIND CURRENT PkSdlHode EXCLUSIVE-LOCK.
    FOR EACH PkSdlLinje OF PkSdlHode NO-LOCK,
        FIRST ArtBas NO-LOCK WHERE 
        ArtBas.ArtikkelNr = PkSdlLinje.ArtikkelNr: 
            
            
        lKjedeInnkPris = ArtBas.KjedeInnkPris.    
            
        /* Henter LC fra en artikkel med annen sesongkode. */
        IF lKjedeInnkPris = 0 THEN 
        DO:
            BUFLOOP:
            FOR EACH bufArtBas NO-LOCK WHERE 
                bufArtBas.LevNr = ArtBas.LevNr AND 
                bufArtBas.LevKod = ArtBas.LevKod AND 
                bufArtBas.LevFargKod = ArtBas.LevFargKod AND 
                /*bufArtBas.RegistrertDato >= 01/01/2015 AND*/ 
                bufArtBas.KjedeInnkPris > 0:
                ASSIGN 
                    lKjedeInnkPris = bufArtBas.KjedeInnkPris.    
            END. /* BUFLOOP */
        END.         
        IF lKjedeInnkPris > 0 THEN    
            ASSIGN 
                lLandedCost = lLandedCost + (PkSdlLinje.Antall * lKjedeInnkPris)
                .
        ELSE
            /* Logger endret ansatt dato. */
            RUN bibl_loggDbFri.p (cLogg, '    Mangler LC: ;' + 
                STRING(ArtBas.ArtikkelNr) + ';' + 
                STRING(ArtBas.LevKod) + ';' +
                STRING(ArtBas.LEvFargKod) + ';' + 
                STRING(ArtBas.Sasong) 
                ). 
            
        ASSIGN 
            iSaSong     = ArtBas.Sasong
            .
    END.
    IF lLandedCost = ? OR lLandedCost < 0 THEN 
        lLandedCost = 0.
    ASSIGN 
        PkSdlHode.MeldingFraLev = 'Ordretype: 0' + CHR(10) + 
                                  'Sesongkode: ' + STRING(iSasong) + CHR(10) + 
                                  'LandedCost: ' + STRING(lLandedCost) + CHR(10) + 
                                  PkSdlHode.MeldingFraLEv
        .
    FIND CURRENT PkSdlHode NO-LOCK.

END PROCEDURE.

