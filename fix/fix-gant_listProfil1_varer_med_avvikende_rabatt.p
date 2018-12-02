
/*------------------------------------------------------------------------
    File        : fix-gant_listProfil1_varer_med_avvikende_rabatt.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Mon May 10 21:30:20 CEST 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE iAntUten AS INTEGER NO-UNDO.
DEFINE VARIABLE cUtFil AS CHARACTER NO-UNDO.
DEFINE VARIABLE bUtfor AS LOG NO-UNDO.

DEFINE BUFFER bufArtBas FOR ArtBas.
DEFINE BUFFER buf2ArtBas FOR ArtBas.

DEFINE STREAM Ut.

DEFINE TEMP-TABLE tmpDublett NO-UNDO 
    FIELD ArtikkelNr LIKE ArtBas.ArtikkelNr
    FIELD LevNr LIKE ArtBas.LevNr
    FIELD LevKod LIKE ArtBas.LevKod
    FIELD LevFargKod LIKE ArtBas.LevFargKod
    FIELD Sasong LIKE ArtBas.Sasong
    FIELD KjedeInnkPris LIKE ArtBas.KjedeInnkPris
    INDEX idxArtikkel ArtikkelNr.

/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


/* ***************************  Main Block  *************************** */
CURRENT-WINDOW:WIDTH = 350.

ASSIGN 
    cUtFil = 'konv\ArtMedAvvikRabaatt' + REPLACE(STRING(TODAY),'/','') + REPLACE(STRING(TIME,"HH:MM:SS"),':','') + '.csv'
    bUtfor = FALSE 
    .
    
RUN finnDubletter.

RETURN.

/* ************************  Function Implementations ***************** */

/* **********************  Internal Procedures  *********************** */

PROCEDURE finnDubletter:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE pbApen AS LOG NO-UNDO.
    DEFINE VARIABLE piAnt AS INTEGER NO-UNDO.
    DEFINE VARIABLE plLagAnt LIKE Lager.Lagant NO-UNDO.
    DEFINE VARIABLE plLagAnt16 LIKE Lager.Lagant NO-UNDO.

    FOR EACH ArtBas NO-LOCK WHERE 
        ArtBas.LevNr > 0 AND 
        ArtBas.LevKod >= '' AND 
        ArtBas.LevFargKod >= '' AND 
        ArtBas.SaSong >= 0,
        FIRST ArtPris OF ArtBas NO-LOCK WHERE 
            ArtPris.ProfilNr = 1 AND 
            ArtPris.Rab1%[1] > 10
        BREAK BY ArtBas.LevNr
        BY ArtBas.LevKod
        BY ArtBas.LevFargKod
        BY ArtBas.Sasong:

        piAnt = piant + 1.

        plLagAnt = 0.
        plLagAnt16 = 0.
        FOR EACH Lager OF ArtBas NO-LOCK:
            IF Lager.Lagant > 0 THEN 
                plLagAnt = plLagAnt + Lager.Lagant.
            IF Lager.butik = 16 AND Lager.Lagant > 0 THEN 
                plLagAnt16 = plLagAnt16 + Lager.Lagant.
        END.
        
        DO: 
            IF pbApen = FALSE THEN 
            DO:
                pbApen = TRUE.
                OUTPUT STREAM Ut TO VALUE(cUtFil).
                PUT STREAM Ut UNFORMATTED 
                    'ArtikkelNr;'
                    'Beskr;'
                    'LevNr;'
                    'LevKod;'
                    'LevFargKod;'
                    'Sasong;'
                    'InnkjPris;'
                    'Rabatt%;'
                    'Pris;'
                    'KjedeInnkPris;'
                    'TotalLagerBeh;'
                    'LagerBehBut16;'
                    'RegistrertDato;'
                    'SistEndretDato'
                    SKIP.
            END.          

            PUT STREAM Ut UNFORMATTED   
                ArtBas.ArtikkelNr ';'
                ArtBas.Beskr ';'
                artBas.LevNr ';'
                ArtBas.LevKod ';'
                ArtBas.LevFargKod ';'
                ArtBas.Sasong ';'
                ArtPris.InnkjopsPris[1] ';'
                ArtPris.Rab1%[1] ';'
                ArtPris.Pris[1] ';'
                ArtBas.KjedeInnkPris ';'
                plLagAnt ';'
                plLagAnt16 ';'
                ArtBas.RegistrertDato ';'
                ArtBas.EDato 
                SKIP.
            piAnt = 0.
        END. /* LAST-OF */    
    END.    
    IF pbApen THEN 
        OUTPUT STREAM Ut CLOSE.
END PROCEDURE.


