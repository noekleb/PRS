
/*------------------------------------------------------------------------
    File        : fix-gant_sett_LC_Modell.p
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
DEF VAR cLagAntLst AS CHAR NO-UNDO.
DEF VAR cLagButLst AS CHAR NO-UNDO.

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
    cUtFil = 'konv\ArtDubletter' + REPLACE(STRING(TODAY),'/','') + REPLACE(STRING(TIME,"HH:MM:SS"),':','') + '.csv'
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

    FOR EACH ArtBas NO-LOCK WHERE 
        ArtBas.LevNr > 0 AND 
        ArtBas.LevKod > '' AND 
        ArtBas.LevFargKod > ''
        BREAK BY ArtBas.LevNr
        BY ArtBas.LevKod
        BY ArtBas.LevFargKod
        BY ArtBas.Sasong
        BY ArtBas.KjedeInnkPris:

        piAnt = piant + 1.
        
        IF LAST-OF(ArtBas.LevFargKod) THEN
        DO: 
            IF piAnt > 1 THEN
            EKSPORT:          
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
                        'KjedeInnkPris;'
                        'TotalLagerBeh'
                        'RegistrertDato;'
                        'SistEndretDato;'
                        'Antall;'
                        'But. med lager;'
                        'Lager ant. pr. butikk'
                    SKIP.
                END.          
                
                FOR EACH bufArtBas NO-LOCK WHERE 
                    bufArtBas.LevNr = ArtBas.LevNr AND 
                    bufArtBas.LevKod = ArtBas.LevKod AND 
                    bufArtBas.LevFargKod = ArtBas.LevFargKod 
                    BREAK BY bufArtBas.LevNr
                    BY bufArtBas.LevKod
                    BY bufArtBas.LevFargKod
                    BY bufArtBas.Sasong:
                
                    ASSIGN 
                        plLagAnt = 0
                        cLagantLst = ''
                        cLagButLst = ''
                        .
                    FOR EACH Lager OF bufArtBas NO-LOCK:
                        IF Lager.Lagant > 0 AND Lager.butik < 99 THEN 
                        DO:
                            ASSIGN 
                                plLagAnt = plLagAnt + Lager.Lagant
                                cLagantLst = cLagantLst + (IF cLagAntLst = '' THEN '' ELSE '|') + STRING(Lager.LagAnt)
                                cLagButLst = cLagButLst + (IF cLagButLst = '' THEN '' ELSE '|') + STRING(Lager.Butik)
                                .
                        END.
                    END.
                    
                    PUT STREAM Ut UNFORMATTED   
                        bufArtBas.ArtikkelNr ';'
                        bufArtBas.Beskr ';'
                        bufartBas.LevNr ';'
                        bufArtBas.LevKod ';'
                        bufArtBas.LevFargKod ';'
                        bufArtBas.Sasong ';'
                        bufArtBas.KjedeInnkPris ';'
                        plLagAnt ';'
                        bufArtBas.RegistrertDato ';'
                        bufArtBas.EDato ';'
                        piAnt ';'
                        cLagButLst ';'
                        cLagAntLst
                    SKIP.
/*                DISPLAY                 */
/*                    ArtBas.ArtikkelNr   */
/*                    ArtBas.Beskr        */
/*                    artBas.LevNr        */
/*                    ArtBas.LevKod       */
/*                    ArtBas.LevFargKod   */
/*                    ArtBas.Sasong       */
/*                    ArtBas.KjedeInnkPris*/
/*                    iAntUten            */
/*                WITH WIDTH 350.         */
                END.
            END. /* EKSPORT */
            piAnt = 0.
        END. /* LASST-OF */    
    END.    
    IF pbApen THEN 
        OUTPUT STREAM Ut CLOSE.
END PROCEDURE.


