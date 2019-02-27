
/*------------------------------------------------------------------------
    File        : fix-gant_sett_LC_Modell.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Mon May 07 21:30:20 CEST 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE iAntUten AS INTEGER NO-UNDO.
DEFINE VARIABLE cUtFil AS CHARACTER NO-UNDO.
DEFINE VARIABLE bUtfor AS LOG NO-UNDO.

DEFINE BUFFER bufArtBas FOR ArtBas.
DEFINE BUFFER buf2ArtBas FOR ArtBas.

DEFINE STREAM Ut.

/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */
FUNCTION getAntUtent RETURNS INTEGER 
    (  ) FORWARD.


/* ***************************  Main Block  *************************** */
CURRENT-WINDOW:WIDTH = 350.

ASSIGN 
    cUtFil = 'konv\ArtIModellUtenLC' + REPLACE(STRING(TODAY),'/','') + REPLACE(STRING(TIME,"HH:MM:SS"),':','') + '.csv'
    bUtfor = TRUE 
    .
    
RUN visModell.

RETURN.

/* ************************  Function Implementations ***************** */

FUNCTION getAntUten RETURNS INTEGER 
    (  ):
/*------------------------------------------------------------------------------
 Purpose: Teller opp antall artikler i modellen hvor LC ikke er satt.
 Notes:
------------------------------------------------------------------------------*/    

    DEFINE VARIABLE piResult AS INTEGER NO-UNDO.

    FOR EACH bufArtBas NO-LOCK WHERE 
        bufArtBas.LevNr  = ArtBas.LevNr AND 
        bufArtBas.LevKod = ArtBas.LevKod AND 
        bufArtBas.LevFargKod = ArtBas.LevFargKod AND 
        bufArtBas.KjedeInnkPris = 0:
            
        piResult = piResult + 1.
        
        IF bUtfor THEN 
        DO TRANSACTION:
            FIND buf2ArtBas EXCLUSIVE-LOCK WHERE 
                RECID(buf2ArtBas) = RECID(bufArtBas) NO-ERROR.
            IF AVAILABLE buf2ArtBas THEN 
            DO:
                ASSIGN buf2ArtBas.KjedeInnkPris = ArtBas.KjedeInnkPris.
                RELEASE buf2ArtBas.
            END.
        END.
    END. 
    RETURN piResult.
        
END FUNCTION.

/* **********************  Internal Procedures  *********************** */

PROCEDURE visModell:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE pbApen AS LOG NO-UNDO.

    FOR EACH ArtBas NO-LOCK WHERE 
        ArtBas.LevNr > 0 AND 
        ArtBas.LevKod > '' AND 
        ArtBas.LevFargKod > '' AND 
        ArtBas.KjedeInnkPris > 0
        BREAK BY ArtBas.LevNr
        BY ArtBas.LevKod
        BY ArtBas.LevFargKod
        BY ArtBas.Sasong
        BY ArtBas.KjedeInnkPris:

        IF LAST-OF(ArtBas.Sasong) THEN
        DO: 
            iAntUten = getAntUten( ).
            IF iAntUten > 0 THEN          
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
                        'Antall uten LC'
                    SKIP.
                END.          
                PUT STREAM Ut UNFORMATTED   
                    ArtBas.ArtikkelNr ';'
                    ArtBas.Beskr ';'
                    artBas.LevNr ';'
                    ArtBas.LevKod ';'
                    ArtBas.LevFargKod ';'
                    ArtBas.Sasong ';'
                    ArtBas.KjedeInnkPris ';'
                    iAntUten
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
        END.    
    END.    
    IF pbApen THEN 
        OUTPUT STREAM Ut CLOSE.
END PROCEDURE.

