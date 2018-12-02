
/*------------------------------------------------------------------------
    File        : hentProduktDataPRSCl.p
    Purpose     : 

    Syntax      :

    Description : Henter data fra PRS via AppServer.

    Author(s)   : 
    Created     : Sat Feb 24 11:54:30 CET 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{cls\BxLog\tempTabeller.i}    
{cls\BxLog\tempDatasett.i}

DEFINE INPUT  PARAMETER cLogg AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER DATASET FOR dsBxProduct.
DEFINE OUTPUT PARAMETER DATASET FOR dsBxUnit.

DEFINE VARIABLE hServer    AS HANDLE  NO-UNDO.
DEFINE VARIABLE bConnected AS LOGICAL NO-UNDO.
DEFINE VARIABLE obOk       AS LOG     NO-UNDO.

DEFINE VARIABLE iX     AS INTEGER  NO-UNDO.
DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.

DEFINE VARIABLE cSystem AS CHARACTER NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.

rStandardFunksjoner:hentSystem( INPUT-OUTPUT cSystem ).

/* Kobler opp AppServer. */
bConnected = rStandardFunksjoner:oppkoblingAppServer( cLogg, OUTPUT hServer ).

IF bConnected THEN 
DO: 
    RUN cls\BxLog\asHentProduktDataPRS.p ON SERVER hServer (cLogg, OUTPUT DATASET dsBxProduct, OUTPUT DATASET dsBxUnit) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
    DO:
        cTekst = ''. 
        DO ix = 1 TO ERROR-STATUS:NUM-MESSAGES:    
            cTekst = cTekst + 
                (IF cTekst <> '' THEN CHR(10) ELSE '') + 
                STRING(ERROR-STATUS:GET-NUMBER(ix)) + ' ' + ERROR-STATUS:GET-MESSAGE(ix).
        END.
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            'hentProduktDataPRSCl - ** Feil ved Run av program via AppServer: ' + cTekst 
            ).    
    END.
    obOk = TRUE.
END.
ELSE obOk = FALSE.

/* Rydder opp */
bConnected = rStandardFunksjoner:nedkoblingAppServer( cLogg, INPUT-OUTPUT hServer ).

