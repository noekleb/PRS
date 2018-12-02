
/*------------------------------------------------------------------------
    File        : initPlukklistePRSCl.p
    Purpose     : 

    Syntax      :

    Description : Henter data fra PRS via AppServer.

    Author(s)   : 
    Created     : Sat Feb 24 11:54:30 CET 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{cls\bxLog\tmpTblBxPickinglist.i}
{cls\BxLog\tmpDsBxPickinglist.i}        

DEFINE INPUT  PARAMETER cLogg AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsBxPickinglist.

DEFINE VARIABLE hServer           AS HANDLE    NO-UNDO.
DEFINE VARIABLE cConnectionString AS CHARACTER NO-UNDO.
DEFINE VARIABLE obOk              AS LOG       NO-UNDO.
DEFINE VARIABLE cIpAdr            AS CHARACTER      NO-UNDO.

DEFINE VARIABLE iX     AS INTEGER  NO-UNDO.
DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.
DEFINE VARIABLE bConnected AS LOG NO-UNDO.

DEFINE VARIABLE cSystem AS CHARACTER NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.

rStandardFunksjoner:hentSystem( INPUT-OUTPUT cSystem ).

/* Kobler opp AppServer. */
bConnected = rStandardFunksjoner:oppkoblingAppServer( cLogg, OUTPUT hServer ).

rStandardFunksjoner:SkrivTilLogg(cLogg,
    '  initPlukklistePRSCl - Oppkoblet mot Appserver: ' + STRING(bConnected) + '.' 
    ).    

IF bConnected THEN 
DO: 
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  initPlukklistePRSCl AppServer oppkoblet.' 
        ).    

    RUN cls\Plukking\asinitPlukklistePRS.p ON SERVER hServer (cLogg, INPUT-OUTPUT DATASET dsBxPickinglist ) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
    DO:
        cTekst = ''. 
        DO ix = 1 TO ERROR-STATUS:NUM-MESSAGES:    
            cTekst = cTekst + 
                (IF cTekst <> '' THEN CHR(10) ELSE '') + 
                STRING(ERROR-STATUS:GET-NUMBER(ix)) + ' ' + ERROR-STATUS:GET-MESSAGE(ix).
        END.
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '  initPlukklistePRSCl - Run: ' + cTekst 
            ).    
    END.
    ELSE 
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '  initPlukklistePRSCl - Vellykket henting av data.' 
            ).
        
END.
ELSE obOk = FALSE.

/* Rydder opp */
bConnected = rStandardFunksjoner:nedkoblingAppServer( cLogg, INPUT-OUTPUT hServer ).

rStandardFunksjoner:SkrivTilLogg(cLogg,
    '  initPlukklistePRSCl - Nedkoblet Appserver: ' + STRING(bConnected) + '.' 
    ).    

/* Rydder opp */
IF VALID-HANDLE(hServer) THEN 
    DELETE OBJECT hServer. 

    CATCH e1 AS Progress.Lang.AppError:
    DO ix = 1 TO e1:NumMessages:
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '  ' + e1:GetMessage(ix) 
            ).    
    END.
    
    IF e1:ReturnValue > "" THEN
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '  Returverdi: ' + e1:ReturnValue 
            ).    
END CATCH.
CATCH e2 AS Progress.Lang.Error:
    DO ix = 1 TO e2:NumMessages:
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '  ' + e2:GetMessage(ix) 
            ).    
    END.
END CATCH.

