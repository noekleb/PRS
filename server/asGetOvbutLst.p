
/*------------------------------------------------------------------------
    File        : asGetOvbutLst.p
    Purpose     : Gi kassen en liste med butikker og butikknavn som den kan legge opp i en combo-box, hvor bruker kan velge butikk det skal overføres til.

    Syntax      : run asSjekkOverforing.p (<FraBut>, <TilBut>, output obOk, output ocReturn).

    Description : Tar frem en systemparameter som ineholder liste over de butikker en butikk kan over til.

    Author(s)   : Tom nøkleby
    Created     : Tue Aug 15 08:43:55 CEST 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT  PARAMETER iFrabut   AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER obOk      AS LOG NO-UNDO.
DEFINE OUTPUT PARAMETER cOvButLst AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER ocReturn  AS CHARACTER NO-UNDO.

DEFINE VARIABLE iValidering AS INTEGER NO-UNDO.
DEFINE VARIABLE cButLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE iParaNr AS INTEGER NO-UNDO.
DEFINE VARIABLE iButNr AS INTEGER NO-UNDO.
DEFINE VARIABLE iLoop AS INTEGER NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION GetTekst RETURNS CHARACTER 
	( INPUT iParaNr AS INTEGER ) FORWARD.


/* ***************************  Main Block  *************************** */
{syspara.i 11 7 1 iValidering INT}

/* Validering ikke aktiv */
IF iValidering <> 1 THEN 
DO:
    ASSIGN 
        obOk     = FALSE 
        ocReturn = getTekst(1)
        .
    
    RETURN.
END.

/* Leser grupper og finner butikklister */
LES_GRUPPER:
FOR EACH SysPara NO-LOCK WHERE 
    SysPara.SysHId = 11 AND 
    SysPara.SysGr  = 7:
    
    IF CAN-DO(SysPara.Parameter1,STRING(iFraBut)) THEN 
    DO:
        ASSIGN 
            iParaNr   = SysPara.ParaNr
            cButLst   = SysPara.Parameter2
            .    
        LEAVE LES_GRUPPER.
    END. /* LES_GRUPPER */
END.

/* Ugyldig fra butikk */
IF NOT CAN-FIND(Butiker WHERE 
                Butiker.Butik = iFrabut) THEN 
    DO:
        ASSIGN 
            obOk = FALSE 
            ocReturn = 'Ugyldig FRA butikk.'
            .
        RETURN.        
    END.
    
/* Sjekker at butikken er åpen. */
FIND Butiker NO-LOCK WHERE 
    Butiker.Butik = iFrabut NO-ERROR.
IF Butiker.ApningsDato = ? OR 
   Butiker.harButikksystem = FALSE OR 
   Butiker.NedlagtDato <> ? THEN 
   DO:
       IF Butiker.ApningsDato = ? THEN 
            ocReturn = 'FRA Butikken er ikke åpen.'.
       ELSE IF Butiker.harButikksystem = FALSE THEN
            ocReturn = 'FRA Butikken er ikke satt opp med butikksystem.'.
       ELSE IF Butiker.NedlagtDato <> ? THEN 
            ocReturn = 'FRA Butikken er nedlagt.'.
       ELSE ocReturn = ''.
       obOk = FALSE.
       RETURN. 
       
   END.    

/* Hvis butikken ikke ligger i noen grupper og validering er aktiv, skal overføring ikke tillates. */
IF cButLst = '' THEN 
DO:
    ASSIGN 
        obOk     = FALSE 
        ocReturn = 'Ingen butikker er satt opp for mottak av overføring fra denne butikken.'
        .
    
    RETURN.
END.

/* Prepp av butikkliste */
ELSE DO:
    cOvbutLst = ''.
    DO iLoop = 1 TO NUM-ENTRIES(cButLst):
        ASSIGN iButNr = INT(ENTRY(iLoop,cButLst)).
        FIND Butiker NO-LOCK WHERE 
            Butiker.Butik = iButNr NO-ERROR.
        IF AVAILABLE Butiker THEN 
        DO:
            /* Butikken kan ikke overføre til seg selv. */
            IF iButNr = iFrabut THEN 
                NEXT.
            
            ASSIGN 
                cOvbutLst = cOvbutLst + 
                            (IF cOvbutLst <> '' THEN ',' ELSE '') + 
                            STRING(Butiker.Butik) + ',' + Butiker.KortNavn.
        END.  
    END.
    IF cOvbutLst <> '' THEN 
        ASSIGN 
            obOk      = TRUE
            ocReturn  = getTekst(iParaNr)
            .
    ELSE 
        ASSIGN 
            obOk      = FALSE 
            ocReturn  = 'Det er ikke satt opp noen butikker å overføre til.'
            .        
END.

/* ************************  Function Implementations ***************** */


FUNCTION GetTekst RETURNS CHARACTER 
	( INPUT iParaNr AS INTEGER ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/	


    DEFINE VARIABLE result AS CHARACTER NO-UNDO.
    
    FIND SysPara NO-LOCK WHERE 
        SysPara.SysHId = 11 AND 
        SysPara.SysGr  =  7 AND 
        SysPara.ParaNr = iParaNr NO-ERROR.
    IF AVAILABLE SysPara THEN 
    DO:
        ASSIGN RESULT = SysPara.Hjelpetekst2.    
    END.
    ELSE DO:
        RESULT = ''.
    END.

    RETURN result.
END FUNCTION.

