
/*------------------------------------------------------------------------
    File        : asSjekkOverforing.p
    Purpose     : Forhindre at butikker overfører varer til butikker de ikke skal overføre varer til.

    Syntax      : run asSjekkOverforing.p (<FraBut>, <TilBut>, output obOk, output ocReturn).

    Description : Sjekker om en butikk har anledning til å overføre varer. Har den det, sjekkes også om den har rett til å overføre til valgt butikk.

    Author(s)   : Tom Nøkleby
    Created     : Fri Aug 11 12:43:51 CEST 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER iFrabut AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER iTilBut AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER obOk AS LOG NO-UNDO.
DEFINE OUTPUT PARAMETER ocReturn AS CHARACTER NO-UNDO.

DEFINE VARIABLE cIngenOverforing AS CHARACTER NO-UNDO.
DEFINE VARIABLE iValidering AS INTEGER NO-UNDO.
DEFINE VARIABLE cButLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE iParaNr AS INTEGER NO-UNDO.
DEFINE VARIABLE cNobutLst AS CHARACTER NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION getTekst RETURNS CHARACTER 
	( INPUT iParaNr AS INTEGER ) FORWARD.


/* ***************************  Main Block  *************************** */
{syspara.i 11 7 1 iValidering INT}
{syspar2.i 11 7 9 cIngenOverforing}

/* Validering ikke aktiv */
IF iValidering <> 1 THEN 
DO:
    ASSIGN 
        obOk     = TRUE 
        ocReturn = ''
        .
    
    RETURN.
END.

/* Sjekk Ingen overføring */
IF CAN-DO(cIngenOverforing,STRING(iTilBut)) THEN 
DO:
    ASSIGN 
        obOk     = FALSE 
        ocReturn = getTekst(9)
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
            cButLst   = SysPara.Parameter1
            cNobutLst = SysPara.Parameter2
            .    
        LEAVE LES_GRUPPER.
    END. /* LES_GRUPPER */
END.

/* Hvis butikken ikke ligger i noen grupper og validering er aktiv, skal overøfring ikke tillates. */
IF cButLst = '' THEN 
DO:
    ASSIGN 
        obOk     = FALSE 
        ocReturn = 'Butikk ikke oppsatt i gruppe.'
        .
    
    RETURN.
END.

/* Sjekker om butikken overfører til noen som ikke er tillatt. */
IF CAN-DO(cNoButLst,STRING(iTilBut)) THEN 
DO:
    ASSIGN 
        obOk     = FALSE 
        ocReturn = getTekst(iParaNr)
        .
    
    RETURN.
END.
/* Overforing tillatt */
ELSE IF CAN-DO(cButLst,STRING(iFraBut)) THEN 
DO:
    ASSIGN 
        obOk     = TRUE 
        ocReturn = ''
        .
    
    RETURN.
END.

/* Dead End */
DO:
    ASSIGN 
        obOk     = FALSE 
        ocReturn = 'Udefinert.'
        .
    
    RETURN.
END.

/* ************************  Function Implementations ***************** */


FUNCTION getTekst RETURNS CHARACTER 
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

