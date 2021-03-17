&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEF INPUT PARAMETER iOrdreNr AS INT NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

FIND Ordre NO-LOCK WHERE
    Ordre.OrdreNr = iOrdreNr NO-ERROR.
IF NOT AVAILABLE Ordre THEN
    RETURN.

RUN DoBekreftetStuff.

/* IF Ordre.BekreftetOrdre THEN      */
/*     RUN SjekkEtterUBekreftetBest. */
/* ELSE                              */
/*     RUN SjekkEtterBekreftetBest.  */
/*                                   */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-DoBekreftetStuff) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DoBekreftetStuff Procedure 
PROCEDURE DoBekreftetStuff :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR piNumBekr  AS INT NO-UNDO.
DEF VAR piNumBest AS INT NO-UNDO.
DEF VAR iOrdreStatus AS INT NO-UNDO.
/* Ligger det ingen bestillinger på ordre, skal den ikke røres. */
ON WRITE OF Ordre OVERRIDE
DO:    
END.

IF NOT CAN-FIND(FIRST BestHode WHERE
    BestHode.OrdreNr = Ordre.OrdreNr) THEN
    RETURN.
IF ordre.OrdreStatus < 2 OR ordre.OrdreStatus > 4 THEN
    RETURN.
/* Teller opp bestillinger og bekreftede bestillinger. */
BESTHODE:
FOR EACH BestHode NO-LOCK WHERE
    BestHode.OrdreNr        = Ordre.OrdreNr:

    IF BestHode.BekreftetOrdre THEN
        piNumBekr = piNumBekr + 1.

    piNumBest = piNumBest + 1.
END. /* BESTHODE */

IF (piNumBest = piNumBekr) THEN
   iOrdreStatus = 4.
ELSE IF piNumBekr > 0 THEN
   iOrdreStatus = 3.
ELSE
    iOrdreStatus = 2.
IF ordre.OrdreStatus <> iOrdreStatus THEN
DO:
    FIND CURRENT ordre EXCLUSIVE-LOCK.
    ASSIGN 
    Ordre.BekreftetOrdre = IF iOrdreStatus = 4 THEN TRUE ELSE FALSE
    Ordre.BekreftetDato  = IF iOrdreStatus = 4 THEN TODAY ELSE ?
    Ordre.BekreftetAv    = IF iOrdreStatus = 4 THEN USERID("SkoTex") ELSE ""
    Ordre.Ordrestatus    = iOrdreStatus.
    FIND CURRENT ordre NO-LOCK.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SjekkEtterBekreftetBest) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SjekkEtterBekreftetBest Procedure 
PROCEDURE SjekkEtterBekreftetBest :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR piNumBekr  AS INT NO-UNDO.
DEF VAR piNumBest AS INT NO-UNDO.
DEF VAR iOrdreStatus AS INT NO-UNDO.
/* Ligger det ingen bestillinger på ordre, skal den ikke røres. */
IF NOT CAN-FIND(FIRST BestHode WHERE
    BestHode.OrdreNr = Ordre.OrdreNr) THEN
    RETURN.

/* Teller opp bestillinger og bekreftede bestillinger. */
BESTHODE:
FOR EACH BestHode NO-LOCK WHERE
    BestHode.OrdreNr        = Ordre.OrdreNr:

    IF BestHode.BekreftetOrdre THEN
        piNumBekr = piNumBekr + 1.

    piNumBest = piNumBest + 1.
END. /* BESTHODE */

IF (piNumBest = piNumBekr) THEN
   iOrdreStatus = 4.
ELSE IF piNumBekr > 0 THEN
   iOrdreStatus = 3.
ELSE
    RETURN.
IF ordre.OrdreStatus <> iOrdreStatus THEN
DO:
    FIND CURRENT ordre EXCLUSIVE-LOCK.
    ASSIGN 
    Ordre.BekreftetOrdre = TRUE 
    Ordre.BekreftetDato  = TODAY 
    Ordre.BekreftetAv    = USERID("SkoTex")
    Ordre.Ordrestatus    = iOrdreStatus.
    FIND CURRENT ordre NO-LOCK.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SjekkEtterUBekreftetBest) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SjekkEtterUBekreftetBest Procedure 
PROCEDURE SjekkEtterUBekreftetBest :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER bOrdre FOR Ordre.

DO FOR bOrdre TRANSACTION:
    /* Leser de bestillinger som er ubekreftet og setter disse. */
    BESTHODE:
    FOR EACH BestHode EXCLUSIVE-LOCK WHERE
        BestHode.OrdreNr        = Ordre.OrdreNr:
        
        IF BestHode.BekreftetOrdre = TRUE THEN NEXT BESTHODE.

        /* Finnes det ubekreftet bestilling, skal ordre trekkes ned til ubekreftet. */
        FIND bOrdre WHERE 
            RECID(bOrdre) = RECID(Ordre) NO-ERROR.
        IF AVAILABLE bOrdre THEN
            ASSIGN 
            Ordre.BekreftetOrdre = FALSE
            Ordre.BekreftetDato  = ? 
            Ordre.BekreftetAv    = ""   
           .
        LEAVE BESTHODE.
    END. /* BESTHODE */

END. /* TRANSACTION */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

