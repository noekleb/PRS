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

/* Skal ikke håndteres her. */
IF Ordre.Ordrestatus < 2 OR Ordre.OrdreStatus > 6 THEN
    RETURN.

RUN SjekkLevert.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-SjekkLevert) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SjekkLevert Procedure 
PROCEDURE SjekkLevert :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:    Husk at vi bare kommer inn her med ordre som har status 2 eller 6.
            Koden lengre nede forutsetter det. Kontroll gjøres i MainBlokk.   
------------------------------------------------------------------------------*/
DEF VAR piNumILev AS INT NO-UNDO.
DEF VAR piNumBest AS INT NO-UNDO.
DEF VAR piNumLev  AS INT NO-UNDO.
DEF VAR iStatus   AS INT NO-UNDO.

/* Ligger det ingen bestillinger på ordre, skal den ikke røres. */
IF NOT CAN-FIND(FIRST BestHode WHERE
    BestHode.OrdreNr = Ordre.OrdreNr) THEN
    RETURN.

BESTHODE:
FOR EACH BestHode NO-LOCK WHERE
    BestHode.OrdreNr = Ordre.OrdreNr:

    /* Antall ikke leverte og delhvis leverte */
    IF BestHode.BestStat < 6 THEN
        piNumILev = piNumILev + 1.
    /* Antall fulleverte */
    IF BestHode.BestStat = 6 THEN
        piNumLev = piNumLev + 1.
    /* Totalt antall */
    piNumBest = piNumBest + 1.
END. /* BESTHODE */

IF (piNumBest = piNumLev) THEN istatus = 6.
ELSE IF piNumILev > 0 THEN  istatus = 5.
ELSE IF piNumLev = 0 THEN istatus = 4.


IF Ordre.OrdreStatus <> iStatus THEN
DO:
    FIND CURRENT ordre EXCLUSIVE-LOCK.
    ASSIGN Ordre.OrdreStatus = iStatus.
    FIND CURRENT ordre NO-LOCK.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

