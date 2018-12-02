&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
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


ROUTINE-LEVEL ON ERROR UNDO,THROW.

DEF INPUT PARAM icMethod    AS CHAR NO-UNDO.
DEF INPUT PARAM iiInterval  AS INT  NO-UNDO.

DEF VAR timer1 AS System.Windows.Forms.Timer NO-UNDO.
DEF VAR bOk            AS LOG    NO-UNDO.
DEF VAR cMethod        AS CHAR   NO-UNDO.
DEF VAR hParent        AS HANDLE NO-UNDO.
DEF VAR bSuspend       AS LOG    NO-UNDO.
DEF VAR bFirstSuspend  AS LOG    NO-UNDO.
DEF VAR iInterval      AS INT    NO-UNDO.
DEF VAR hiTimer        AS INT    NO-UNDO.
DEF VAR hiSuccess      AS INT    NO-UNDO.
DEF VAR bNamedSuspend  AS LOG    NO-UNDO.
DEF VAR cTimerName     AS CHAR   NO-UNDO.
DEF VAR hFocus         AS HANDLE NO-UNDO.
DEF VAR hFocus2        AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-setInterval) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setInterval Procedure 
FUNCTION setInterval RETURNS LOGICAL
  ( INPUT iiInterval AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


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

timer1 = NEW System.Windows.Forms.Timer ().
timer1:Tick:Subscribe("timer1_Tick").
timer1:ENABLED = TRUE.

hParent = SOURCE-PROCEDURE.

cMethod = icMethod.

setInterval(iiInterval).

SUBSCRIBE TO "InvalidateHandle"  IN hParent.
SUBSCRIBE TO "SuspendJBoxTimer"  ANYWHERE.
SUBSCRIBE TO "SuspendNamedTimer" ANYWHERE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-InvalidateHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InvalidateHandle Procedure 
PROCEDURE InvalidateHandle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihProcHandle AS HANDLE NO-UNDO.

IF ihProcHandle = hParent THEN DO:
  DELETE OBJECT timer1 NO-ERROR.  
  DELETE PROCEDURE THIS-PROCEDURE.    
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setFocusHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setFocusHandle Procedure
PROCEDURE setFocusHandle:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihFocus AS HANDLE NO-UNDO.

hFocus = ihFocus.
timer1:ENABLED = YES.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-setTwoFocusHandles) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setTwoFocusHandles Procedure
PROCEDURE setTwoFocusHandles:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihFocus  AS HANDLE NO-UNDO.
DEF INPUT PARAM ihFocus2 AS HANDLE NO-UNDO.

ASSIGN hFocus  = ihFocus
       hFocus2 = ihFocus2
       .
timer1:ENABLED = YES.


END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-SuspendJBoxTimer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SuspendJBoxTimer Procedure 
PROCEDURE SuspendJBoxTimer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ibSuspend AS LOG NO-UNDO.

IF cTimerName NE "" THEN RETURN.

bSuspend = ibSuspend.

IF ibSuspend THEN DO:
  bFirstSuspend = YES.
  timer1:ENABLED = NO.
END. 
ELSE
  timer1:ENABLED = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SuspendNamedTimer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SuspendNamedTimer Procedure 
PROCEDURE SuspendNamedTimer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icTimerName AS CHAR NO-UNDO.
DEF INPUT PARAM ibSuspend   AS LOG  NO-UNDO.

IF icTimerName NE cTimerName THEN RETURN.

bNamedSuspend = ibSuspend.
IF ibSuspend THEN 
  timer1:ENABLED = NO.
ELSE
  timer1:ENABLED = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-timer1_Tick) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE timer1_Tick Procedure 
PROCEDURE timer1_Tick :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER sender as System.Object.
DEFINE INPUT PARAMETER  e as System.EventArgs.

IF bNamedSuspend THEN RETURN.

IF VALID-HANDLE(hFocus) THEN DO:
  IF hFocus:TYPE = "WINDOW" THEN
    hFocus:MOVE-TO-TOP().
  ELSE APPLY "ENTRY" TO hFocus.
  hFocus = ?.
  IF VALID-HANDLE(hFocus2) THEN DO:
    IF hFocus2:TYPE = "WINDOW" THEN
      hFocus2:MOVE-TO-TOP().
    ELSE APPLY "ENTRY" TO hFocus2.
    hFocus2 = ?.
  END.  
  timer1:ENABLED = NO.  
END.
ELSE IF NOT bSuspend THEN DO:
  IF cMethod NE "" THEN DO:
    RUN VALUE(cMethod) IN hParent NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
      MESSAGE "Procedure " cMethod " not defined in " hParent:FILE-NAME SKIP
              ERROR-STATUS:GET-MESSAGE(1)
              VIEW-AS ALERT-BOX ERROR.
  END.            
  RETURN.
END.
ELSE IF bFirstSuspend THEN DO:
  bFirstSuspend = NO.
  RUN setFocus IN hParent NO-ERROR.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-setInterval) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setInterval Procedure 
FUNCTION setInterval RETURNS LOGICAL
  ( INPUT iiInterval AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
timer1:INTERVAL = iiInterval.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

