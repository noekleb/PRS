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

DEF TEMP-TABLE ttIdleProcs
    FIELD cCheckProc        AS CHAR
    FIELD cCheckProcTarget  AS CHAR /* Client, Persistent or Server */
    FIELD cCheckProcParam   AS CHAR
    FIELD hCheckProc        AS HANDLE /* Set when persistent */
    FIELD cCheckMethod      AS CHAR
    FIELD cManageProc       AS CHAR
    FIELD hManageProc       AS HANDLE
    FIELD cManageMethod     AS CHAR
    .

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
   Other Settings: CODE-ONLY
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

ON 'close':U OF THIS-PROCEDURE DO:
  FOR EACH ttIdleProcs
      WHERE ttIdleProcs.cCheckProcTarget = "persistent":
    IF VALID-HANDLE(ttIdleProcs.hCheckProc) THEN
      APPLY "close" TO ttIdleProcs.hCheckProc.
    IF VALID-HANDLE(ttIdleProcs.hCheckProc) THEN
      DELETE PROCEDURE ttIdleProcs.hCheckProc NO-ERROR.
  END.
END.
/*
CREATE ttIdleProcs.
ASSIGN ttIdleProcs.cCheckProc       = icCheckProc
       ttIdleProcs.cCheckProcTarget = icCheckProcTarget
       ttIdleProcs.cCheckProcParam  = icCheckProcParam
       ttIdleProcs.cCheckMethod     = icCheckMethod
       ttIdleProcs.cManageProc      = icManageProc
       ttIdleProcs.cManageMethod    = icManageMethod
       .

IF icCheckProcTarget BEGINS "persist" THEN DO:
  RUN VALUE(icCheckProc) PERSISTENT SET ttIdleProcs.hCheckProc.
  IF CAN-DO(ttIdleProcs.hCheckProc:INTERNAL-ENTRIES,"InitializeObject") THEN
    RUN InitializeObject IN ttIdleProcs.hCheckProc.
END.
*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-SetIdleProc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetIdleProc Procedure 
PROCEDURE SetIdleProc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icCheckProc       AS CHAR NO-UNDO.
DEF INPUT PARAM icCheckProcTarget AS CHAR NO-UNDO.
DEF INPUT PARAM icCheckProcParam  AS CHAR NO-UNDO.
DEF INPUT PARAM icCheckMethod     AS CHAR NO-UNDO.
DEF INPUT PARAM icManageProc      AS CHAR NO-UNDO.
DEF INPUT PARAM icManageMethod    AS CHAR NO-UNDO.

CREATE ttIdleProcs.
ASSIGN ttIdleProcs.cCheckProc       = icCheckProc
       ttIdleProcs.cCheckProcTarget = icCheckProcTarget
       ttIdleProcs.cCheckProcParam  = icCheckProcParam
       ttIdleProcs.cCheckMethod     = icCheckMethod
       ttIdleProcs.cManageProc      = icManageProc
       ttIdleProcs.cManageMethod    = icManageMethod
       .

IF icCheckProcTarget BEGINS "persist" THEN DO:
  RUN VALUE(icCheckProc) PERSISTENT SET ttIdleProcs.hCheckProc.
  IF CAN-DO(ttIdleProcs.hCheckProc:INTERNAL-ENTRIES,"InitializeObject") THEN
    RUN InitializeObject IN ttIdleProcs.hCheckProc.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-StartProcedures) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartProcedures Procedure 
PROCEDURE StartProcedures :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FOR EACH ttIdleProcs:
  IF VALID-HANDLE(ttIdleProcs.hCheckProc) THEN DO:
    RUN VALUE(ttIdleProcs.cCheckMethod) IN ttIdleProcs.hCheckProc NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
      DYNAMIC-FUNCTION(ttIdleProcs.cCheckMethod IN ttIdleProcs.hCheckProc) NO-ERROR.
  END.

END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

