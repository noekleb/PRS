&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEF INPUT  PARAM icMenuList    AS CHAR NO-UNDO.
DEF INPUT  PARAM icSelSeq      AS CHAR NO-UNDO.
DEF INPUT  PARAM icSelParent   AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocLabel       AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocParent      AS CHAR NO-UNDO.
DEF OUTPUT PARAM oiInsertAfter AS INT NO-UNDO.

/* Local Variable Definitions ---                                       */

DEF VAR hParent AS HANDLE NO-UNDO.
DEF VAR iEntry  AS INT NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fiLabel cmbParent cmbInsertAfter Btn_Cancel ~
Btn_OK 
&Scoped-Define DISPLAYED-OBJECTS fiLabel cmbParent cmbInsertAfter 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ValidateImage Dialog-Frame 
FUNCTION ValidateImage RETURNS LOGICAL
  ( INPUT icFileName    AS CHAR,
    INPUT ihTargetField AS HANDLE)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE cmbInsertAfter AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Insert after" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1",0
     DROP-DOWN-LIST
     SIZE 34.4 BY 1 NO-UNDO.

DEFINE VARIABLE cmbParent AS CHARACTER FORMAT "X(256)":U 
     LABEL "Parent menu" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 34 BY 1 NO-UNDO.

DEFINE VARIABLE fiLabel AS CHARACTER FORMAT "X(256)":U 
     LABEL "Menu" 
     VIEW-AS FILL-IN 
     SIZE 34 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     fiLabel AT ROW 2.1 COL 14.8 COLON-ALIGNED
     cmbParent AT ROW 3.33 COL 14.8 COLON-ALIGNED WIDGET-ID 10
     cmbInsertAfter AT ROW 4.52 COL 14.6 COLON-ALIGNED WIDGET-ID 12
     Btn_Cancel AT ROW 6.19 COL 31
     Btn_OK AT ROW 6.19 COL 46.6
     SPACE(1.59) SKIP(0.18)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Add new sub-menu"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Add new sub-menu */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  DEF VAR bOk AS LOG NO-UNDO.


  RUN adecomm/_valpnam.p (fiLabel:SCREEN-VALUE,
                         YES,"_EXTERNAL",OUTPUT bOk).
  IF NOT bOk THEN RETURN NO-APPLY.

  ASSIGN ocLabel        = fiLabel:SCREEN-VALUE
         ocParent       = cmbParent:SCREEN-VALUE
         oiInsertAfter  = INT(cmbInsertAfter:SCREEN-VALUE)
         .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbParent
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbParent Dialog-Frame
ON VALUE-CHANGED OF cmbParent IN FRAME Dialog-Frame /* Parent menu */
DO:
  cmbInsertAfter:LIST-ITEM-PAIRS = RIGHT-TRIM("<first>,0," + DYNAMIC-FUNCTION("getMenuNodes" IN hParent,SELF:SCREEN-VALUE),",").
  iEntry = NUM-ENTRIES(cmbInsertAfter:LIST-ITEM-PAIRS) / 2.
  cmbInsertAfter:SCREEN-VALUE = cmbInsertAfter:ENTRY(iEntry).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  hParent = SOURCE-PROCEDURE.

  IF icMenuList = "" THEN 
    ASSIGN cmbParent:HIDDEN = YES
           cmbInsertAfter:Y = cmbParent:Y
           cmbInsertAfter:SIDE-LABEL-HANDLE:Y = cmbParent:SIDE-LABEL-HANDLE:Y
           cmbInsertAfter:LIST-ITEM-PAIRS = RIGHT-TRIM("<first>,0," + DYNAMIC-FUNCTION("getMenuNodes" IN hParent,""),",")
           iEntry = NUM-ENTRIES(cmbInsertAfter:LIST-ITEM-PAIRS) / 2
           cmbInsertAfter:SCREEN-VALUE = cmbInsertAfter:ENTRY(iEntry)
           .
  ELSE DO:
    cmbParent:LIST-ITEMS = icMenuList.
    IF icSelParent NE "" THEN
      cmbParent:SCREEN-VALUE = icSelParent.
    ELSE
      cmbParent:SCREEN-VALUE = cmbParent:ENTRY(1).
    APPLY "value-changed" TO cmbParent.

    IF icSelSeq NE "" THEN
      cmbInsertAfter:SCREEN-VALUE = icSelSeq NO-ERROR.
  END.

  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY fiLabel cmbParent cmbInsertAfter 
      WITH FRAME Dialog-Frame.
  ENABLE fiLabel cmbParent cmbInsertAfter Btn_Cancel Btn_OK 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ValidateImage Dialog-Frame 
FUNCTION ValidateImage RETURNS LOGICAL
  ( INPUT icFileName    AS CHAR,
    INPUT ihTargetField AS HANDLE) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR ix AS INT NO-UNDO.

IF icFileName = "" THEN RETURN NO.

IF icFileName MATCHES "*bmp" OR icFileName MATCHES "*ico" OR icFileName MATCHES "*.png"  OR icFileName MATCHES "*.gif"
   THEN DO WITH FRAME {&FRAME-NAME}:
  ihTargetField:SCREEN-VALUE = ENTRY(NUM-ENTRIES(icFileName,"\"),icFileName,"\").
  IF SEARCH(ihTargetField:SCREEN-VALUE) = ? THEN DO ix = 1 TO 4:
    ihTargetField:SCREEN-VALUE = ENTRY(NUM-ENTRIES(icFileName,"\") - ix,icFileName,"\") + "\" + ihTargetField:SCREEN-VALUE.
    IF SEARCH(ihTargetField:SCREEN-VALUE) NE ? THEN LEAVE.
  END.
  IF SEARCH(ihTargetField:SCREEN-VALUE) = ? THEN DO:
    MESSAGE "Invalid image: " icFileName SKIP "(must be in PROPATH)"
             VIEW-AS ALERT-BOX ERROR.
    ihTargetField:SCREEN-VALUE = "".
    RETURN NO.
  END.    
  RETURN YES.
END.
ELSE 
  MESSAGE "Invalid image file type: Must be bmp, gif or ico"
          VIEW-AS ALERT-BOX ERROR.
  
RETURN NO.
  
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

