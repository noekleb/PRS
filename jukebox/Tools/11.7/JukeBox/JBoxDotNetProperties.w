&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
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
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
{adeuib/uniwidg.i}              /* Universal widget definition              */

/* Parameters Definitions ---                                           */
DEF INPUT  PARAM ir_U          AS RECID NO-UNDO.
DEF OUTPUT PARAM ocControl     AS CHAR  NO-UNDO.
DEF OUTPUT PARAM ocControlType AS CHAR  NO-UNDO INIT "JBoxDevExEdit".
DEF OUTPUT PARAM ocCode        AS CHAR  NO-UNDO.

/* Local Variable Definitions ---                                       */

DEF VAR cFirstQry AS CHAR NO-UNDO.
DEF VAR cFirstFm  AS CHAR NO-UNDO.
DEF VAR cFirstTb  AS CHAR NO-UNDO.
DEF VAR iNumQry   AS INT  NO-UNDO.
DEF VAR iNumTb    AS INT  NO-UNDO.
DEF VAR iNumFm    AS INT  NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS edDesc edCode btnOk btnCancel 
&Scoped-Define DISPLAYED-OBJECTS edDesc edCode 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCancel 
     LABEL "Cancel" 
     SIZE 17 BY 1.14.

DEFINE BUTTON btnOk 
     LABEL "Ok" 
     SIZE 17 BY 1.14.

DEFINE VARIABLE edCode AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-VERTICAL
     SIZE 117.8 BY 7.95 NO-UNDO.

DEFINE VARIABLE edDesc AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 117.6 BY 4.1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     edDesc AT ROW 2.38 COL 3.2 NO-LABEL WIDGET-ID 16
     edCode AT ROW 7.48 COL 3.2 NO-LABEL WIDGET-ID 18
     btnOk AT ROW 15.76 COL 86.8 WIDGET-ID 6
     btnCancel AT ROW 15.76 COL 104 WIDGET-ID 8
     "Description:" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 1.67 COL 3.4 WIDGET-ID 20
     "Code:" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 6.71 COL 3.4 WIDGET-ID 22
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 121.4 BY 16.14 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Add .Net properties for field"
         HEIGHT             = 16.14
         WIDTH              = 121.4
         MAX-HEIGHT         = 23.1
         MAX-WIDTH          = 125.4
         VIRTUAL-HEIGHT     = 23.1
         VIRTUAL-WIDTH      = 125.4
         ALWAYS-ON-TOP      = yes
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
ASSIGN 
       edDesc:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Add .Net properties for field */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Add .Net properties for field */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel C-Win
ON CHOOSE OF btnCancel IN FRAME DEFAULT-FRAME /* Cancel */
DO:
  APPLY "close" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOk C-Win
ON CHOOSE OF btnOk IN FRAME DEFAULT-FRAME /* Ok */
DO:
  ocCode = edCode:SCREEN-VALUE.
  APPLY "close" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  RUN InitWindow.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  DISPLAY edDesc edCode 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE edDesc edCode btnOk btnCancel 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitWindow C-Win 
PROCEDURE InitWindow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cFm     AS CHAR NO-UNDO.
DEF VAR cTable  AS CHAR NO-UNDO.
DEF VAR cOBS    AS CHAR NO-UNDO.

FIND _U WHERE RECID(_U) = ir_U NO-ERROR.

IF AVAIL _U THEN DO WITH FRAME {&FRAME-NAME}:
  ASSIGN ocControl = "o" + _U._NAME + "_" + ocControlType
         cFm = DYNAMIC-FUNCTION("getFmObjectsInUse")
         cTable = DYNAMIC-FUNCTION("getFirstTableList")
         .
  IF NUM-ENTRIES(cFm) > 1 THEN
    MESSAGE "More than one fieldMap object detected: " + cFm SKIP
            "Please make sure that the references are correct"
            VIEW-AS ALERT-BOX INFORMATION.
  ELSE IF NUM-ENTRIES(cTable) > 1 THEN
    MESSAGE "More than one primary table detected: " + cTable SKIP
            "Please make sure that the references are correct"
            VIEW-AS ALERT-BOX INFORMATION.

  ASSIGN cFm = ENTRY(1,cFm)
         cTable = ENTRY(1,cTable).

  FIND _F WHERE RECID(_F) = _U._x-recid NO-ERROR.
  CASE _U._TYPE:
    WHEN "EDITOR" THEN
      ASSIGN edCode:SCREEN-VALUE =
                 CHR(10) + "  " + ocControl + " = NEW JBoxDevExEdit(THIS-PROCEDURE," + _U._NAME + ":HANDLE)."
               + CHR(10) + "  " + ocControl + ":RegisterWithJukeBox(YES). /* YES: Visible */"
               + CHR(10) + "  " + ocControl + ":CreateDisplayLink(" + (IF cFm NE "" THEN cFm ELSE "<fieldMap>") + ":BUFFER-HANDLE,'" + _U._NAME + "')."
               + CHR(10) + "  /* Blob context and format: */"
               + CHR(10) + "  " + ocControl + ":cDocLoadContext = '" + (IF cTable NE "" THEN cTable ELSE "<table>") + "." + _U._NAME + "'."
               + CHR(10) + "  " + ocControl + ":cDocLoadIdFields = '<primary key>'. /* <-NB */"
               + CHR(10) + "  " + ocControl + ":cDocLoadFormat = 'html'."
             ocControlType = "JBoxDevExEdit"
             edDesc:SCREEN-VALUE = "Add Rich Text Format capabilities to editor. Content is saved as blob in the JBoxDocument table and linked to the record via JBoxDocRel."
               + CHR(10) + "If more than one blob fields for a record the link context must be set to point to the specific field."
               + CHR(10) + "Content may be stored as html."
               + CHR(10) + "(Settings are written to InitializeComponents)"
             .
    WHEN "FILL-IN" THEN DO:
      IF _F._DATA-TYPE = "date" THEN
        ASSIGN edCode:SCREEN-VALUE =
                   CHR(10) + "  " + ocControl + " = NEW JBoxDevExDateEdit(THIS-PROCEDURE," + _U._NAME + ":HANDLE)."
                 + CHR(10) + "  " + ocControl + ":RegisterWithJukeBox(YES). /* YES: Visible */"
                 + CHR(10) + "  " + ocControl + ":CreateDisplayLink(" + (IF cFm NE "" THEN cFm ELSE "<fieldMap>") + ":BUFFER-HANDLE,'" + _U._NAME + "')."
               ocControlType = "JBoxDevExDateEdit"
               edDesc:SCREEN-VALUE = "Add calendar lookup for date field."
                 + CHR(10) + "(Settings are written to InitializeComponents)"
               .
      ELSE IF _F._DATA-TYPE = "integer" OR _F._DATA-TYPE = "decimal" THEN
          ASSIGN edCode:SCREEN-VALUE =
                     CHR(10) + "  " + ocControl + " = NEW JBoxDevExCalcEdit(THIS-PROCEDURE," + _U._NAME + ":HANDLE)."
                   + CHR(10) + "  " + ocControl + ":RegisterWithJukeBox(YES). /* YES: Visible */"
                   + CHR(10) + "  " + ocControl + ":CreateDisplayLink(" + (IF cFm NE "" THEN cFm ELSE "<fieldMap>") + ":BUFFER-HANDLE,'" + _U._NAME + "')."
                 ocControlType = "JBoxDevExCalcEdit"
                 edDesc:SCREEN-VALUE = "Add calculator lookup for numeric field. Note that the 4GL input field will not be visible. Use with caution if you have trigger code for the field"
                 + CHR(10) + "(Settings are written to InitializeComponents)"
                 .
      ELSE MESSAGE ".Net extension not avail able for " _U._TYPE
                   VIEW-AS ALERT-BOX.
    END.
  END CASE.

  SESSION:SET-WAIT-STATE("").
END.
ELSE MESSAGE "_U record not available"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

