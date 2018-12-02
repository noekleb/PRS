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
&IF "{1}" = "Developer_Studio_is_Running" &THEN
  &SCOPED-DEFINE UIB_is_Running 1 
&ENDIF   
/*&SCOPED-DEFINE AdvGuiWin*/

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnSelectValues 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnSelectValues 
     LABEL "Select values from temp-table" 
     SIZE 62 BY 2.38.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnSelectValues AT ROW 3.29 COL 12
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 82.2 BY 6.81.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert window title>"
         HEIGHT             = 6.76
         WIDTH              = 82.8
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 96.4
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 96.4
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{incl/devmode.i}
{incl/custdevmode.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 6.81
       FRAME DEFAULT-FRAME:WIDTH            = 82.2.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <insert window title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSelectValues
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSelectValues C-Win
ON CHOOSE OF btnSelectValues IN FRAME DEFAULT-FRAME /* Select values from temp-table */
DO:  
  DEF VAR cRowIdList  AS CHAR NO-UNDO.
  DEF VAR cIdList     AS CHAR NO-UNDO.
  DEF VAR bOk         AS LOG  NO-UNDO.

  RUN JBoxSelector.w (THIS-PROCEDURE,0,
/*   RUN JBoxDSelector.w (THIS-PROCEDURE,0, */
                      "temp-table"                               
                      + ";!dummy|character|x"           /* <- to avoid initial sort invoke a dummy non-visual field */
                      + ";ValueField|INTEGER|>>9||Valuelabel"
                      + ";Description|CHARACTER|x(30)"
                      ,"where false",
                      INPUT-OUTPUT cRowIdList,
                      "ValueField",
                      INPUT-OUTPUT cIdList,
                      "","",
                      OUTPUT bOK).

  IF bOk THEN
    MESSAGE PROGRAM-NAME(1) SKIP
            cIdList SKIP
            cRowIdList
            VIEW-AS ALERT-BOX.
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
  DYNAMIC-FUNCTION("NewObject",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE,"container").
  DYNAMIC-FUNCTION("setAttribute",THIS-PROCEDURE:CURRENT-WINDOW,"keepInitialSize","yes").
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
  ENABLE btnSelectValues 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSelectorAttributes C-Win 
PROCEDURE setSelectorAttributes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihSourceBrw   AS HANDLE NO-UNDO.
DEF INPUT PARAM ihTargetBrw   AS HANDLE NO-UNDO.

DEF VAR hSourceBuffer  AS HANDLE NO-UNDO.
DEF VAR hTargetBuffer  AS HANDLE NO-UNDO.
DEF VAR ix             AS INT    NO-UNDO.
DEF VAR cSelectedRows  AS CHAR   NO-UNDO.

hSourceBuffer = ihSourceBrw:QUERY:GET-BUFFER-HANDLE(1).
hTargetBuffer = ihTargetBrw:QUERY:GET-BUFFER-HANDLE(1).

DYNAMIC-FUNCTION("setAttribute",ihSourceBrw,"basequery","where true").
DYNAMIC-FUNCTION("setAttribute",ihSourceBrw,"uselocaldata","yes").
DYNAMIC-FUNCTION("setAttribute",ihTargetBrw,"uselocaldata","yes").

/* Fill temp-table: */
DO ix = 1 TO 10:
  hSourceBuffer:BUFFER-CREATE().
  ASSIGN hSourceBuffer:BUFFER-FIELD("ValueField"):BUFFER-VALUE = ix
         hSourceBuffer:BUFFER-FIELD("Description"):BUFFER-VALUE = CHR(64 + ix)
         hSourceBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE = "rowid" + STRING(ix)
         .

  /* If any records should be pre-selected: */
  IF ix > 6 THEN DO:
    hTargetBuffer:BUFFER-CREATE().
    ASSIGN hTargetBuffer:BUFFER-FIELD("ValueField"):BUFFER-VALUE = ix
           hTargetBuffer:BUFFER-FIELD("Description"):BUFFER-VALUE = CHR(64 + ix)
           hTargetBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE = "rowid" + STRING(ix)
           cSelectedRows = cSelectedRows + "rowid" + STRING(ix) + ","
           .
  END.
END.

DYNAMIC-FUNCTION("setSelectedRowids" IN SOURCE-PROCEDURE,cSelectedRows).

/* DYNAMIC-FUNCTION("setAttribute",ihSourceBrw,"filterdropdownvaluelist_Description","A|A|B|B").  */

DYNAMIC-FUNCTION("setCurrentObject",ihSourceBrw).
RUN OpenQuerySource IN SOURCE-PROCEDURE.

DYNAMIC-FUNCTION("setCurrentObject",ihTargetBrw).
RUN OpenQueryTarget IN SOURCE-PROCEDURE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

