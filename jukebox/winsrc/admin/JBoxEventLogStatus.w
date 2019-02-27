&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          sports115        PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description:        Container for a JukeBox window program

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author:            brynjar@chemistry.no

  Created:           18.oct.2006

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
/* Uncomment to enable use of .Net components: */
/* &SCOPED-DEFINE AdvGuiWin */ 

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEF VAR bOk         AS LOG    NO-UNDO.
DEF VAR ix          AS INT    NO-UNDO.
DEF VAR hBrowse     AS HANDLE NO-UNDO.
DEF VAR hQuery      AS HANDLE NO-UNDO.
DEF VAR hToolbar    AS HANDLE NO-UNDO.
DEF VAR hFieldMap   AS HANDLE NO-UNDO.
DEF VAR oContainer  AS JBoxContainer NO-UNDO.


/*** Start instance property definitions for JBoxBrowse object oBrwJBoxEventLogStatus ***/
DEF VAR oBrwJBoxEventLogStatus AS JBoxBrowse NO-UNDO.
DEF TEMP-TABLE JBoxEventLogStatus
    FIELD cEventStatus AS character
    FIELD cEventStatusText AS character
    FIELD bUseInFollowUp AS logical
    FIELD dCreated AS date
    FIELD cCreatedBy AS character
    FIELD dModified AS date
    FIELD cModifiedBy AS character
    FIELD RowIdent1 AS CHARACTER 
    FIELD RowCount AS INTEGER
    FIELD jbCountDistinct AS INTEGER FORMAT '>>>,>>>,>>9' INIT 1
    FIELD jbAverage AS DECIMAL FORMAT '->>>,>>>,>>9.99'
    INDEX idxRowids  RowIdent1
    .
DEF BUFFER v_JBoxEventLogStatus FOR JBoxEventLogStatus.


FUNCTION getBuffersAndFieldsBrwJBoxEventLogStatus RETURNS CHARACTER():
  RETURN
    'JBoxEventLogStatus'
     + ';cEventStatus'
     + ';cEventStatusText'
     + ';bUseInFollowUp'
     + ';dCreated'
     + ';cCreatedBy'
     + ';dModified'
     + ';cModifiedBy'
     .
END FUNCTION.
FUNCTION getQueryJoinBrwJBoxEventLogStatus RETURNS CHARACTER():
  RETURN ''.
END FUNCTION.
DEF VAR oFmJBoxEventLogStatus AS JBoxFieldMap NO-UNDO.
DEF VAR otbJBoxEventLogStatus AS JBoxToolbar NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

&Scoped-define WIDGETID-FILE-NAME 

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BrwJBoxEventLogStatus

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES JBoxEventLogStatus

/* Definitions for BROWSE BrwJBoxEventLogStatus                         */
&Scoped-define FIELDS-IN-QUERY-BrwJBoxEventLogStatus ~
JBoxEventLogStatus.cEventStatus JBoxEventLogStatus.cEventStatusText ~
JBoxEventLogStatus.bUseInFollowUp JBoxEventLogStatus.dCreated ~
JBoxEventLogStatus.cCreatedBy JBoxEventLogStatus.dModified ~
JBoxEventLogStatus.cModifiedBy 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BrwJBoxEventLogStatus ~
JBoxEventLogStatus.cEventStatus 
&Scoped-define ENABLED-TABLES-IN-QUERY-BrwJBoxEventLogStatus ~
JBoxEventLogStatus
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BrwJBoxEventLogStatus JBoxEventLogStatus
&Scoped-define QUERY-STRING-BrwJBoxEventLogStatus FOR EACH JBoxEventLogStatus NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BrwJBoxEventLogStatus OPEN QUERY BrwJBoxEventLogStatus FOR EACH JBoxEventLogStatus NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BrwJBoxEventLogStatus JBoxEventLogStatus
&Scoped-define FIRST-TABLE-IN-QUERY-BrwJBoxEventLogStatus JBoxEventLogStatus


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tbJBoxEventLogStatus ~
new_tbJBoxEventLogStatus copy_tbJBoxEventLogStatus ~
undo_tbJBoxEventLogStatus delete_tbJBoxEventLogStatus ~
save_tbJBoxEventLogStatus excel_tbJBoxEventLogStatus BrwJBoxEventLogStatus ~
cEventStatus cEventStatusText bUseInFollowUp 
&Scoped-Define DISPLAYED-OBJECTS cEventStatus cEventStatusText ~
bUseInFollowUp 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON copy_tbJBoxEventLogStatus 
     IMAGE-UP FILE "bmp/copy.bmp":U
     LABEL "Kopier" 
     SIZE 6 BY 1.52 TOOLTIP "Kopier (ALT-K)".

DEFINE BUTTON delete_tbJBoxEventLogStatus 
     IMAGE-UP FILE "bmp/del16e.bmp":U
     LABEL "Slett" 
     SIZE 6 BY 1.52 TOOLTIP "Slett (CTRL-D)".

DEFINE BUTTON excel_tbJBoxEventLogStatus 
     IMAGE-UP FILE "gif/afexcel.gif":U
     LABEL "Eksporter til E&xcel" 
     SIZE 6 BY 1.52 TOOLTIP "Eksporter til E&xcel (ALT-X)".

DEFINE BUTTON new_tbJBoxEventLogStatus 
     IMAGE-UP FILE "bmp/new16e.bmp":U
     LABEL "Ny" 
     SIZE 6 BY 1.52 TOOLTIP "Ny (CTRL-N)".

DEFINE BUTTON save_tbJBoxEventLogStatus 
     IMAGE-UP FILE "bmp/save.bmp":U
     LABEL "Lagre" 
     SIZE 6 BY 1.52 TOOLTIP "Lagre (CTRL-S)".

DEFINE BUTTON undo_tbJBoxEventLogStatus 
     IMAGE-UP FILE "bmp/undo16e.bmp":U
     LABEL "Angre" 
     SIZE 6 BY 1.52 TOOLTIP "Angre (CTRL-Z)".

DEFINE VARIABLE cEventStatus AS CHARACTER FORMAT "x(20)" 
     LABEL "Status" 
     VIEW-AS FILL-IN 
     SIZE 17.6 BY 1.

DEFINE VARIABLE cEventStatusText AS CHARACTER FORMAT "x(30)" 
     LABEL "Tekst" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE RECTANGLE tbJBoxEventLogStatus
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 89.8 BY 1.71.

DEFINE VARIABLE bUseInFollowUp AS LOGICAL INITIAL no 
     LABEL "Oppgave" 
     VIEW-AS TOGGLE-BOX
     SIZE 14.8 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BrwJBoxEventLogStatus FOR 
      JBoxEventLogStatus SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BrwJBoxEventLogStatus
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BrwJBoxEventLogStatus C-Win _STRUCTURED
  QUERY BrwJBoxEventLogStatus NO-LOCK DISPLAY
      JBoxEventLogStatus.cEventStatus COLUMN-LABEL "Status" FORMAT "x(20)":U
      JBoxEventLogStatus.cEventStatusText COLUMN-LABEL "Tekst" FORMAT "x(40)":U
      JBoxEventLogStatus.bUseInFollowUp COLUMN-LABEL "Oppgave" FORMAT "yes/no":U
      JBoxEventLogStatus.dCreated FORMAT "99/99/9999":U
      JBoxEventLogStatus.cCreatedBy FORMAT "X(8)":U
      JBoxEventLogStatus.dModified FORMAT "99/99/9999":U
      JBoxEventLogStatus.cModifiedBy FORMAT "X(8)":U
  ENABLE
      JBoxEventLogStatus.cEventStatus
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 90 BY 9.05 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     new_tbJBoxEventLogStatus AT ROW 1.33 COL 2.2 WIDGET-ID 12
     copy_tbJBoxEventLogStatus AT ROW 1.33 COL 8.2 WIDGET-ID 14
     undo_tbJBoxEventLogStatus AT ROW 1.33 COL 14.4 WIDGET-ID 16
     delete_tbJBoxEventLogStatus AT ROW 1.33 COL 20.4 WIDGET-ID 18
     save_tbJBoxEventLogStatus AT ROW 1.33 COL 26.6 WIDGET-ID 20
     excel_tbJBoxEventLogStatus AT ROW 1.33 COL 32.6 WIDGET-ID 22
     BrwJBoxEventLogStatus AT ROW 3 COL 2 WIDGET-ID 200
     cEventStatus AT ROW 12.33 COL 20.2 COLON-ALIGNED WIDGET-ID 4
     cEventStatusText AT ROW 13.33 COL 20.2 COLON-ALIGNED WIDGET-ID 6
     bUseInFollowUp AT ROW 14.33 COL 22.2 WIDGET-ID 8
     tbJBoxEventLogStatus AT ROW 1.24 COL 2 WIDGET-ID 10
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 91.8 BY 14.91 WIDGET-ID 100.


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
         TITLE              = "<Insert window title>"
         HEIGHT             = 14.91
         WIDTH              = 91.8
         MAX-HEIGHT         = 23.52
         MAX-WIDTH          = 114.2
         VIRTUAL-HEIGHT     = 23.52
         VIRTUAL-WIDTH      = 114.2
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
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB BrwJBoxEventLogStatus excel_tbJBoxEventLogStatus DEFAULT-FRAME */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 14.91
       FRAME DEFAULT-FRAME:WIDTH            = 91.8.

ASSIGN 
       tbJBoxEventLogStatus:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "new;Ny,copy;Kopier,undo;Angre,delete;Slett,save;Lagre,excel;Eksporter til E&xcelmaxborder".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BrwJBoxEventLogStatus
/* Query rebuild information for BROWSE BrwJBoxEventLogStatus
     _TblList          = "sports115.JBoxEventLogStatus"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > JBoxEventLogStatus.cEventStatus
"JBoxEventLogStatus.cEventStatus" "Status" "x(20)" "character" ? ? ? ? ? ? yes "" no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > JBoxEventLogStatus.cEventStatusText
"JBoxEventLogStatus.cEventStatusText" "Tekst" "x(40)" "character" ? ? ? ? ? ? no "" no no "40" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > JBoxEventLogStatus.bUseInFollowUp
"JBoxEventLogStatus.bUseInFollowUp" "Oppgave" "yes/no" "logical" ? ? ? ? ? ? no "" no no "15.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > JBoxEventLogStatus.dCreated
"JBoxEventLogStatus.dCreated" "Opprettet" "99/99/9999" "date" ? ? ? ? ? ? no "" no no "11.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > JBoxEventLogStatus.cCreatedBy
"JBoxEventLogStatus.cCreatedBy" "Av" "X(8)" "character" ? ? ? ? ? ? no "" no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > JBoxEventLogStatus.dModified
"JBoxEventLogStatus.dModified" "Endret" "99/99/9999" "date" ? ? ? ? ? ? no "" no no "11.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > JBoxEventLogStatus.cModifiedBy
"JBoxEventLogStatus.cModifiedBy" "Av" "X(8)" "character" ? ? ? ? ? ? no "" no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BrwJBoxEventLogStatus */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <Insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <Insert window title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BrwJBoxEventLogStatus
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
  IF NOT DYNAMIC-FUNCTION("DoCleanUpObjects",THIS-PROCEDURE:CURRENT-WINDOW) THEN RETURN NO-APPLY.
  DYNAMIC-FUNCTION("SetCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
  PUBLISH "InvalidateHandle" (THIS-PROCEDURE).
  RUN disable_UI.
END.

{incl/wintrigg.i}
{incl/conttrigg.i oBrwJBoxEventLogStatus:BROWSE-HANDLE}  

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  &IF DEFINED(UIB_is_Running) NE 0 &THEN
    RUN InitializeObject.
    RUN MoveToTop.
    IF DYNAMIC-FUNCTION("getAttribute",THIS-PROCEDURE,"advGuiWin") = "YES" THEN
      RUN WaitForForm NO-ERROR.
  &ENDIF

  IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
    &IF DEFINED(UIB_is_Running) = 0 &THEN
    RUN InitializeObject.
    RUN MoveToTop.
    APPLY "WINDOW-RESIZED" TO {&WINDOW-NAME}.
    SESSION:SET-WAIT-STATE("").
    IF DYNAMIC-FUNCTION("getAttribute",THIS-PROCEDURE,"advGuiWin") = "YES" THEN
      RUN WaitForForm NO-ERROR.
    ELSE 
    &ENDIF
      WAIT-FOR CLOSE OF THIS-PROCEDURE.
  END.  
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
  DISPLAY cEventStatus cEventStatusText bUseInFollowUp 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE tbJBoxEventLogStatus new_tbJBoxEventLogStatus 
         copy_tbJBoxEventLogStatus undo_tbJBoxEventLogStatus 
         delete_tbJBoxEventLogStatus save_tbJBoxEventLogStatus 
         excel_tbJBoxEventLogStatus BrwJBoxEventLogStatus cEventStatus 
         cEventStatusText bUseInFollowUp 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeObject C-Win 
PROCEDURE InitializeObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Refer to the <jukebox>\winsrc\samples for working examples for Sports2000
------------------------------------------------------------------------------*/
RUN enable_UI.

oContainer = NEW JBoxContainer().
oContainer:addStatusBar().

DO WITH FRAME {&FRAME-NAME}:

  oBrwJBoxEventLogStatus = NEW JBoxBrowse(brwJBoxEventLogStatus:HANDLE).

  oFmJBoxEventLogStatus = NEW JBoxFieldMap(FRAME {&FRAME-NAME}:HANDLE).
  oFmJBoxEventLogStatus:updateFields = 'cEventStatus,cEventStatusText,bUseInFollowUp'.
  oFmJBoxEventLogStatus:primaryKeyFields = 'cEventStatus'.

  oFmJBoxEventLogStatus:BROWSE-OBJECT = oBrwJBoxEventLogStatus.
  otbJBoxEventLogStatus = NEW JBoxToolbar(tbJBoxEventLogStatus:HANDLE).

  oBrwJBoxEventLogStatus:TOOLBAR-OBJECT = otbJBoxEventLogStatus.
  oFmJBoxEventLogStatus:TOOLBAR-OBJECT = otbJBoxEventLogStatus.
END.
oBrwJBoxEventLogStatus:OpenQuery().


oContainer:initResize(300,200).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MoveToTop C-Win 
PROCEDURE MoveToTop :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = NO.
IF DYNAMIC-FUNCTION("getAttribute",THIS-PROCEDURE,"advGuiWin") = "YES" THEN
  RUN ShowForm("").
THIS-PROCEDURE:CURRENT-WINDOW:WINDOW-STATE = 3.
THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
DYNAMIC-FUNCTION("DoLockWindow",?).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

