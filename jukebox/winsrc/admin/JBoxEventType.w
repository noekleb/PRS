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


/*** Start instance property definitions for JBoxBrowse object oBrwJBoxEventLogType ***/
DEF VAR oBrwJBoxEventLogType AS JBoxBrowse NO-UNDO.
DEF TEMP-TABLE JBoxEventLogType
    FIELD cEventLogType AS character
    FIELD cEventLogTypeText AS character
    FIELD cJBoxUserId AS character
    FIELD cUserName AS character
    FIELD cRoutine AS character
    FIELD dCreated AS date
    FIELD cCreatedBy AS character
    FIELD dModified AS date
    FIELD cModifiedBy AS character
    FIELD RowIdent1 AS CHARACTER 
    FIELD RowIdent2 AS CHARACTER 
    FIELD RowCount AS INTEGER
    FIELD jbCountDistinct AS INTEGER FORMAT '>>>,>>>,>>9' INIT 1
    FIELD jbAverage AS DECIMAL FORMAT '->>>,>>>,>>9.99'
    INDEX idxRowids  RowIdent1 RowIdent2
    .
DEF BUFFER v_JBoxEventLogType FOR JBoxEventLogType.


FUNCTION getBuffersAndFieldsBrwJBoxEventLogType RETURNS CHARACTER():
  RETURN
    'JBoxEventLogType'
     + ';cEventLogType'
     + ';cEventLogTypeText'
     + ';cJBoxUserId'
     + ';cRoutine'
     + ';dCreated'
     + ';cCreatedBy'
     + ';dModified'
     + ';cModifiedBy'
  + ',JBoxUser'
     + ';cUserName'
     .
END FUNCTION.
FUNCTION getQueryJoinBrwJBoxEventLogType RETURNS CHARACTER():
  RETURN 'EACH JBoxUser OF JBoxEventLogType OUTER-JOIN NO-LOCK'.
END FUNCTION.
DEF VAR oFmJBoxEventLogType AS JBoxFieldMap NO-UNDO.
DEF VAR otbJBoxEventLogType AS JBoxToolbar NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BrwJBoxEventLogType

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES JBoxEventLogType

/* Definitions for BROWSE BrwJBoxEventLogType                           */
&Scoped-define FIELDS-IN-QUERY-BrwJBoxEventLogType ~
JBoxEventLogType.cEventLogType JBoxEventLogType.cEventLogTypeText ~
JBoxEventLogType.cJBoxUserId JBoxEventLogType.cUserName ~
JBoxEventLogType.cRoutine JBoxEventLogType.dCreated ~
JBoxEventLogType.cCreatedBy JBoxEventLogType.dModified ~
JBoxEventLogType.cModifiedBy 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BrwJBoxEventLogType ~
JBoxEventLogType.cEventLogType 
&Scoped-define ENABLED-TABLES-IN-QUERY-BrwJBoxEventLogType JBoxEventLogType
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BrwJBoxEventLogType JBoxEventLogType
&Scoped-define QUERY-STRING-BrwJBoxEventLogType FOR EACH JBoxEventLogType NO-LOCK, ~
    EACH JBoxUser OF JBoxEventLogType OUTER-JOIN NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BrwJBoxEventLogType OPEN QUERY BrwJBoxEventLogType FOR EACH JBoxEventLogType NO-LOCK, ~
    EACH JBoxUser OF JBoxEventLogType OUTER-JOIN NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BrwJBoxEventLogType JBoxEventLogType
&Scoped-define FIRST-TABLE-IN-QUERY-BrwJBoxEventLogType JBoxEventLogType


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tbJBoxEventLogType new_tbJBoxEventLogType ~
edit_tbJBoxEventLogType copy_tbJBoxEventLogType delete_tbJBoxEventLogType ~
undo_tbJBoxEventLogType save_tbJBoxEventLogType filter_tbJBoxEventLogType ~
excel_tbJBoxEventLogType BrwJBoxEventLogType cEventLogType ~
cEventLogTypeText cJBoxUserId btncJBoxUserId cUserName cRoutine 
&Scoped-Define DISPLAYED-OBJECTS cEventLogType cEventLogTypeText ~
cJBoxUserId cUserName cRoutine 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btncJBoxUserId 
     LABEL "..." 
     SIZE 4 BY 1.

DEFINE BUTTON copy_tbJBoxEventLogType 
     IMAGE-UP FILE "bmp/copy.bmp":U
     LABEL "Kopier" 
     SIZE 6 BY 1.52 TOOLTIP "Kopier (ALT-K)".

DEFINE BUTTON delete_tbJBoxEventLogType 
     IMAGE-UP FILE "bmp/del16e.bmp":U
     LABEL "Slett" 
     SIZE 6 BY 1.52 TOOLTIP "Slett (CTRL-D)".

DEFINE BUTTON edit_tbJBoxEventLogType 
     IMAGE-UP FILE "bmp/edit16e.bmp":U
     LABEL "Edit" 
     SIZE 6 BY 1.52 TOOLTIP "Edit (CTRL-E)".

DEFINE BUTTON excel_tbJBoxEventLogType 
     IMAGE-UP FILE "gif/afexcel.gif":U
     LABEL "Eksporter til E&xcel" 
     SIZE 6 BY 1.52 TOOLTIP "Eksporter til E&xcel (ALT-X)".

DEFINE BUTTON filter_tbJBoxEventLogType 
     IMAGE-UP FILE "gif/filter.gif":U
     LABEL "Filter" 
     SIZE 6 BY 1.52 TOOLTIP "Filter (CTRL-F)".

DEFINE BUTTON new_tbJBoxEventLogType 
     IMAGE-UP FILE "bmp/new16e.bmp":U
     LABEL "Ny" 
     SIZE 6 BY 1.52 TOOLTIP "Ny (CTRL-N)".

DEFINE BUTTON save_tbJBoxEventLogType 
     IMAGE-UP FILE "bmp/save.bmp":U
     LABEL "Lagre" 
     SIZE 6 BY 1.52 TOOLTIP "Lagre (CTRL-S)".

DEFINE BUTTON undo_tbJBoxEventLogType 
     IMAGE-UP FILE "bmp/undo16e.bmp":U
     LABEL "Angre" 
     SIZE 6 BY 1.52 TOOLTIP "Angre (CTRL-Z)".

DEFINE VARIABLE cJBoxUserId AS CHARACTER 
     LABEL "Saksbehandler" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEMS "Item 1" 
     DROP-DOWN
     SIZE 29 BY 1.

DEFINE VARIABLE cRoutine AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 99 BY 3.1.

DEFINE VARIABLE cEventLogType AS CHARACTER FORMAT "x(30)" 
     LABEL "Type" 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1.

DEFINE VARIABLE cEventLogTypeText AS CHARACTER FORMAT "x(40)" 
     LABEL "Beskrivelse" 
     VIEW-AS FILL-IN 
     SIZE 60 BY 1.

DEFINE VARIABLE cUserName AS CHARACTER FORMAT "X(40)" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1.

DEFINE RECTANGLE tbJBoxEventLogType
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 99.2 BY 1.71.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BrwJBoxEventLogType FOR 
      JBoxEventLogType SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BrwJBoxEventLogType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BrwJBoxEventLogType C-Win _STRUCTURED
  QUERY BrwJBoxEventLogType NO-LOCK DISPLAY
      JBoxEventLogType.cEventLogType COLUMN-LABEL "Type" FORMAT "x(20)":U
      JBoxEventLogType.cEventLogTypeText COLUMN-LABEL "Beskrivelse" FORMAT "x(40)":U
      JBoxEventLogType.cJBoxUserId COLUMN-LABEL "Saksbehandler" FORMAT "x(30)":U
            WIDTH 16.6
      JBoxEventLogType.cUserName COLUMN-LABEL "Navn" FORMAT "X(40)":U
      JBoxEventLogType.cRoutine COLUMN-LABEL "Rutine" FORMAT "x(80)":U
      JBoxEventLogType.dCreated COLUMN-LABEL "Opprettet" FORMAT "99/99/9999":U
      JBoxEventLogType.cCreatedBy COLUMN-LABEL "Av" FORMAT "X(8)":U
      JBoxEventLogType.dModified COLUMN-LABEL "Endret" FORMAT "99/99/9999":U
      JBoxEventLogType.cModifiedBy COLUMN-LABEL "Av" FORMAT "X(8)":U
            WIDTH 6
  ENABLE
      JBoxEventLogType.cEventLogType
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 99 BY 11.38 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     new_tbJBoxEventLogType AT ROW 1.24 COL 2.2 WIDGET-ID 8
     edit_tbJBoxEventLogType AT ROW 1.24 COL 8.2 WIDGET-ID 10
     copy_tbJBoxEventLogType AT ROW 1.24 COL 14.4 WIDGET-ID 12
     delete_tbJBoxEventLogType AT ROW 1.24 COL 20.4 WIDGET-ID 14
     undo_tbJBoxEventLogType AT ROW 1.24 COL 26.6 WIDGET-ID 16
     save_tbJBoxEventLogType AT ROW 1.24 COL 32.6 WIDGET-ID 18
     filter_tbJBoxEventLogType AT ROW 1.24 COL 38.8 WIDGET-ID 20
     excel_tbJBoxEventLogType AT ROW 1.24 COL 44.8 WIDGET-ID 22
     BrwJBoxEventLogType AT ROW 2.95 COL 2 WIDGET-ID 200
     cEventLogType AT ROW 14.57 COL 19 COLON-ALIGNED
     cEventLogTypeText AT ROW 15.62 COL 19 COLON-ALIGNED
     cJBoxUserId AT ROW 16.67 COL 19 COLON-ALIGNED WIDGET-ID 26
     btncJBoxUserId AT ROW 16.67 COL 50 WIDGET-ID 24 NO-TAB-STOP 
     cUserName AT ROW 16.67 COL 52 COLON-ALIGNED NO-LABEL
     cRoutine AT ROW 18.38 COL 2 NO-LABEL WIDGET-ID 30
     "Rutine:" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 17.67 COL 3 WIDGET-ID 32
     tbJBoxEventLogType AT ROW 1.14 COL 2 WIDGET-ID 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 101.2 BY 20.67 WIDGET-ID 100.


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
         HEIGHT             = 20.67
         WIDTH              = 101.2
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
/* BROWSE-TAB BrwJBoxEventLogType excel_tbJBoxEventLogType DEFAULT-FRAME */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 20.67
       FRAME DEFAULT-FRAME:WIDTH            = 101.2.

ASSIGN 
       tbJBoxEventLogType:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "new;Ny,edit;Edit,copy;Kopier,delete;Slett,undo;Angre,save;Lagre,filter;Filter,excel;Eksporter til E&xcelmaxborder".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BrwJBoxEventLogType
/* Query rebuild information for BROWSE BrwJBoxEventLogType
     _TblList          = "sports115.JBoxEventLogType"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _TblOptList       = ", OUTER"
     _FldNameList[1]   > JBoxEventLogType.cEventLogType
"JBoxEventLogType.cEventLogType" "Type" "x(20)" "character" ? ? ? ? ? ? yes "" no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > JBoxEventLogType.cEventLogTypeText
"JBoxEventLogType.cEventLogTypeText" "Beskrivelse" "x(40)" "character" ? ? ? ? ? ? no "" no no "40" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > JBoxEventLogType.cJBoxUserId
"JBoxEventLogType.cJBoxUserId" "Saksbehandler" "x(30)" "character" ? ? ? ? ? ? no "" no no "16.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > JBoxEventLogType.cUserName
"JBoxEventLogType.cUserName" "Navn" "X(40)" "character" ? ? ? ? ? ? no "" no no "40" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > JBoxEventLogType.cRoutine
"JBoxEventLogType.cRoutine" "Rutine" "x(80)" "character" ? ? ? ? ? ? no "" no no "80" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > JBoxEventLogType.dCreated
"JBoxEventLogType.dCreated" "Opprettet" "99/99/9999" "date" ? ? ? ? ? ? no "" no no "11.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > JBoxEventLogType.cCreatedBy
"JBoxEventLogType.cCreatedBy" "Av" "X(8)" "character" ? ? ? ? ? ? no "" no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > JBoxEventLogType.dModified
"JBoxEventLogType.dModified" "Endret" "99/99/9999" "date" ? ? ? ? ? ? no "" no no "11.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > JBoxEventLogType.cModifiedBy
"JBoxEventLogType.cModifiedBy" "Av" "X(8)" "character" ? ? ? ? ? ? no "" no no "6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BrwJBoxEventLogType */
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


&Scoped-define SELF-NAME btncJBoxUserId
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btncJBoxUserId C-Win
ON CHOOSE OF btncJBoxUserId IN FRAME DEFAULT-FRAME /* ... */
DO:
  DEF VAR cReturnValues   AS CHAR NO-UNDO.
  DEF VAR bOk             AS LOG  NO-UNDO.

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxLookup.w (THIS-PROCEDURE,200,
                    "JBoxUser"
                    + ";cUserName"
                    + ";cJBoxUserId"
                   ,"WHERE true"
                    ,""                                                  
                    ,"cJBoxUserId,cUserName",   /* <- return values for these fields */
                    OUTPUT cReturnValues,
                    OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk AND cReturnValues NE "" THEN DO:
    ASSIGN cJBoxUserId:SCREEN-VALUE = ENTRY(1,cReturnValues,"|")
           cUserName:SCREEN-VALUE = ENTRY(2,cReturnValues,"|")
           .

    APPLY "any-printable" TO cJBoxUserId.
  END.
  THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BrwJBoxEventLogType
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

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

{incl/wintrigg.i}
{incl/conttrigg.i oBrwJBoxEventLogType:BROWSE-HANDLE} 

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
  DISPLAY cEventLogType cEventLogTypeText cJBoxUserId cUserName cRoutine 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE tbJBoxEventLogType new_tbJBoxEventLogType edit_tbJBoxEventLogType 
         copy_tbJBoxEventLogType delete_tbJBoxEventLogType 
         undo_tbJBoxEventLogType save_tbJBoxEventLogType 
         filter_tbJBoxEventLogType excel_tbJBoxEventLogType BrwJBoxEventLogType 
         cEventLogType cEventLogTypeText cJBoxUserId btncJBoxUserId cUserName 
         cRoutine 
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
  &IF DEFINED(UIB_is_Running) NE 0 &THEN
    DYNAMIC-FUNCTION("setAttribute",SESSION,"taskUserMethod","eiendom_boligansvar.p").
  &ENDIF
  
  cJBoxUserId:LIST-ITEMS = DYNAMIC-FUNCTION("getAttribute",SESSION,"taskUserMethod").

  oBrwJBoxEventLogType = NEW JBoxBrowse(brwJBoxEventLogType:HANDLE).
  oBrwJBoxEventLogType:setNoResizeY().

  oFmJBoxEventLogType = NEW JBoxFieldMap(FRAME {&FRAME-NAME}:HANDLE).
  oFmJBoxEventLogType:updateFields = 'cEventLogType,cEventLogTypeText,cJBoxUserId,cRoutine'.
  oFmJBoxEventLogType:displayFields = 'cUserName'.
  oFmJBoxEventLogType:primaryKeyFields = 'cEventLogType'.
  oFmJBoxEventLogType:dynamicValidation = NO.

  oFmJBoxEventLogType:BROWSE-OBJECT = oBrwJBoxEventLogType.
  otbJBoxEventLogType = NEW JBoxToolbar(tbJBoxEventLogType:HANDLE).

  oBrwJBoxEventLogType:TOOLBAR-OBJECT = otbJBoxEventLogType.
  oFmJBoxEventLogType:TOOLBAR-OBJECT = otbJBoxEventLogType.
END.
oBrwJBoxEventLogType:OpenQuery().


oContainer:initResize(300,800).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LeaveOfFieldHandle C-Win 
PROCEDURE LeaveOfFieldHandle :
/*------------------------------------------------------------------------------
  Purpose:     Retrieve lookup values when a foreign key field is modified
  Parameters:  Handle to foreign key field
  Notes:       The cReturn variable should be replaced with a fill-in
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihField AS HANDLE NO-UNDO.

DEF VAR cReturn AS CHAR NO-UNDO.

IF ihField:MODIFIED THEN DO WITH FRAME {&FRAME-NAME}:
  CASE ihField:NAME:
    WHEN "cJBoxUserId" THEN cUserName:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","JBoxUser","WHERE cJBoxUserId = '" + ihField:SCREEN-VALUE + "'","cUserName").
  END CASE.
END.

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

