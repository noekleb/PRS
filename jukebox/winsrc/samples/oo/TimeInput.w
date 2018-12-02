&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          sports117        PROGRESS
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


/*** Start instance property definitions for JBoxBrowse object oBrwJBoxLoginSession ***/
DEF VAR oBrwJBoxLoginSession AS JBoxBrowse NO-UNDO.
DEF TEMP-TABLE JBoxLoginSession
    FIELD cJBoxUserId AS character
    FIELD cLanguage AS character
    FIELD cNumFormat AS character
    FIELD cSessionId AS character
    FIELD dCreated AS date
    FIELD iLoginTime AS integer
    FIELD iLogoutTime AS integer
    FIELD iLoginTime_time AS CHARACTER
    FIELD iLogoutTime_time AS CHARACTER
    FIELD RowIdent1 AS CHARACTER 
    FIELD RowCount AS INTEGER
    FIELD jbCountDistinct AS INTEGER FORMAT '>>>,>>>,>>9' INIT 1
    FIELD jbAverage AS DECIMAL FORMAT '->>>,>>>,>>9.99'
    INDEX idxRowids  RowIdent1
    .
DEF BUFFER v_JBoxLoginSession FOR TEMP-TABLE JBoxLoginSession.


FUNCTION getBuffersAndFieldsBrwJBoxLoginSession RETURNS CHARACTER():
  RETURN
    'JBoxLoginSession'
     + ';cJBoxUserId'
     + ';cLanguage'
     + ';cNumFormat'
     + ';cSessionId'
     + ';dCreated'
     + ';iLoginTime'
     + ';iLogoutTime'
     + ';+iLoginTime_time|CHARACTER||jb_hhmm(iLoginTime)|Login'
     + ';+iLogoutTime_time|CHARACTER||jb_hhmm(iLogoutTime)|Logout time'
     .
END FUNCTION.
FUNCTION getQueryJoinBrwJBoxLoginSession RETURNS CHARACTER():
  RETURN ''.
END FUNCTION.
DEF VAR oFmJBoxLoginSession AS JBoxFieldMap NO-UNDO.


DEF VAR otbJBoxLoginSession AS JBoxToolbar NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BrwJBoxLoginSession

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES JBoxLoginSession

/* Definitions for BROWSE BrwJBoxLoginSession                           */
&Scoped-define FIELDS-IN-QUERY-BrwJBoxLoginSession ~
JBoxLoginSession.cJBoxUserId JBoxLoginSession.cLanguage ~
JBoxLoginSession.cNumFormat JBoxLoginSession.cSessionId ~
JBoxLoginSession.dCreated JBoxLoginSession.iLoginTime ~
JBoxLoginSession.iLogoutTime JBoxLoginSession.iLoginTime_time ~
JBoxLoginSession.iLogoutTime_time 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BrwJBoxLoginSession ~
JBoxLoginSession.cJBoxUserId 
&Scoped-define QUERY-STRING-BrwJBoxLoginSession FOR EACH JBoxLoginSession NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BrwJBoxLoginSession OPEN QUERY BrwJBoxLoginSession FOR EACH JBoxLoginSession NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BrwJBoxLoginSession JBoxLoginSession
&Scoped-define FIRST-TABLE-IN-QUERY-BrwJBoxLoginSession JBoxLoginSession


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS new_tbJBoxLoginSession ~
edit_tbJBoxLoginSession copy_tbJBoxLoginSession undo_tbJBoxLoginSession ~
save_tbJBoxLoginSession delete_tbJBoxLoginSession filter_tbJBoxLoginSession ~
excel_tbJBoxLoginSession tbJBoxLoginSession BrwJBoxLoginSession cJBoxUserId ~
iLoginTime_time cLanguage iLogoutTime_time cNumFormat 
&Scoped-Define DISPLAYED-OBJECTS cJBoxUserId iLoginTime_time cLanguage ~
iLogoutTime_time cNumFormat 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON copy_tbJBoxLoginSession 
     IMAGE-UP FILE "bmp/copy.bmp":U
     LABEL "Kopier" 
     SIZE 6 BY 1.52 TOOLTIP "Kopier (ALT-K)".

DEFINE BUTTON delete_tbJBoxLoginSession 
     IMAGE-UP FILE "bmp/del16e.bmp":U
     LABEL "Slett" 
     SIZE 6 BY 1.52 TOOLTIP "Slett (CTRL-D)".

DEFINE BUTTON edit_tbJBoxLoginSession 
     IMAGE-UP FILE "bmp/edit16e.bmp":U
     LABEL "Edit" 
     SIZE 6 BY 1.52 TOOLTIP "Edit (CTRL-E)".

DEFINE BUTTON excel_tbJBoxLoginSession 
     IMAGE-UP FILE "gif/afexcel.gif":U
     LABEL "Eksporter til E&xcel" 
     SIZE 6 BY 1.52 TOOLTIP "Eksporter til E&xcel (ALT-X)".

DEFINE BUTTON filter_tbJBoxLoginSession 
     IMAGE-UP FILE "gif/filter.gif":U
     LABEL "Filter" 
     SIZE 6 BY 1.52 TOOLTIP "Filter (CTRL-F)".

DEFINE BUTTON new_tbJBoxLoginSession 
     IMAGE-UP FILE "bmp/new16e.bmp":U
     LABEL "Ny" 
     SIZE 6 BY 1.52 TOOLTIP "Ny (CTRL-N)".

DEFINE BUTTON save_tbJBoxLoginSession 
     IMAGE-UP FILE "bmp/save.bmp":U
     LABEL "Lagre" 
     SIZE 6 BY 1.52 TOOLTIP "Lagre (CTRL-S)".

DEFINE BUTTON undo_tbJBoxLoginSession 
     IMAGE-UP FILE "bmp/undo16e.bmp":U
     LABEL "Angre" 
     SIZE 6 BY 1.52 TOOLTIP "Angre (CTRL-Z)".

DEFINE VARIABLE cJBoxUserId AS CHARACTER FORMAT "X(8)" 
     LABEL "User id" 
     VIEW-AS FILL-IN 
     SIZE 13.6 BY 1.

DEFINE VARIABLE cLanguage AS CHARACTER FORMAT "XX" 
     LABEL "Språk" 
     VIEW-AS FILL-IN 
     SIZE 6.4 BY 1.

DEFINE VARIABLE cNumFormat AS CHARACTER FORMAT "X(12)" 
     LABEL "Numeric format" 
     VIEW-AS FILL-IN 
     SIZE 17.6 BY 1.

DEFINE VARIABLE iLoginTime_time AS CHARACTER FORMAT "99:99":U 
     LABEL "Login" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE iLogoutTime_time AS CHARACTER FORMAT "99:99":U 
     LABEL "Logout" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE tbJBoxLoginSession
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 61.8 BY 1.71.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BrwJBoxLoginSession FOR 
      JBoxLoginSession SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BrwJBoxLoginSession
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BrwJBoxLoginSession C-Win _STRUCTURED
  QUERY BrwJBoxLoginSession NO-LOCK DISPLAY
      JBoxLoginSession.cJBoxUserId COLUMN-LABEL "User id" FORMAT "X(8)":U
      JBoxLoginSession.cLanguage COLUMN-LABEL "Språk" FORMAT "XX":U
      JBoxLoginSession.cNumFormat COLUMN-LABEL "Numeric format" FORMAT "X(12)":U
      JBoxLoginSession.cSessionId COLUMN-LABEL "Session id" FORMAT "X(40)":U
      JBoxLoginSession.dCreated COLUMN-LABEL "Opprettet" FORMAT "99/99/9999":U
      JBoxLoginSession.iLoginTime COLUMN-LABEL "Login" FORMAT "->>>>>9":U
      JBoxLoginSession.iLogoutTime COLUMN-LABEL "Logout time" FORMAT "->>>>>9":U
      JBoxLoginSession.iLoginTime_time COLUMN-LABEL "Login" FORMAT "x(6)":U
      JBoxLoginSession.iLogoutTime_time COLUMN-LABEL "Logout time" FORMAT "x(6)":U
  ENABLE
      JBoxLoginSession.cJBoxUserId
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 59 BY 4.52 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     new_tbJBoxLoginSession AT ROW 1.1 COL 1.2 WIDGET-ID 4
     edit_tbJBoxLoginSession AT ROW 1.1 COL 7.2 WIDGET-ID 6
     copy_tbJBoxLoginSession AT ROW 1.1 COL 13.4 WIDGET-ID 8
     undo_tbJBoxLoginSession AT ROW 1.1 COL 19.4 WIDGET-ID 10
     save_tbJBoxLoginSession AT ROW 1.1 COL 25.6 WIDGET-ID 12
     delete_tbJBoxLoginSession AT ROW 1.1 COL 31.6 WIDGET-ID 14
     filter_tbJBoxLoginSession AT ROW 1.1 COL 37.8 WIDGET-ID 16
     excel_tbJBoxLoginSession AT ROW 1.1 COL 43.8 WIDGET-ID 18
     BrwJBoxLoginSession AT ROW 3.05 COL 3 WIDGET-ID 200
     cJBoxUserId AT ROW 8 COL 15 COLON-ALIGNED
     iLoginTime_time AT ROW 8 COL 43 COLON-ALIGNED
     cLanguage AT ROW 9 COL 15 COLON-ALIGNED
     iLogoutTime_time AT ROW 9.1 COL 43 COLON-ALIGNED
     cNumFormat AT ROW 10 COL 15 COLON-ALIGNED
     tbJBoxLoginSession AT ROW 1 COL 1 WIDGET-ID 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 62.8 BY 11.38 WIDGET-ID 100.


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
         HEIGHT             = 11.33
         WIDTH              = 63.4
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
&IF "{&WINDOW-SYSTEM}" EQ "TTY" &THEN
  IF C-Win = ? THEN
    CREATE FRAME C-Win
           ASSIGN HIDDEN = YES.
&ENDIF

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
/* BROWSE-TAB BrwJBoxLoginSession tbJBoxLoginSession DEFAULT-FRAME */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 11.38
       FRAME DEFAULT-FRAME:WIDTH            = 62.8.

ASSIGN 
       tbJBoxLoginSession:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "new;Ny,edit;Edit,copy;Kopier,undo;Angre,save;Lagre,delete;Slett,filter;Filter,excel;Eksporter til E&xcelmaxborder".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BrwJBoxLoginSession
/* Query rebuild information for BROWSE BrwJBoxLoginSession
     _TblList          = "sports117.JBoxLoginSession"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > "_<CALC>"
"JBoxLoginSession.cJBoxUserId" "User id" "X(8)" "character" ? ? ? ? ? ? yes "" no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > "_<CALC>"
"JBoxLoginSession.cLanguage" "Språk" "XX" "character" ? ? ? ? ? ? no "" no no "5.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > "_<CALC>"
"JBoxLoginSession.cNumFormat" "Numeric format" "X(12)" "character" ? ? ? ? ? ? no "" no no "14.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > "_<CALC>"
"JBoxLoginSession.cSessionId" "Session id" "X(40)" "character" ? ? ? ? ? ? no "" no no "40" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > "_<CALC>"
"JBoxLoginSession.dCreated" "Opprettet" "99/99/9999" "date" ? ? ? ? ? ? no "" no no "11.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > "_<CALC>"
"JBoxLoginSession.iLoginTime" "Login" "->>>>>9" "integer" ? ? ? ? ? ? no "" no no "7.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > "_<CALC>"
"JBoxLoginSession.iLogoutTime" "Logout time" "->>>>>9" "integer" ? ? ? ? ? ? no "" no no "11" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > "_<CALC>"
"JBoxLoginSession.iLoginTime_time" "Login" "x(6)" "CHARACTER" ? ? ? ? ? ? no "" no no "6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > "_<CALC>"
"JBoxLoginSession.iLogoutTime_time" "Logout time" "x(6)" "CHARACTER" ? ? ? ? ? ? no "" no no "11" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BrwJBoxLoginSession */
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


&Scoped-define BROWSE-NAME BrwJBoxLoginSession
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

&IF "{&WINDOW-SYSTEM}" NE "TTY" &THEN  
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.
&ENDIF

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
  IF NOT DYNAMIC-FUNCTION("DoCleanUpObjects",THIS-PROCEDURE:CURRENT-WINDOW) THEN RETURN NO-APPLY.
  DYNAMIC-FUNCTION("SetCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
  PUBLISH "InvalidateHandle" (THIS-PROCEDURE).
  RUN disable_UI.
END.

&IF "{&WINDOW-SYSTEM}" NE "TTY" &THEN  
{incl/wintrigg.i}
{incl/conttrigg.i oBrwJBoxLoginSession:BROWSE-HANDLE}
&ENDIF

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
  DISPLAY cJBoxUserId iLoginTime_time cLanguage iLogoutTime_time cNumFormat 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE new_tbJBoxLoginSession edit_tbJBoxLoginSession copy_tbJBoxLoginSession 
         undo_tbJBoxLoginSession save_tbJBoxLoginSession 
         delete_tbJBoxLoginSession filter_tbJBoxLoginSession 
         excel_tbJBoxLoginSession tbJBoxLoginSession BrwJBoxLoginSession 
         cJBoxUserId iLoginTime_time cLanguage iLogoutTime_time cNumFormat 
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
&IF "{&WINDOW-SYSTEM}" NE "TTY" &THEN  
  RUN enable_UI.

  oContainer = NEW JBoxContainer().
  oContainer:addStatusBar().
&ENDIF

DO WITH FRAME {&FRAME-NAME}:

  oBrwJBoxLoginSession = NEW JBoxBrowse(brwJBoxLoginSession:HANDLE).

  oFmJBoxLoginSession = NEW JBoxFieldMap(FRAME {&FRAME-NAME}:HANDLE).
  oFmJBoxLoginSession:updateFields = 'cJBoxUserId,cLanguage,cNumFormat,iLoginTime_time,iLogoutTime_time'.
  oFmJBoxLoginSession:timeInputFields = "iLoginTime_time,iLogoutTime_time".
  oFmJBoxLoginSession:bufferExtraFields = "iLoginTime,iLogoutTime".

  oFmJBoxLoginSession:BROWSE-OBJECT = oBrwJBoxLoginSession.
  otbJBoxLoginSession = NEW JBoxToolbar(tbJBoxLoginSession:HANDLE).

  oBrwJBoxLoginSession:TOOLBAR-OBJECT = otbJBoxLoginSession.
  oFmJBoxLoginSession:TOOLBAR-OBJECT = otbJBoxLoginSession.
END.
oBrwJBoxLoginSession:OpenQuery().

&IF "{&WINDOW-SYSTEM}" NE "TTY" &THEN  
  oContainer:initResize(500,100).
&ELSE
  DYNAMIC-FUNCTION("setASlibBehaviour",
                    "QueryLogFile|ServerQuery.log," +   
                    "TransLogFile|ServerTrans.log").
&ENDIF

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SaveRecord C-Win 
PROCEDURE SaveRecord :
  DEF VAR iFrom AS INT NO-UNDO.
  DEF VAR iTo   AS INT NO-UNDO.
  
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN iFrom = INT(ENTRY(1,iLoginTime_time:SCREEN-VALUE,":")) * 3600 + INT(ENTRY(2,iLoginTime_time:SCREEN-VALUE,":")) * 60
           iTo   = INT(ENTRY(1,iLogoutTime_time:SCREEN-VALUE,":")) * 3600 + INT(ENTRY(2,iLogoutTime_time:SCREEN-VALUE,":")) * 60
           .
    IF iFrom > iTo THEN DO:
      MESSAGE "From time cannot be later than to time"
              VIEW-AS ALERT-BOX.
      RETURN.
    END.       
    oFmJBoxLoginSession:bufferExtraValues =  STRING(iFrom) + "|" + STRING(iTo).
      
  END.
  RUN SUPER.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

