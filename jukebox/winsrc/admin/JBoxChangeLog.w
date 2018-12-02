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

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEF VAR ix             AS INT NO-UNDO.
DEF VAR bOK            AS LOG NO-UNDO.

DEF VAR hBrwChangeLogHeader AS HANDLE NO-UNDO.
DEF VAR hTBchangeLog   AS HANDLE NO-UNDO.

DEF VAR hBrwChangeLog  AS HANDLE NO-UNDO.

DEF VAR hTBWin              AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnSplitBarY brwChangeLogHeader brwChangeLog ~
rectTBFaktura rectWinTB cmbCompany 
&Scoped-Define DISPLAYED-OBJECTS cmbCompany 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnSplitBarY 
     IMAGE-UP FILE "bmp/tabdown.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Button 1" 
     SIZE 158.6 BY .48.

DEFINE VARIABLE cmbCompany AS CHARACTER FORMAT "X(256)":U 
     LABEL "Company" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 41 BY 1 NO-UNDO.

DEFINE RECTANGLE brwChangeLog
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 158.6 BY 11.67.

DEFINE RECTANGLE brwChangeLogHeader
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 158 BY 11.19.

DEFINE RECTANGLE rectTBFaktura
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 29 BY 1.19.

DEFINE RECTANGLE rectWinTB
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 5 BY 1.19.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnSplitBarY AT ROW 14.1 COL 1.4
     cmbCompany AT ROW 1.19 COL 40 COLON-ALIGNED
     brwChangeLogHeader AT ROW 2.67 COL 1
     brwChangeLog AT ROW 14.81 COL 1.4
     rectTBFaktura AT ROW 1.24 COL 1.8
     rectWinTB AT ROW 1.24 COL 154
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 159 BY 25.76.


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
         TITLE              = "Change log"
         HEIGHT             = 25.76
         WIDTH              = 159
         MAX-HEIGHT         = 57.14
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 57.14
         VIRTUAL-WIDTH      = 320
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
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
ASSIGN 
       btnSplitBarY:MOVABLE IN FRAME DEFAULT-FRAME          = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Change log */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Change log */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSplitBarY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSplitBarY C-Win
ON END-MOVE OF btnSplitBarY IN FRAME DEFAULT-FRAME /* Button 1 */
DO:
  DYNAMIC-FUNCTION("setSplitBarY",THIS-PROCEDURE:CURRENT-WINDOW,btnSplitBarY:HANDLE IN FRAME {&FRAME-NAME},NO).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbCompany
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbCompany C-Win
ON VALUE-CHANGED OF cmbCompany IN FRAME DEFAULT-FRAME /* Company */
DO:
  IF cmbCompany:SCREEN-VALUE NE "0" AND cmbCompany:SCREEN-VALUE NE ? THEN
    DYNAMIC-FUNCTION("setAttribute",hBrwChangeLogHeader,"baseQuery","WHERE iJBoxCompanyId = " + cmbCompany:SCREEN-VALUE).
  ELSE
    DYNAMIC-FUNCTION("setAttribute",hBrwChangeLogHeader,"baseQuery","").

  RUN InvokeMethod(hBrwChangeLogHeader,"OpenQuery").
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
ON CLOSE OF THIS-PROCEDURE DO:
  DYNAMIC-FUNCTION("setCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
  DYNAMIC-FUNCTION("DoCleanUpObjects",THIS-PROCEDURE:CURRENT-WINDOW).
  PUBLISH "InvalidateHandle" (THIS-PROCEDURE).
  RUN disable_UI.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

{incl/wintrigg.i}

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  &IF DEFINED(UIB_is_Running) NE 0 &THEN
    RUN InitializeObject.
  &ENDIF
  
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
  DISPLAY cmbCompany 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btnSplitBarY brwChangeLogHeader brwChangeLog rectTBFaktura rectWinTB 
         cmbCompany 
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
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR hButton  AS HANDLE NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  ASSIGN cmbCompany:DELIMITER = "|"
         cmbCompany:LIST-ITEM-PAIRS = RIGHT-TRIM("|0|" +  DYNAMIC-FUNCTION("getFieldList","JBoxCompany;cCompanyName;iJBoxCompanyId","WHERE true BY cCompanyName"),"|")
         .

  hBrwChangeLogHeader =  DYNAMIC-FUNCTION("NewBrowse",         
                    brwChangeLogHeader:HANDLE IN FRAME {&FRAME-NAME},  
                    200,                          
                    "", 
                    "JBoxChangeLogHeader" 
                     + ";cChangeLogHeaderText"
                     + ";dChange"
                     + ";+ChangeTime|CHARACTER|x(8)|jb_hhmmss(iChangeTime)|Change time"
                     + ";dCreated"
                     + ";cCreatedBy"
                     + ";dUndoDate"
                     + ";+UndoTime|CHARACTER|x(8)|jb_hhmmss(iUndoTime)|Undo time"
                     + ";!iJBoxCompanyId"
                     + ";!iJBoxChangeLogHeaderId"
                   + ",JBoxCompany"
                     + ";cCompanyName|Company"
                    ,"WHERE false,FIRST JBoxCompany OF JBoxChangeLogHeader OUTER-JOIN",
                    "").

  DYNAMIC-FUNCTION("setSortString",hBrwChangeLogHeader,"dChange;desc,ChangeTime;desc").
  
  DYNAMIC-FUNCTION("NewMenuBand",hBrwChangeLogHeader
                  ,"MultiSortBrowse;Sort on multiple columns"
                  ,"").

  hTBchangeLog = DYNAMIC-FUNCTION("NewToolbar",
                            rectTBFaktura:HANDLE,
                            "File",
                            "flatview"
                          + ",undoChange;Undo change"
                           ,"maxborder").
  DYNAMIC-FUNCTION("CreateObjectLink",hBrwChangeLogHeader,hTBchangeLog).

  hTBWin = DYNAMIC-FUNCTION("NewToolbar",
                   rectWinTB:HANDLE,
                   "File",
                   "close",
                   "enable,right").

  hBrwChangeLog = DYNAMIC-FUNCTION("NewBrowse",         
                  brwChangeLog:HANDLE IN FRAME {&FRAME-NAME},  
                  200,                          
                  "", 
                  "JBoxChangeLog" 
                   + ";cChangeText"
                   + ";cChangeLogTypeId"
                   + ";cTableChanged"
                   + ";cFieldsChanged"
                   + ";cFromValues"
                   + ";cToValues"
                   + ";cEntityIdFields"
                   + ";cEntityIdValues"
                   + ";!iJBoxChangeLogHeaderId"
                   ,"WHERE false",
                  ""). 

  DYNAMIC-FUNCTION("setAttribute",hBrwChangeLog,"getRecordCount","yes").
  
  DYNAMIC-FUNCTION("CreateParentLink",hBrwChangeLog,hBrwChangeLogHeader,"iJBoxChangeLogHeaderId").
  
  DYNAMIC-FUNCTION("NewMenuBand",hBrwChangeLog
                  ,"MultiSortBrowse;Sort on multiple columns"
                  ,"").
  
  DYNAMIC-FUNCTION("setSplitBarY" , THIS-PROCEDURE:CURRENT-WINDOW, btnSplitBarY:HANDLE IN FRAME {&FRAME-NAME},NO).
  DYNAMIC-FUNCTION("setSplitBarYlimits",btnSplitBarY:HANDLE IN FRAME {&FRAME-NAME},200,150).
  DYNAMIC-FUNCTION("setFollowSplitBarY",THIS-PROCEDURE:CURRENT-WINDOW,btnSplitBarY:HANDLE IN FRAME {&FRAME-NAME},
                      STRING(brwChangeLogHeader:HANDLE)      + "," +
                      STRING(hBrwChangeLogHeader)            + "," +
                      STRING(hBrwChangeLog)       + "," +
                      STRING(brwChangeLog:HANDLE)
                      ).
  DYNAMIC-FUNCTION("setNoResizeY",  THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "brwChangeLogHeader," + hBrwChangeLogHeader:NAME).
  
  DYNAMIC-FUNCTION("setOrgWinSize", THIS-PROCEDURE:CURRENT-WINDOW,450,541,0,0).

  RUN InvokeMethod(hBrwChangeLogHeader,"OpenQuery").
END.
  
THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE undoChangeRecord C-Win 
PROCEDURE undoChangeRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
MESSAGE "Undo all changes for this header?"
         VIEW-AS ALERT-BOX QUESTION BUTTONS OK-CANCEL UPDATE bOk.
IF bOk THEN DO:
  IF DYNAMIC-FUNCTION("ProcessSelectedRows",hBrwChangeLogHeader,"jbadmin_undo_changelog.p","") THEN

END. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

