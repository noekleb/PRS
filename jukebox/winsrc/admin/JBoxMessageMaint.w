&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
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

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEF VAR bOk         AS LOG    NO-UNDO.
DEF VAR ix          AS INT    NO-UNDO.
DEF VAR hBrowse     AS HANDLE NO-UNDO.
DEF VAR hQuery      AS HANDLE NO-UNDO.
DEF VAR hToolbar    AS HANDLE NO-UNDO.
DEF VAR hFieldMap   AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS brwMessage tbMessage tbAlwaysOnTop iMsgNo ~
cLanguage cMsgCat cTitle cMessage cMsgType iMsgButtonType dCreated ~
cCreatedBy dModified cModifiedBy fiMessageLabel 
&Scoped-Define DISPLAYED-OBJECTS tbAlwaysOnTop iMsgNo cLanguage cMsgCat ~
cTitle cMessage cMsgType iMsgButtonType dCreated cCreatedBy dModified ~
cModifiedBy fiMessageLabel 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE cLanguage AS CHARACTER FORMAT "X(2)" 
     LABEL "Language" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEMS "NO","SE","EN" 
     DROP-DOWN-LIST
     SIZE 8.2 BY 1.

DEFINE VARIABLE cMsgCat AS CHARACTER FORMAT "x(12)" 
     LABEL "Category" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEMS "Standard","Application" 
     DROP-DOWN-LIST
     SIZE 21.2 BY 1 TOOLTIP "Standard,Application".

DEFINE VARIABLE cMsgType AS CHARACTER FORMAT "x(12)" 
     LABEL "Message type" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEMS "Error","Information","Question","Warning" 
     DROP-DOWN-LIST
     SIZE 20 BY 1 TOOLTIP "Error,Information,Question,Warning".

DEFINE VARIABLE iMsgButtonType AS INTEGER FORMAT "->,>>>,>>9" INITIAL 0 
     LABEL "Btn type" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "0: OK/1",0,
                     "1: OK/1-Cancel/2",1,
                     "2: Abort/3-Retry/4-Ignore/5",2,
                     "3: Yes/6-No/7-Cancel/2",3,
                     "4: Yes/6-No/7",4,
                     "5: Retry/4-Cancel/2",5,
                     "6: Cancel/2-Try Again/10-Continue/11",6,
                     "20: Notepad",20
     DROP-DOWN-LIST
     SIZE 40 BY 1.

DEFINE VARIABLE cMessage AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 70.2 BY 3.81 NO-UNDO.

DEFINE VARIABLE cCreatedBy AS CHARACTER FORMAT "X(8)" 
     LABEL "By" 
     VIEW-AS FILL-IN 
     SIZE 10.2 BY 1.

DEFINE VARIABLE cModifiedBy AS CHARACTER FORMAT "X(8)" 
     LABEL "By" 
     VIEW-AS FILL-IN 
     SIZE 10.2 BY 1.

DEFINE VARIABLE cTitle AS CHARACTER FORMAT "X(100)" 
     LABEL "Title" 
     VIEW-AS FILL-IN 
     SIZE 70.4 BY 1.

DEFINE VARIABLE dCreated AS DATE FORMAT "99/99/99" 
     LABEL "Created" 
     VIEW-AS FILL-IN 
     SIZE 14.2 BY 1.

DEFINE VARIABLE dModified AS DATE FORMAT "99/99/99" 
     LABEL "Modified" 
     VIEW-AS FILL-IN 
     SIZE 14.2 BY 1.

DEFINE VARIABLE fiMessageLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Message:" 
      VIEW-AS TEXT 
     SIZE 9.6 BY .62 NO-UNDO.

DEFINE VARIABLE iMsgNo AS INTEGER FORMAT "->>>>>9" INITIAL 0 
     LABEL "Msg number" 
     VIEW-AS FILL-IN 
     SIZE 11.8 BY 1.

DEFINE RECTANGLE brwMessage
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 86 BY 10.71.

DEFINE RECTANGLE tbMessage
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9.8 BY .91.

DEFINE VARIABLE tbAlwaysOnTop AS LOGICAL INITIAL yes 
     LABEL "Always on top" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .81 TOOLTIP "Keep clipboard conversion always on top" NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     tbAlwaysOnTop AT ROW 1.48 COL 70
     iMsgNo AT ROW 13.67 COL 15.6 COLON-ALIGNED
     cLanguage AT ROW 13.71 COL 40.8 COLON-ALIGNED
     cMsgCat AT ROW 13.71 COL 65 COLON-ALIGNED HELP
          "Standard,Application"
     cTitle AT ROW 14.81 COL 15.6 COLON-ALIGNED
     cMessage AT ROW 16 COL 17.8 NO-LABEL
     cMsgType AT ROW 19.95 COL 16 COLON-ALIGNED HELP
          "Error,Information,Question,Warning"
     iMsgButtonType AT ROW 19.95 COL 46 COLON-ALIGNED
     dCreated AT ROW 21.24 COL 16 COLON-ALIGNED
     cCreatedBy AT ROW 21.24 COL 34.6 COLON-ALIGNED
     dModified AT ROW 21.24 COL 57.2 COLON-ALIGNED
     cModifiedBy AT ROW 21.24 COL 75.6 COLON-ALIGNED
     fiMessageLabel AT ROW 16 COL 6 COLON-ALIGNED NO-LABEL
     brwMessage AT ROW 2.67 COL 2
     tbMessage AT ROW 1.38 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 88.2 BY 21.52.


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
         TITLE              = "Maintain application messages"
         HEIGHT             = 21.52
         WIDTH              = 88.2
         MAX-HEIGHT         = 24.33
         MAX-WIDTH          = 138.8
         VIRTUAL-HEIGHT     = 24.33
         VIRTUAL-WIDTH      = 138.8
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
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Maintain application messages */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Maintain application messages */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbAlwaysOnTop
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbAlwaysOnTop C-Win
ON VALUE-CHANGED OF tbAlwaysOnTop IN FRAME DEFAULT-FRAME /* Always on top */
DO:
  THIS-PROCEDURE:CURRENT-WINDOW:ALWAYS-ON-TOP = tbAlwaysOnTop:CHECKED.
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
  IF NOT DYNAMIC-FUNCTION("DoCleanUpObjects",THIS-PROCEDURE:CURRENT-WINDOW) THEN RETURN NO-APPLY.
  DYNAMIC-FUNCTION("SetCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
  PUBLISH "InvalidateHandle" (THIS-PROCEDURE).
  RUN disable_UI.
END.

{incl/wintrigg.i}

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
  DISPLAY tbAlwaysOnTop iMsgNo cLanguage cMsgCat cTitle cMessage cMsgType 
          iMsgButtonType dCreated cCreatedBy dModified cModifiedBy 
          fiMessageLabel 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE brwMessage tbMessage tbAlwaysOnTop iMsgNo cLanguage cMsgCat cTitle 
         cMessage cMsgType iMsgButtonType dCreated cCreatedBy dModified 
         cModifiedBy fiMessageLabel 
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

DYNAMIC-FUNCTION("CreateStatusBar",THIS-PROCEDURE:CURRENT-WINDOW,"",0,YES,?).

DO WITH FRAME {&FRAME-NAME}:
  hBrowse = DYNAMIC-FUNCTION("NewBrowse"
          ,BrwMessage:HANDLE
          ,1000
          ,""
          ,"JBoxMessage"
          + ";iMsgNo"
          + ";cLanguage"
          + ";cMessage"
          + ";iMsgButtonType"
          + ";cMsgType"
          + ";cMsgCat"
          + ";cTitle"
          ,"WHERE false"
          ,"").

  DYNAMIC-FUNCTION("setSortString",hBrowse,"iMsgNo").

  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap"
        ,hBrowse:QUERY
        ,FRAME {&FRAME-NAME}:HANDLE
        ,"iMsgNo,cLanguage,cMsgCat,cMsgType,iMsgButtonType,cMessage,cTitle",""
        ,"cCreatedBy,cModifiedBy,dCreated,dModified",""
        ,"").

  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hFieldMap).

  hToolbar = DYNAMIC-FUNCTION("NewToolBar"
        ,TbMessage:HANDLE
        ,"File"
        ,"new,copy,undo,delete,save,excel"
        ,"maxborder").

  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hToolbar).
  DYNAMIC-FUNCTION("CreateObjectLink",hFieldMap,hToolbar).

  RUN InvokeMethod(hBrowse,"OpenQuery").

  APPLY "value-changed" TO tbAlwaysOnTop.
END.

DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,
                 "cMessage").
DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,200,200,0,0).

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
THIS-PROCEDURE:CURRENT-WINDOW:WINDOW-STATE = 3.
THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().

APPLY "entry" TO hBrowse.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NewRecord C-Win 
PROCEDURE NewRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cLast AS CHAR NO-UNDO.

RUN SUPER.
DO WITH FRAME {&FRAME-NAME}:

  ASSIGN cLanguage:SCREEN-VALUE = "NO"
         cMsgCat:SCREEN-VALUE = "Application"
         cMsgType:SCREEN-VALUE = "Error"
         iMsgButtonType:SCREEN-VALUE = "1"
         cLast = DYNAMIC-FUNCTION("getFieldValues","LAST JBoxMessage","","iMsgNo")
         .
  IF cLast NE ? THEN
    iMsgNo:SCREEN-VALUE = STRING(INT(cLast) + 1).
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

