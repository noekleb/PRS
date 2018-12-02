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
DEF VAR hBuffer     AS HANDLE NO-UNDO.

DEF TEMP-TABLE ttReport NO-UNDO
    FIELD cLine     AS CHAR
    .
DEF VAR hReportBuffer AS HANDLE NO-UNDO.
hReportBuffer = BUFFER ttReport:HANDLE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS brwReport tbReport 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE RECTANGLE brwReport
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 115 BY 18.1.

DEFINE RECTANGLE tbReport
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9.8 BY .91.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     brwReport AT ROW 2.67 COL 2 WIDGET-ID 2
     tbReport AT ROW 1.24 COL 2.4 WIDGET-ID 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 117.6 BY 20 WIDGET-ID 100.


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
         HEIGHT             = 20
         WIDTH              = 117.6
         MAX-HEIGHT         = 23.52
         MAX-WIDTH          = 117.6
         VIRTUAL-HEIGHT     = 23.52
         VIRTUAL-WIDTH      = 117.6
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
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 20
       FRAME DEFAULT-FRAME:WIDTH            = 117.6.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ActivateRecord C-Win 
PROCEDURE ActivateRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iReturn          AS INT  NO-UNDO.
DEF VAR cParamDesc       AS CHAR NO-UNDO.
DEF VAR cParamItemLabel  AS CHAR NO-UNDO.
DEF VAR cParamItemType   AS CHAR NO-UNDO.
DEF VAR cParamItemFormat AS CHAR NO-UNDO.
DEF VAR cParamItemInit   AS CHAR NO-UNDO.
DEF VAR cParamItemReturn AS CHAR NO-UNDO.
DEF VAR cParam           AS CHAR NO-UNDO.
DEF VAR cReportFileType  AS CHAR NO-UNDO.
DEF VAR hQuery           AS HANDLE NO-UNDO.
DEF VAR cFileName        AS CHAR   NO-UNDO.
DEF VAR cProgName        AS CHAR   NO-UNDO.

IF hBuffer:BUFFER-FIELD("cMisc1"):BUFFER-VALUE NE "" THEN DO ix = 1 TO NUM-ENTRIES(hBuffer:BUFFER-FIELD("cMisc1"):BUFFER-VALUE,";"):
  ASSIGN cParamDesc  = ENTRY(ix,hBuffer:BUFFER-FIELD("cMisc1"):BUFFER-VALUE,";")
         cParamItemLabel  = ENTRY(1,cParamDesc,"|")
         cParamItemType   = "CHARACTER"
         cParamItemFormat = ""
         cParamItemInit   = ""
         .

  IF NUM-ENTRIES(cParamDesc,"|") > 1 THEN
    cParamItemType = ENTRY(2,cParamDesc,"|").
  ELSE
    cParamItemFormat = "x(40)".

  IF NUM-ENTRIES(cParamDesc,"|") > 2 THEN
    cParamItemFormat = ENTRY(3,cParamDesc,"|").

  IF NUM-ENTRIES(cParamDesc,"|") > 3 THEN
    cParamItemInit = ENTRY(4,cParamDesc,"|").

  IF NUM-ENTRIES(hBuffer:BUFFER-FIELD("LastParam"):BUFFER-VALUE,"|") GE ix THEN
    cParamItemReturn = ENTRY(ix,hBuffer:BUFFER-FIELD("LastParam"):BUFFER-VALUE,"|").

  RUN JBoxAskForValue.w (cParamItemLabel,cParamItemType + "|" + cParamItemFormat + (IF cParamItemInit NE "" THEN "|" + cParamItemInit ELSE ""),
                         INPUT-OUTPUT cParamItemReturn,OUTPUT iReturn).
  IF iReturn = 2 THEN 
    cParam = cParam + (IF ix > 1 THEN "|" ELSE "") + cParamItemReturn.  
  ELSE RETURN.
END.
ELSE DO:
  iReturn = DYNAMIC-FUNCTION("DoMessage",0,1,"Start " + hBuffer:BUFFER-FIELD("cDescription"):BUFFER-VALUE + "?","","").
  IF iReturn = 2 THEN iReturn = 0.
END.

IF iReturn > 0 THEN DO:
  hBuffer:BUFFER-FIELD("LastParam"):BUFFER-VALUE = cParam.
  hReportBuffer:EMPTY-TEMP-TABLE().

  cProgName = REPLACE(hBuffer:BUFFER-FIELD("cCodeValue"):BUFFER-VALUE,"\","/").

  IF NOT DYNAMIC-FUNCTION("getMyTempTable","jbserv_run_adhoc_report.p",cProgName + ";" + cParam,hReportBuffer) THEN
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
  ELSE DO:
    cReportFileType = DYNAMIC-FUNCTION("getTransactionMessage").
    cFileName = SESSION:TEMP-DIR + ENTRY(NUM-ENTRIES(cProgName,"/"),cProgName,"/") + "_" + STRING(YEAR(TODAY)) + "_" + STRING(MONTH(TODAY),"99") + "_" + STRING(DAY(TODAY),"99") + "." + cReportFileType.

    FIND FIRST ttReport NO-ERROR.

    CREATE QUERY hQuery.
    hQuery:SET-BUFFERS(hReportBuffer).
    hQuery:QUERY-PREPARE("FOR EACH " + hReportBuffer:NAME).
    hQuery:QUERY-OPEN().
    hQuery:GET-FIRST().
    OUTPUT TO VALUE(cFileName).
    REPEAT WHILE NOT hQuery:QUERY-OFF-END:
      PUT UNFORMATTED ttReport.cLine SKIP.
      hQuery:GET-NEXT().
    END.
    OUTPUT CLOSE.

    DELETE OBJECT hQuery.

    DYNAMIC-FUNCTION("setWebDoc","",cFileName).
  END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  ENABLE brwReport tbReport 
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
DEFINE VAR bScand AS LOG NO-UNDO.

RUN enable_UI.

DYNAMIC-FUNCTION("CreateStatusBar",THIS-PROCEDURE:CURRENT-WINDOW,"",0,YES,?).

bScand = DYNAMIC-FUNCTION("Scandinavian").

DO WITH FRAME {&FRAME-NAME}:
  hBrowse = DYNAMIC-FUNCTION("NewBrowse"
          ,brwReport:HANDLE
          ,100
          ,""
          ,"JBoxGenCode"
           + ";cDescription|" + (IF bScand THEN "Rapport" ELSE "Report")
           + ";cCodeValue|" + (IF bScand THEN "Programnavn" ELSE "Program name")
           + ";cMisc1|Parameter"
           + ";!+LastParam|CHARACTER|x(40)||Order Total"
          ,"WHERE false"
          ,"").

   hBuffer = hBrowse:QUERY:GET-BUFFER-HANDLE(1).

   hToolbar = DYNAMIC-FUNCTION("NewToolBar"
         ,tbReport:HANDLE
         ,"Fil" + (IF NOT bScand THEN "e" ELSE "")
         ,"activate"
         ,"maxborder").

  DYNAMIC-FUNCTION("CreateObjectLink",hToolbar,hBrowse).

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"baseQuery","WHERE cCodeType = 'ad-hoc-reports'(+Company)").

  RUN InvokeMethod(hBrowse,"OpenQuery").
END.


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

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

