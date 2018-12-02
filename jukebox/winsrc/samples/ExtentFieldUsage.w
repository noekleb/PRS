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
&IF "{1}" = "Developer_Studio_is_Running" &THEN
  &SCOPED-DEFINE UIB_is_Running 1 
&ENDIF   

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
/* &SCOPED-DEFINE PureABLWin 1  */

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
&Scoped-Define ENABLED-OBJECTS brwSalesrep tbSalesrep SalesRep RepName ~
Region MonthQuota_1 MonthQuota_2 MonthQuota_3 MonthQuota_4 MonthQuota_5 ~
MonthQuota_6 MonthQuota_7 MonthQuota_8 MonthQuota_9 MonthQuota_10 ~
MonthQuota_11 MonthQuota_12 
&Scoped-Define DISPLAYED-OBJECTS SalesRep RepName Region MonthQuota_1 ~
MonthQuota_2 MonthQuota_3 MonthQuota_4 MonthQuota_5 MonthQuota_6 ~
MonthQuota_7 MonthQuota_8 MonthQuota_9 MonthQuota_10 MonthQuota_11 ~
MonthQuota_12 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE Region AS CHARACTER FORMAT "x(8)" 
     LABEL "Region" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 16 BY 1 TOOLTIP "Please enter the Sales Region covered by this salesman.".

DEFINE VARIABLE MonthQuota_1 AS INTEGER FORMAT "->,>>>,>>9" INITIAL 0 
     LABEL "Month Quota" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 TOOLTIP "Please enter the Month Quota.".

DEFINE VARIABLE MonthQuota_10 AS INTEGER FORMAT "->,>>>,>>9" INITIAL 0 
     LABEL "Month Quota" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 TOOLTIP "Please enter the Month Quota.".

DEFINE VARIABLE MonthQuota_11 AS INTEGER FORMAT "->,>>>,>>9" INITIAL 0 
     LABEL "Month Quota" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 TOOLTIP "Please enter the Month Quota.".

DEFINE VARIABLE MonthQuota_12 AS INTEGER FORMAT "->,>>>,>>9" INITIAL 0 
     LABEL "Month Quota" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 TOOLTIP "Please enter the Month Quota.".

DEFINE VARIABLE MonthQuota_2 AS INTEGER FORMAT "->,>>>,>>9" INITIAL 0 
     LABEL "Month Quota" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 TOOLTIP "Please enter the Month Quota.".

DEFINE VARIABLE MonthQuota_3 AS INTEGER FORMAT "->,>>>,>>9" INITIAL 0 
     LABEL "Month Quota" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 TOOLTIP "Please enter the Month Quota.".

DEFINE VARIABLE MonthQuota_4 AS INTEGER FORMAT "->,>>>,>>9" INITIAL 0 
     LABEL "Month Quota" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 TOOLTIP "Please enter the Month Quota.".

DEFINE VARIABLE MonthQuota_5 AS INTEGER FORMAT "->,>>>,>>9" INITIAL 0 
     LABEL "Month Quota" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 TOOLTIP "Please enter the Month Quota.".

DEFINE VARIABLE MonthQuota_6 AS INTEGER FORMAT "->,>>>,>>9" INITIAL 0 
     LABEL "Month Quota" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 TOOLTIP "Please enter the Month Quota.".

DEFINE VARIABLE MonthQuota_7 AS INTEGER FORMAT "->,>>>,>>9" INITIAL 0 
     LABEL "Month Quota" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 TOOLTIP "Please enter the Month Quota.".

DEFINE VARIABLE MonthQuota_8 AS INTEGER FORMAT "->,>>>,>>9" INITIAL 0 
     LABEL "Month Quota" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 TOOLTIP "Please enter the Month Quota.".

DEFINE VARIABLE MonthQuota_9 AS INTEGER FORMAT "->,>>>,>>9" INITIAL 0 
     LABEL "Month Quota" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 TOOLTIP "Please enter the Month Quota.".

DEFINE VARIABLE RepName AS CHARACTER FORMAT "x(30)" 
     LABEL "Rep Name" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1 TOOLTIP "Please enter the Name of the Salesperson.".

DEFINE VARIABLE SalesRep AS CHARACTER FORMAT "x(4)" 
     LABEL "Sales Rep" 
     VIEW-AS FILL-IN 
     SIZE 9.6 BY 1 TOOLTIP "Please enter the Sales Rep.".

DEFINE RECTANGLE brwSalesrep
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88 BY 6.33.

DEFINE RECTANGLE tbSalesrep
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9 BY .95.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     SalesRep AT ROW 9 COL 15 COLON-ALIGNED HELP
          "Please enter the Sales Rep."
     RepName AT ROW 10 COL 15 COLON-ALIGNED HELP
          "Please enter the Name of the Salesperson."
     Region AT ROW 11 COL 15 COLON-ALIGNED HELP
          "Please enter the Sales Region covered by this salesman."
     MonthQuota_1 AT ROW 12 COL 15 COLON-ALIGNED HELP
          "Please enter the Month Quota."
     MonthQuota_2 AT ROW 13 COL 15 COLON-ALIGNED HELP
          "Please enter the Month Quota."
     MonthQuota_3 AT ROW 14 COL 15 COLON-ALIGNED HELP
          "Please enter the Month Quota."
     MonthQuota_4 AT ROW 15 COL 15 COLON-ALIGNED HELP
          "Please enter the Month Quota."
     MonthQuota_5 AT ROW 16 COL 15 COLON-ALIGNED HELP
          "Please enter the Month Quota."
     MonthQuota_6 AT ROW 17 COL 15 COLON-ALIGNED HELP
          "Please enter the Month Quota."
     MonthQuota_7 AT ROW 18 COL 15 COLON-ALIGNED HELP
          "Please enter the Month Quota."
     MonthQuota_8 AT ROW 19 COL 15 COLON-ALIGNED HELP
          "Please enter the Month Quota."
     MonthQuota_9 AT ROW 20 COL 15 COLON-ALIGNED HELP
          "Please enter the Month Quota."
     MonthQuota_10 AT ROW 21 COL 15 COLON-ALIGNED HELP
          "Please enter the Month Quota."
     MonthQuota_11 AT ROW 22 COL 15 COLON-ALIGNED HELP
          "Please enter the Month Quota."
     MonthQuota_12 AT ROW 23 COL 15 COLON-ALIGNED HELP
          "Please enter the Month Quota."
     brwSalesrep AT ROW 2.38 COL 2
     tbSalesrep AT ROW 1.29 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 91.4 BY 23.38.


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
         HEIGHT             = 23.43
         WIDTH              = 91.4
         MAX-HEIGHT         = 24.48
         MAX-WIDTH          = 167.2
         VIRTUAL-HEIGHT     = 24.48
         VIRTUAL-WIDTH      = 167.2
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
    &IF DEFINED(PureABLWin) = 1 &THEN
      IF PROVERSION GE "10.2" THEN RUN WaitForForm.
    &ENDIF
  &ENDIF

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BeforeDynAccum C-Win 
PROCEDURE BeforeDynAccum :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihBrowse AS HANDLE NO-UNDO. 
DEF INPUT PARAM icAction AS CHAR NO-UNDO.

IF ihBrowse NE hBrowse THEN RETURN.

IF icAction = "accum" THEN DO:
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"shadedRows","yes").
  hBrowse:SEPARATORS = NO.
END.
ELSE DO:
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"shadedRows","no").
  hBrowse:SEPARATORS = YES.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplayRecord C-Win 
PROCEDURE DisplayRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN SUPER.
Salesrep:READ-ONLY IN FRAME {&FRAME-NAME} = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DynAccumDone C-Win 
PROCEDURE DynAccumDone :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihBrowse AS HANDLE NO-UNDO. 
DEF INPUT PARAM icAction AS CHAR NO-UNDO.

IF ihBrowse NE hBrowse THEN RETURN.

IF icAction = "accum" THEN DO:
  DYNAMIC-FUNCTION("setAttribute",hToolbar,"disabledEvents","new,copy,delete,save,undo").
  DYNAMIC-FUNCTION("DeleteObjectLink",hBrowse,hFieldMap).
  DYNAMIC-FUNCTION("ViewHideFieldMap",hFieldMap,NO).
END.
ELSE DO:
  DYNAMIC-FUNCTION("setAttribute",hToolbar,"disabledEvents","").
  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hFieldMap).
  DYNAMIC-FUNCTION("ViewHideFieldMap",hFieldMap,YES).
END.

RUN InvokeMethod(hBrowse,"DisplayRecord").
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
  DISPLAY SalesRep RepName Region MonthQuota_1 MonthQuota_2 MonthQuota_3 
          MonthQuota_4 MonthQuota_5 MonthQuota_6 MonthQuota_7 MonthQuota_8 
          MonthQuota_9 MonthQuota_10 MonthQuota_11 MonthQuota_12 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE brwSalesrep tbSalesrep SalesRep RepName Region MonthQuota_1 
         MonthQuota_2 MonthQuota_3 MonthQuota_4 MonthQuota_5 MonthQuota_6 
         MonthQuota_7 MonthQuota_8 MonthQuota_9 MonthQuota_10 MonthQuota_11 
         MonthQuota_12 
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

  SUBSCRIBE TO "DynAccumDone" ANYWHERE.
  SUBSCRIBE TO "BeforeDynAccum" ANYWHERE.

  ASSIGN Region:DELIMITER = "|"
         Region:LIST-ITEM-PAIRS = DYNAMIC-FUNCTION("getFieldList",
                                 "Salesrep;region;distinct region",
                                 "WHERE true").

  hBrowse = DYNAMIC-FUNCTION("NewBrowse"
  ,brwSalesrep:HANDLE
  ,100
  ,"TITLE|Monthly sales quotas"
  ,"Salesrep"
   + ";Salesrep"
   + ";Repname"
   + ";Region"
   + ";MonthQuota[1]|January"
   + ";MonthQuota[2]|February"
   + ";MonthQuota[3]|March"
   + ";MonthQuota[4]|April"
   + ";MonthQuota[5]|May"
   + ";MonthQuota[6]|June"
   + ";MonthQuota[7]|July"
   + ";MonthQuota[8]|August"
   + ";MonthQuota[9]|Septemper"
   + ";MonthQuota[10]|October"
   + ";MonthQuota[11]|November"
   + ";MonthQuota[12]|December"
  ,"WHERE false"
  ,"").

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"accumDataTypes","integer,decimal").

  DYNAMIC-FUNCTION("NewMenuBand",hBrowse
      ,"MultiSortBrowse;Sort on multiple columns"
      ,"").

  hToolbar = DYNAMIC-FUNCTION("NewToolBar"
    ,tbSalesrep:HANDLE
    ,"File"
    ,"new,copy,undo,delete,save"
   + ",BrowseConfig;C&olumn setup,Filter,Accum,excel;E&xcel"
    ,"maxborder").

  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hToolbar).

  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap"
  ,hBrowse:QUERY
  ,FRAME {&FRAME-NAME}:HANDLE
  ,"SalesRep,RepName,Region,MonthQuota[1],MonthQuota[2],MonthQuota[3],MonthQuota[4],MonthQuota[5],MonthQuota[6],MonthQuota[7],MonthQuota[8],MonthQuota[9],MonthQuota[10],MonthQuota[11],MonthQuota[12]",
   "SalesRep,RepName,Region,MonthQuota_1,MonthQuota_2,MonthQuota_3,MonthQuota_4,MonthQuota_5,MonthQuota_6,MonthQuota_7,MonthQuota_8,MonthQuota_9,MonthQuota_10,MonthQuota_11,MonthQuota_12"
  ,"",""
  ,"").

  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hFieldmap).
  DYNAMIC-FUNCTION("CreateObjectLink",hToolbar,hFieldmap).

  DYNAMIC-FUNCTION("BrwOrQryToFMapTranslation",hBrowse) NO-ERROR.
END.


DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,200,100,0,0).

RUN InvokeMethod(hBrowse,"OpenQuery").

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
&IF DEFINED(PureABLWin) = 1 &THEN
  IF PROVERSION GE "10.2" THEN RUN ShowForm ("").
&ENDIF

THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = NO.
THIS-PROCEDURE:CURRENT-WINDOW:WINDOW-STATE = 3.
THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().

APPLY "entry" TO hBrowse.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

