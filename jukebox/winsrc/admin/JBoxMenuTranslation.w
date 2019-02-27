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
DEF VAR hBrwTrans   AS HANDLE NO-UNDO.
DEF VAR hBuffTrans  AS HANDLE NO-UNDO.
DEF VAR hFieldLabel AS HANDLE NO-UNDO.
DEF VAR hFieldAccel AS HANDLE NO-UNDO.
DEF VAR hFieldToolt AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS brwMenu tbMenu brwMenuTrans cLanguage 
&Scoped-Define DISPLAYED-OBJECTS cLanguage 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ReposLeft C-Win 
FUNCTION ReposLeft RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE cLanguage AS CHARACTER FORMAT "X(256)":U 
     LABEL "Language" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE RECTANGLE brwMenu
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 96 BY 26.19.

DEFINE RECTANGLE brwMenuTrans
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 97.4 BY 26.19.

DEFINE RECTANGLE tbMenu
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 17 BY 1.19.

DEFINE BUTTON btnSplitBarX  NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 1.2 BY 26.19
     BGCOLOR 12 FGCOLOR 12 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     cLanguage AT ROW 1.24 COL 109.4 COLON-ALIGNED
     brwMenu AT ROW 2.91 COL 2
     tbMenu AT ROW 1.24 COL 3
     brwMenuTrans AT ROW 2.91 COL 100.6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 198.4 BY 28.19.

DEFINE FRAME frSplitBarX
     btnSplitBarX AT ROW 1 COL 46
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 53.6 ROW 2.91
         SIZE 94 BY 26.19.


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
         TITLE              = "Menu translation"
         HEIGHT             = 28.19
         WIDTH              = 198.4
         MAX-HEIGHT         = 36.57
         MAX-WIDTH          = 204.8
         VIRTUAL-HEIGHT     = 36.57
         VIRTUAL-WIDTH      = 204.8
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
/* REPARENT FRAME */
ASSIGN FRAME frSplitBarX:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR FRAME frSplitBarX
                                                                        */
ASSIGN 
       btnSplitBarX:MOVABLE IN FRAME frSplitBarX          = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME frSplitBarX
/* Query rebuild information for FRAME frSplitBarX
     _Query            is NOT OPENED
*/  /* FRAME frSplitBarX */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Menu translation */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Menu translation */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frSplitBarX
&Scoped-define SELF-NAME btnSplitBarX
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSplitBarX C-Win
ON END-MOVE OF btnSplitBarX IN FRAME frSplitBarX
DO:
  DYNAMIC-FUNCTION("setSplitBarX" , THIS-PROCEDURE:CURRENT-WINDOW, btnSplitBarX:HANDLE IN FRAME frSplitBarX,NO).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME cLanguage
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cLanguage C-Win
ON VALUE-CHANGED OF cLanguage IN FRAME DEFAULT-FRAME /* Language */
DO:
  RUN BuildTranslation.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BuildTranslation C-Win 
PROCEDURE BuildTranslation :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  hBuffTrans:EMPTY-TEMP-TABLE().

  IF cLanguage:SCREEN-VALUE NE ? AND cLanguage:SCREEN-VALUE NE "" THEN DO:
    IF DYNAMIC-FUNCTION("runProc","jbadmin_getmenutranslation.p",cLanguage:SCREEN-VALUE,hBuffer:TABLE-HANDLE) THEN
      DYNAMIC-FUNCTION("getRunProcReturnTable",hBuffTrans).
  END.

  RUN InvokeMethod(hBrwTrans,"OpenQuery").
  
  IF hBrwTrans:QUERY:NUM-RESULTS NE ? THEN
    APPLY "entry" TO hFieldLabel.
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
  DISPLAY cLanguage 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE brwMenu tbMenu brwMenuTrans cLanguage 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  ENABLE btnSplitBarX 
      WITH FRAME frSplitBarX IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frSplitBarX}
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
DEF VAR cLanguages AS CHAR NO-UNDO.
DEF VAR cBaseLang  AS CHAR NO-UNDO.

RUN enable_UI.

DYNAMIC-FUNCTION("CreateStatusBar",THIS-PROCEDURE:CURRENT-WINDOW,"",0,YES,?).

DO WITH FRAME {&FRAME-NAME}:

  cLanguage:DELIMITER  = "|".

  IF NUM-ENTRIES(DYNAMIC-FUNCTION("getLanguages"),"|") > 1 THEN 
    ASSIGN cLanguages = DYNAMIC-FUNCTION("getLanguages")
           cBaseLang  = DYNAMIC-FUNCTION("getBaseLanguageCode")
           cLanguages = REPLACE(cLanguages,cBaseLang + "|","")
           cLanguages = REPLACE(cLanguages,"|" + cBaseLang,"")
           cLanguage:LIST-ITEMS = cLanguages
           .
  ELSE
    cLanguage:LIST-ITEMS = "EN|SE".

  hBrowse = DYNAMIC-FUNCTION("NewBrowse"
          ,brwMenu:HANDLE
          ,100
          ,"title|Menu definition"
          ,"temp-table like JBoxMenu"
          + ";cMenuType"
          + ";cMenuLabel"
          + ";cAccelerator"
          + ";cMenuTooltip|Tooltip|x(80)"
          + ";+iNodeIndex|INTEGER|>>>9"
          + ";DISTINCT iJBoxMenuId"
          ,"WHERE false"
          ,"").
         
  ASSIGN hBuffer = hBrowse:QUERY:GET-BUFFER-HANDLE(1)
         hBrowse:GET-BROWSE-COLUMN(2):WIDTH-PIXELS = 150
         hBrowse:GET-BROWSE-COLUMN(3):WIDTH-PIXELS = 60
         hBrowse:GET-BROWSE-COLUMN(4):WIDTH-PIXELS = 170
         .

  DYNAMIC-FUNCTION("setSortString",hBrowse,"iNodeIndex").

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"baseQuery","WHERE (cMenuType = 'sub-menu' OR cMenuType = 'menu-item')").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"noColumnSort","*").

  hToolbar = DYNAMIC-FUNCTION("NewToolBar"
           ,TbMenu:HANDLE
           ,"File"
           ,"Excel"
           ,"maxborder").
  DYNAMIC-FUNCTION("CreateObjectLink",hToolbar,hBrowse).


  hBrwTrans = DYNAMIC-FUNCTION("NewBrowse"
          ,brwMenuTrans:HANDLE
          ,100
          ,"title|Translation"
          ,"temp-table like JBoxMenuTranslation"
          + ";cMenuLabel"
          + ";cAccelerator"
          + ";cMenuTooltip|Tooltip|x(80)"
          + ";+iNodeIndex|INTEGER|>>>9"
          + ";iJBoxMenuId"
          + ";+cMenuType|CHARACTER|x(10)||Menu Type"
          ,"WHERE false"
          ,"").
  
  ASSIGN hBuffTrans = hBrwTrans:QUERY:GET-BUFFER-HANDLE(1)
         hBrwTrans:GET-BROWSE-COLUMN(1):WIDTH-PIXELS = 180
         hBrwTrans:GET-BROWSE-COLUMN(2):WIDTH-PIXELS = 60
         hBrwTrans:GET-BROWSE-COLUMN(3):WIDTH-PIXELS = 210
         .

  DYNAMIC-FUNCTION("setAttribute",hBrwTrans,"baseQuery","WHERE (cMenuType = 'sub-menu' OR cMenuType = 'menu-item')").
  DYNAMIC-FUNCTION("setAttribute",hBrwTrans,"noColumnSort","*").
  DYNAMIC-FUNCTION("setSortString",hBrwTrans,"iNodeIndex").

  DYNAMIC-FUNCTION("getTempTable","jbsetup_getmenustruct.p"
                    ,"WHERE iJBoxMenuId = " + DYNAMIC-FUNCTION("getFieldValues","FIRST JBoxMenu","WHERE cMenuType = 'menu' USE-INDEX idxMenuId","iJBoxMenuId") + "|"
                    ,hBuffer).

  hFieldLabel  = DYNAMIC-FUNCTION("NewBrowseFillIn",hBrwTrans,"cMenuLabel","cMenuLabel"
                ,"","",""
                ,"").
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrwTrans,hFieldLabel,"cMenuLabel").

  hFieldAccel  = DYNAMIC-FUNCTION("NewBrowseFillIn",hBrwTrans,"cAccelerator","cAccelerator"
                ,"","",""
                ,"").
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrwTrans,hFieldAccel,"cAccelerator").

  hFieldToolt  = DYNAMIC-FUNCTION("NewBrowseFillIn",hBrwTrans,"cMenuTooltip","cMenuTooltip"
                ,"","",""
                ,"").
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrwTrans,hFieldToolt,"cMenuTooltip").

  RUN InvokeMethod(hBrowse,"OpenQuery").

  APPLY "value-changed" TO cLanguage.

  DYNAMIC-FUNCTION("setSplitBarX" , THIS-PROCEDURE:CURRENT-WINDOW, btnSplitBarX:HANDLE IN FRAME frSplitBarX,NO).
  
  DYNAMIC-FUNCTION("setFollowSplitBarX",THIS-PROCEDURE:CURRENT-WINDOW,btnSplitBarX:HANDLE IN FRAME frSplitBarX, 
                    STRING(brwMenu:HANDLE) + "," 
                  + STRING(hBrowse) + "," 
                  + STRING(cLanguage:HANDLE) + ","
                  + STRING(brwMenuTrans:HANDLE) + ","
                  + STRING(hBrwTrans)
                    ).
  
  DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,200,200,0,0).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LeaveBrowseFillIn C-Win 
PROCEDURE LeaveBrowseFillIn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR bExist        AS LOG    NO-UNDO.
DEF VAR hFillIn       AS HANDLE NO-UNDO.

hFillIn = DYNAMIC-FUNCTION("getCurrentObject").

IF hBuffTrans:AVAIL THEN DO WITH FRAME {&FRAME-NAME}:
  IF cLanguage:SCREEN-VALUE = "" OR cLanguage:SCREEN-VALUE = ? THEN DO:
    MESSAGE "You must supply a target language"
            VIEW-AS ALERT-BOX ERROR.
    RETURN.
  END.

  bExist = DYNAMIC-FUNCTION("getFieldValues","JBoxMenuTranslation",
                            "WHERE iJBoxMenuId = " + STRING(hBuffTrans:BUFFER-FIELD("iJBoxMenuId"):BUFFER-VALUE)
                          + " AND cLanguage = '" + cLanguage:SCREEN-VALUE + "'"
                           ,"iJBoxMenuId") NE ?.

  IF bExist THEN DO:
    bOk = DYNAMIC-FUNCTION("DoUpdate","JBoxMenuTranslation","",
                  "iJBoxMenuId,cLanguage",
                  STRING(hBuffTrans:BUFFER-FIELD("iJBoxMenuId"):BUFFER-VALUE) + "|" + cLanguage:SCREEN-VALUE,
                  hFillIn:NAME,
                   hFillIn:SCREEN-VALUE 
                 ,TRUE).
  END.
  ELSE DO: 
    bOk = DYNAMIC-FUNCTION("DoCreate","JBoxMenuTranslation","",
                  "iJBoxMenuId,cLanguage," + hFillIn:NAME,
                   STRING(hBuffTrans:BUFFER-FIELD("iJBoxMenuId"):BUFFER-VALUE) + "|"
                 + cLanguage:SCREEN-VALUE + "|"
                 + hFillIn:SCREEN-VALUE 
                 ,TRUE).
  END.
END.
RUN SUPER.

IF hBrowse:FOCUSED-ROW NE hBrwTrans:FOCUSED-ROW OR hBrowse:FOCUSED-ROW = hBrowse:DOWN THEN
  ReposLeft().

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RowEntry C-Win 
PROCEDURE RowEntry :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN SUPER.

IF DYNAMIC-FUNCTION("getCurrentObject") = hBrwTrans THEN 
  ReposLeft().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ReposLeft C-Win 
FUNCTION ReposLeft RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
hBrowse:SET-REPOSITIONED-ROW(hBrwTrans:FOCUSED-ROW,"Always").
hBuffer:FIND-FIRST("WHERE iJBoxMenuId = " + STRING(hBuffTrans:BUFFER-FIELD("iJBoxMenuId"):BUFFER-VALUE)) NO-ERROR.
IF hBuffer:AVAIL THEN DO:
  IF hBrowse:FOCUSED-ROW = hBrowse:DOWN THEN 
    hBrowse:DOWN.
  hBrowse:QUERY:REPOSITION-TO-ROWID(hBuffer:ROWID).
END.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

