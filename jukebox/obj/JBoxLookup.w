&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
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

&SCOPED-DEFINE PureABLWin 0

/* Parameters Definitions ---                                           */
&IF DEFINED(UIB_is_Running) NE 0 &THEN
  DEF VAR ihParent             AS HANDLE NO-UNDO.

  DEF VAR iiBatchSize          AS INT    NO-UNDO INIT 200.

  DEF VAR icSourceTableAndFlds AS CHAR   NO-UNDO INIT "vare;ean;varetekst".
  DEF VAR icSourceQuery        AS CHAR   NO-UNDO INIT "where true".

  DEF VAR icBrowseProperties   AS CHAR   NO-UNDO.
  DEF VAR icOutputFields       AS CHAR   NO-UNDO.
  DEF VAR ocOutputValues       AS CHAR   NO-UNDO.

  DEF VAR obOK                 AS LOG    NO-UNDO.

&ELSE
  DEF INPUT PARAM ihParent               AS HANDLE NO-UNDO.

  DEF INPUT PARAM iiBatchSize            AS INT  NO-UNDO.
  DEF INPUT PARAM icSourceTableAndFlds   AS CHAR NO-UNDO.
  DEF INPUT PARAM icSourceQuery          AS CHAR NO-UNDO.
  DEF INPUT PARAM icBrowseProperties     AS CHAR NO-UNDO.
  DEF INPUT PARAM icOutputFields         AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocOutputValues        AS CHAR NO-UNDO.
  DEF OUTPUT PARAM obOK                  AS LOG NO-UNDO.
&ENDIF

/* Local Variable Definitions ---                                       */

DEF VAR ix                 AS INT    NO-UNDO.
DEF VAR bOK                AS LOG    NO-UNDO.

DEF VAR cSelRowidList      AS CHAR NO-UNDO.
DEF VAR cDeSelRowidList    AS CHAR NO-UNDO.

DEF VAR hBrwSource         AS HANDLE NO-UNDO.
DEF VAR hBrwTarget         AS HANDLE NO-UNDO.
DEF VAR hBuffSource        AS HANDLE NO-UNDO.
DEF VAR hBuffTarget        AS HANDLE NO-UNDO.
DEF VAR hSearchField AS HANDLE NO-UNDO.

DEF VAR httTarget          AS HANDLE NO-UNDO.
DEF VAR cSortColumn        AS CHAR   NO-UNDO.
DEF VAR bDesc              AS LOG    NO-UNDO.
DEF VAR hFilToolBar        AS HANDLE NO-UNDO.
DEF VAR hWinToolBar        AS HANDLE NO-UNDO.

DEF VAR iSelectIndex       AS INT NO-UNDO.
DEF VAR iDeSelectIndex     AS INT NO-UNDO.
DEF VAR oLookup            AS JBoxDynLookup NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rectBrwSource rectSearchSource ~
rectWinToolBar Btn_OK Btn_Cancel 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getOutputData C-Win 
FUNCTION getOutputData RETURNS CHARACTER
  ( INPUT icOutputFields AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getToolbarRectangle C-Win 
FUNCTION getToolbarRectangle RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LocalTranslation C-Win 
FUNCTION LocalTranslation RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "&Avbryt" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE RECTANGLE rectBrwSource
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 102.2 BY 13.57.

DEFINE RECTANGLE rectSearchSource
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 22 BY .95.

DEFINE RECTANGLE rectToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9 BY 1.19.

DEFINE RECTANGLE rectWinToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9 BY 1.19.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     Btn_OK AT ROW 17.24 COL 73.2
     Btn_Cancel AT ROW 17.24 COL 89.2
     rectBrwSource AT ROW 3.62 COL 1.8
     rectSearchSource AT ROW 2.48 COL 2
     rectWinToolBar AT ROW 1.19 COL 95.2
     rectToolBar AT ROW 1.19 COL 1.8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 104.6 BY 17.62
         DEFAULT-BUTTON Btn_OK.


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
         TITLE              = "Velg"
         HEIGHT             = 17.52
         WIDTH              = 104.6
         MAX-HEIGHT         = 57.14
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 57.14
         VIRTUAL-WIDTH      = 320
         MIN-BUTTON         = no
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

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-ICON("ico/dict%.ico":U) THEN
    MESSAGE "Unable to load icon: ico/dict%.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
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
/* SETTINGS FOR RECTANGLE rectToolBar IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       rectToolBar:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Velg */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Velg */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Velg */
DO:
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"resize","").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel C-Win
ON CHOOSE OF Btn_Cancel IN FRAME DEFAULT-FRAME /* Avbryt */
DO:
  APPLY "close" TO THIS-PROCEDURE.

  IF VALID-HANDLE(ihParent:CURRENT-WINDOW) THEN
    ihParent:CURRENT-WINDOW:MOVE-TO-TOP().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK C-Win
ON ANY-PRINTABLE OF Btn_OK IN FRAME DEFAULT-FRAME /* OK */
DO:
  IF VALID-HANDLE(hSearchField) THEN DO:
    APPLY "entry" TO hSearchField.
    APPLY LASTKEY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK C-Win
ON BACK-TAB OF Btn_OK IN FRAME DEFAULT-FRAME /* OK */
DO:
  APPLY "entry" TO hBrwSource.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK C-Win
ON CHOOSE OF Btn_OK IN FRAME DEFAULT-FRAME /* OK */
DO:
  DEF VAR iReturn   AS INT NO-UNDO INIT 6.

  IF hBrwSource:NUM-ITERATIONS = 0 THEN DO:
    IF VALID-HANDLE(hSearchField) THEN
      APPLY "entry" TO hSearchField.
    RETURN NO-APPLY.
  END.

  IF VALID-HANDLE(ihParent) AND CAN-DO(ihParent:INTERNAL-ENTRIES,"getLookupAttributes") THEN
    RUN getLookupAttributes IN ihParent(hBrwSource,OUTPUT iReturn).

  IF iReturn = 2 THEN RETURN NO-APPLY.

  IF icOutputFields NE "" THEN
    ocOutputValues = getOutputData(icOutputFields).

  ASSIGN cSelRowidList   = TRIM(cSelRowidList,",")
         cDeSelRowidList = TRIM(cDeSelRowidList,",")
         obOK            = TRUE
         .

  APPLY "close" TO THIS-PROCEDURE.

  IF VALID-HANDLE(ihParent:CURRENT-WINDOW) THEN
    ihParent:CURRENT-WINDOW:MOVE-TO-TOP().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK C-Win
ON CTRL-CURSOR-RIGHT OF Btn_OK IN FRAME DEFAULT-FRAME /* OK */
DO:
  APPLY "ctrl-cursor-right" TO hBrwSource.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK C-Win
ON CTRL-SHIFT-CURSOR-RIGHT OF Btn_OK IN FRAME DEFAULT-FRAME /* OK */
DO:
  APPLY "shift-ctrl-cursor-right" TO hBrwSource.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK C-Win
ON CURSOR-DOWN OF Btn_OK IN FRAME DEFAULT-FRAME /* OK */
DO:
  APPLY "entry" TO hBrwSource.
  APPLY "cursor-down".
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK C-Win
ON CURSOR-UP OF Btn_OK IN FRAME DEFAULT-FRAME /* OK */
DO:
  APPLY "entry" TO hBrwSource.
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
  DYNAMIC-FUNCTION("DoCleanUpObjects",THIS-PROCEDURE:CURRENT-WINDOW).
  RUN disable_UI.
  IF VALID-HANDLE(ihParent:CURRENT-WINDOW) THEN 
    ihParent:CURRENT-WINDOW:MOVE-TO-TOP().
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:


  IF VALID-HANDLE(ihParent) THEN
    SUBSCRIBE TO "ExpandSearchDialog" IN ihParent.
  ELSE
    SUBSCRIBE TO "ExpandSearchDialog" ANYWHERE.

  IF NOT VALID-HANDLE(ihParent) THEN ihParent = SOURCE-PROCEDURE.

  DYNAMIC-FUNCTION("DoLockWindow",THIS-PROCEDURE:CURRENT-WINDOW).

  RUN enable_UI.

  RUN InitWindow.

  IF NOT VALID-HANDLE(hBrwSource) AND VALID-OBJECT(oLookup) THEN
    hBrwSource = oLookup:BROWSE-OBJECT:BROWSE-HANDLE.

  THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.

  DYNAMIC-FUNCTION("DoLockWindow",?).

  RUN MoveToTop.
  
  JBoxSession:Instance:setDelayedFocus(THIS-PROCEDURE:CURRENT-WINDOW,hBrwSource).

  ON 'default-action':U OF hBrwSource
  DO:
    APPLY "choose" TO btn_ok IN FRAME {&FRAME-NAME}.
    RETURN.
  END.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BrowseConfigRecord C-Win 
PROCEDURE BrowseConfigRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DYNAMIC-FUNCTION("setCurrentObject",hBrwSource).
DYNAMIC-FUNCTION("setCurrentSourceProc",ihParent).
RUN SUPER.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeleteRecord C-Win 
PROCEDURE DeleteRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

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
  ENABLE rectBrwSource rectSearchSource rectWinToolBar Btn_OK Btn_Cancel 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExpandSearchDialog C-Win 
PROCEDURE ExpandSearchDialog :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihBrowse AS HANDLE NO-UNDO.
DEF INPUT PARAM icDir    AS CHAR   NO-UNDO.
DEF INPUT PARAM iiDelta  AS INT    NO-UNDO.

IF ihBrowse NE hBrwSource THEN RETURN.


IF icDir = "x" THEN 
  {&WINDOW-NAME}:WIDTH-PIXELS = {&WINDOW-NAME}:WIDTH-PIXELS + iiDelta.
ELSE
  {&WINDOW-NAME}:HEIGHT-PIXELS = {&WINDOW-NAME}:HEIGHT-PIXELS + iiDelta.

APPLY "WINDOW-RESIZED" TO {&WINDOW-NAME}.

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
DEF VAR cInitSourceSort    AS CHAR NO-UNDO.
DEF VAR cJoin              AS CHAR NO-UNDO.
DEF VAR cBaseQuerySource   AS CHAR NO-UNDO.
DEF VAR cBaseQueryTarget   AS CHAR NO-UNDO.
DEF VAR cNoColumnSearch    AS CHAR NO-UNDO.
DEF VAR cUserColumns       AS CHAR NO-UNDO.
DEF VAR cCompanyTag        AS CHAR NO-UNDO.
DEF VAR iy                 AS INT  NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:

  DO ix = 1 TO NUM-ENTRIES(icSourceTableAndFlds):
    DO iy = 2 TO NUM-ENTRIES(ENTRY(ix,icSourceTableAndFlds),";"):
      IF ENTRY(iy,ENTRY(ix,icSourceTableAndFlds),";") MATCHES "*@1" THEN DO:
        ASSIGN cInitSourceSort = "SORT|" + ENTRY(1,ENTRY(1,ENTRY(iy,ENTRY(ix,icSourceTableAndFlds),";"),"|"),"@")
               cSortColumn     = ENTRY(1,ENTRY(1,ENTRY(iy,ENTRY(ix,icSourceTableAndFlds),";"),"|"),"@")
               .
        LEAVE.
      END.
    END.
  END.

  IF cInitSourceSort = "" AND NUM-ENTRIES(ENTRY(1,icSourceTableAndFlds),";") > 1 AND NOT ENTRY(2,ENTRY(1,icSourceTableAndFlds),";") BEGINS "!" THEN
    ASSIGN cInitSourceSort = "SORT|" + ENTRY(2,ENTRY(1,icSourceTableAndFlds),";")
           cSortColumn     = ENTRY(1,ENTRY(2,ENTRY(1,icSourceTableAndFlds),";"),"|")
           .
  ELSE IF cInitSourceSort = "" THEN
    rectSearchSource:HIDDEN = TRUE.

  IF icSourceQuery MATCHES "*(*Company)" OR icSourceQuery MATCHES "*(Codemaster)" THEN 
    ASSIGN cCompanyTag = SUBSTR(icSourceQuery,R-INDEX(icSourceQuery,"("))
           icSourceQuery = SUBSTR(icSourceQuery,1,R-INDEX(icSourceQuery,"(") - 1)
           .


  /* Define the browsers: */
  IF NUM-ENTRIES(icSourceQuery) > 1 THEN DO:
    IF INDEX(icSourceQuery,"can-do") > 0 AND 
       INDEX(icSourceQuery,"(") < INDEX(icSourceQuery,",") THEN
      ASSIGN cBaseQuerySource = SUBSTR(icSourceQuery,1,INDEX(icSourceQuery,",",INDEX(icSourceQuery,")")) - 1)
             cJoin            = IF INDEX(icSourceQuery,",",INDEX(icSourceQuery,")")) > 0 THEN 
                                  SUBSTR(icSourceQuery,INDEX(icSourceQuery,",",INDEX(icSourceQuery,")")))
                                ELSE ""
                                .
    ELSE
      ASSIGN cBaseQuerySource = ENTRY(1,icSourceQuery)
             cJoin            = SUBSTR(icSourceQuery,INDEX(icSourceQuery,","))
             .
  END.
  ELSE cBaseQuerySource = icSourceQuery.

  IF NUM-ENTRIES(icSourceTableAndFlds) > 1 THEN
    icSourceTableAndFlds = ENTRY(1,icSourceTableAndFlds) + ";+!iJBoxSelectIndex|INTEGER;+!iJBoxDeSelectIndex|INTEGER" + SUBSTR(icSourceTableAndFlds,INDEX(icSourceTableAndFlds,",")).
  ELSE 
    icSourceTableAndFlds = icSourceTableAndFlds + ";+!iJBoxSelectIndex|INTEGER;+!iJBoxDeSelectIndex|INTEGER".

/*  DYNAMIC-FUNCTION("setObjectSourceProc",ihParent).*/
  DYNAMIC-FUNCTION("setAttribute",rectBrwSource:HANDLE,"usersettingcontext",SUBSTR(icSourceTableAndFlds,1,50)).

  cUserColumns =DYNAMIC-FUNCTION("getUserSetting",
                                   ihParent:FILE-NAME,
                                   "rectBrwSource",
                                   DYNAMIC-FUNCTION("getAttribute",rectBrwSource:HANDLE,"usersettingcontext"),
                                   "currviewfields").
  IF cUserColumns NE "" THEN cInitSourceSort = ENTRY(1,cUserColumns).

  hBrwSource = DYNAMIC-FUNCTION("NewBrowse",  
                    rectBrwSource:HANDLE,
                    iiBatchSize,
                    icBrowseProperties,
                    icSourceTableAndFlds,
                    cBaseQuerySource + cJoin + cCompanyTag,
                    cInitSourceSort
                    ).    
  ASSIGN hBrwSource:NAME = ENTRY(1,ENTRY(1,icSourceTableAndFlds),";")
         hBuffSource = hBrwSource:QUERY:GET-BUFFER-HANDLE(1)
         .

/*   IF INDEX(icSourceTableAndFlds,",") > 0 THEN DO:                                                                          */
/*     cNoColumnSearch = REPLACE(REPLACE(SUBSTR(icSourceTableAndFlds,INDEX(icSourceTableAndFlds,",") + 1),";",","),"|",",").  */
/*     DYNAMIC-FUNCTION("setAttribute",hBrwSource,"nocolumnsearch",cNoColumnSearch).                                          */
/*   END.                                                                                                                     */

  IF NOT cBaseQuerySource BEGINS "where false" THEN
    DYNAMIC-FUNCTION("setAttribute",hBrwSource,"basequery",cBaseQuerySource).
  DYNAMIC-FUNCTION("setAttribute",hBrwSource,"querywhere","").

  IF cInitSourceSort NE "" THEN DO:
    hSearchField = DYNAMIC-FUNCTION("NewBrowseSearchField",rectSearchSource:HANDLE,hBrwSource,1).
    DYNAMIC-FUNCTION("CreateObjectLink",hSearchField,hBrwSource).
  END.

  DYNAMIC-FUNCTION("NewMenuBand",
                    hBrwSource,  /* parent widget */
                    "MultiSortBrowse;Sorter på flere kolonner"
                  + ",Excel;Eksporter til Excel"
                   ,"").     


  hWinToolBar = DYNAMIC-FUNCTION("NewToolBar",
                    rectWinToolBar:HANDLE,            
                    IF DYNAMIC-FUNCTION("Scandinavian") THEN "Fil" ELSE "File"
/*                     ,"|"                                                                           */
/*                       + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Innstillinger" ELSE "Settings") */
/*                     + ",Close|"                                                                    */
                    ,"Close|"
                      + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Fil;Avslutt" ELSE "File;Exit") + "¤menu"
                    ,"right,enable,hide"). 


  DYNAMIC-FUNCTION("setNoResizeX",THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:HANDLE,"rectSearchSource").
  DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:HANDLE,"rectSearchSource,rectWinToolBar").
  DYNAMIC-FUNCTION("setOrgWinSize", THIS-PROCEDURE:CURRENT-WINDOW,100,100,0,0).

  LocalTranslation().

  DYNAMIC-FUNCTION("initTranslation",THIS-PROCEDURE:CURRENT-WINDOW).

  DYNAMIC-FUNCTION("SetToolbar",hWinToolBar,"enable").

  IF VALID-HANDLE(ihParent) AND CAN-DO(ihParent:INTERNAL-ENTRIES,"myLookupObject") THEN DO:
    oLookup = NEW JBoxDynLookup(ihParent,THIS-PROCEDURE,hBrwSource,rectToolbar:HANDLE).
    RUN myLookupObject IN ihParent(oLookup).
  END.  

  ELSE IF VALID-HANDLE(ihParent) AND CAN-DO(ihParent:INTERNAL-ENTRIES,"setLookupAttributes") THEN
    RUN setLookupAttributes IN ihParent(hBrwSource,rectToolbar:HANDLE,hSearchField).

  APPLY "value-changed" TO hBrwSource.

  APPLY "entry" TO hBrwSource.
  
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
THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
THIS-PROCEDURE:CURRENT-WINDOW:WINDOW-STATE = 3.
IF hBrwSource:NUM-ITERATIONS > 0 THEN APPLY "entry" TO hBrwSource.
ELSE IF VALID-HANDLE(hSearchField) THEN APPLY "entry" TO hSearchField.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TabFromBrowse C-Win 
PROCEDURE TabFromBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DYNAMIC-FUNCTION("setWidgetEnter",Btn_OK:HANDLE IN FRAME {&FRAME-NAME}).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getOutputData C-Win 
FUNCTION getOutputData RETURNS CHARACTER
  ( INPUT icOutputFields AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cValueList AS CHAR NO-UNDO.
DEF VAR iy         AS INT NO-UNDO.

DO iy = 1 TO hBrwSource:NUM-SELECTED-ROWS:
  IF hBrwSource:FETCH-SELECTED-ROW(iy) THEN
    DO ix = 1 TO NUM-ENTRIES(icOutputFields):
      cValueList = cValueList + (IF hBuffSource:BUFFER-FIELD(ENTRY(ix,icOutputFields)):BUFFER-VALUE NE ? THEN 
                                  STRING(hBuffSource:BUFFER-FIELD(ENTRY(ix,icOutputFields)):BUFFER-VALUE) 
                                 ELSE "") + "|".
    END.
END.

RETURN SUBSTR(cValueList,1,LENGTH(cValueList) - 1). 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getToolbarRectangle C-Win 
FUNCTION getToolbarRectangle RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN rectToolbar:HANDLE IN FRAME {&FRAME-NAME}.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LocalTranslation C-Win 
FUNCTION LocalTranslation RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose: Set english labels
    Notes:  
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  IF DYNAMIC-FUNCTION("Scandinavian") THEN RETURN FALSE.
  ELSE 
    ASSIGN Btn_Cancel:LABEL     = "Cancel" 
           {&WINDOW-NAME}:TITLE = "Select"
           .
  
  RETURN TRUE.  
END.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

