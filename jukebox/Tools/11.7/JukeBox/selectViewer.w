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
DEF INPUT  PARAM icQryList  AS CHAR NO-UNDO.
DEF INPUT  PARAM icTbList   AS CHAR NO-UNDO.
DEF INPUT  PARAM icFmList   AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocTabName  AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocCode     AS CHAR NO-UNDO.

/* Local Variable Definitions ---                                       */

DEF VAR cFirstQry AS CHAR NO-UNDO.
DEF VAR cFirstFm  AS CHAR NO-UNDO.
DEF VAR cFirstTb  AS CHAR NO-UNDO.
DEF VAR iNumQry   AS INT  NO-UNDO.
DEF VAR iNumTb    AS INT  NO-UNDO.
DEF VAR iNumFm    AS INT  NO-UNDO.
DEF VAR ix        AS INT  NO-UNDO.
DEF VAR bOk       AS LOG  NO-UNDO.

DEF TEMP-TABLE ttProcs
    FIELD cCat    AS CHAR 
    FIELD bSelect AS LOG  LABEL "Select" 
    FIELD cName   AS CHAR LABEL "Procedure name" FORMAT "x(40)"
    FIELD cDesc   AS CHAR LABEL "Description"    FORMAT "x(30)"
    FIELD cCode   AS CHAR LABEL "Code" FORMAT "x(30)"
    .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fiName edDesc cmbApplyToQry ~
fiNavQueryLinkFields cmbLinkType-1 fiLaunch-1 edCode btnOk btnLaunch-1 ~
btnCancel 
&Scoped-Define DISPLAYED-OBJECTS fiName edDesc cmbApplyToQry ~
fiNavQueryLinkFields cmbLinkType-1 fiLaunch-1 edCode 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fillFromConfig C-Win 
FUNCTION fillFromConfig RETURNS LOGICAL
  ( INPUT icConfigFile AS CHAR,
    INPUT icCat        AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ImportConfig C-Win 
FUNCTION ImportConfig RETURNS CHARACTER
  ( INPUT icFileName AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ReplaceWithObject C-Win 
FUNCTION ReplaceWithObject RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setTabDef C-Win 
FUNCTION setTabDef RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ValidateImage C-Win 
FUNCTION ValidateImage RETURNS LOGICAL
  ( INPUT icFileName    AS CHAR,
    INPUT ihTargetField AS HANDLE)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ValidateProgram C-Win 
FUNCTION ValidateProgram RETURNS LOGICAL
  ( INPUT icFileName    AS CHAR,
    INPUT ihTargetField AS HANDLE)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCancel 
     LABEL "Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnLaunch-1 
     IMAGE-UP FILE "bmp/open16e.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Klient filnavn" 
     SIZE 4 BY 1.

DEFINE BUTTON btnOk 
     LABEL "Ok" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE cmbApplyToQry AS CHARACTER FORMAT "X(256)":U 
     LABEL "Navigation Browse/Query object" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 33.8 BY 1 NO-UNDO.

DEFINE VARIABLE cmbLinkType-1 AS CHARACTER FORMAT "X(256)":U INITIAL "oneToOne" 
     LABEL "Relation type" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "1 - 1","oneToOne",
                     "1 - N","child",
                     "Shared","Shared"
     DROP-DOWN-LIST
     SIZE 11.4 BY 1 TOOLTIP "Query relation type" NO-UNDO.

DEFINE VARIABLE edCode AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 145 BY 8.67 NO-UNDO.

DEFINE VARIABLE edDesc AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL LARGE
     SIZE 85 BY 10.62 NO-UNDO.

DEFINE VARIABLE fiLaunch-1 AS CHARACTER FORMAT "x(256)" 
     LABEL "Launch" 
     VIEW-AS FILL-IN 
     SIZE 44.4 BY 1 TOOLTIP "Tab program" DROP-TARGET.

DEFINE VARIABLE fiName AS CHARACTER FORMAT "X(256)":U INITIAL "View" 
     LABEL "Name" 
     VIEW-AS FILL-IN 
     SIZE 33.8 BY 1 NO-UNDO.

DEFINE VARIABLE fiNavQueryLinkFields AS CHARACTER FORMAT "X(256)":U 
     LABEL "Default query link fields" 
     VIEW-AS FILL-IN 
     SIZE 33.4 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     fiName AT ROW 1.24 COL 32.6 COLON-ALIGNED
     edDesc AT ROW 2.05 COL 71 NO-LABEL WIDGET-ID 16
     cmbApplyToQry AT ROW 2.29 COL 32.6 COLON-ALIGNED WIDGET-ID 4
     fiNavQueryLinkFields AT ROW 3.33 COL 32.6 COLON-ALIGNED
     cmbLinkType-1 AT ROW 4.81 COL 54.6 COLON-ALIGNED WIDGET-ID 66
     fiLaunch-1 AT ROW 5.86 COL 17.6 COLON-ALIGNED
     edCode AT ROW 13.14 COL 11 NO-LABEL WIDGET-ID 18
     btnOk AT ROW 22.05 COL 126 WIDGET-ID 6
     btnLaunch-1 AT ROW 5.86 COL 64 WIDGET-ID 60
     btnCancel AT ROW 22.05 COL 141.4 WIDGET-ID 8
     "Code:" VIEW-AS TEXT
          SIZE 6 BY .62 AT ROW 13.05 COL 4.2 WIDGET-ID 22
     "Description:" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 1.33 COL 71 WIDGET-ID 20
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 156.6 BY 22.38 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Add viewer"
         HEIGHT             = 22.38
         WIDTH              = 156.6
         MAX-HEIGHT         = 25.33
         MAX-WIDTH          = 156.6
         VIRTUAL-HEIGHT     = 25.33
         VIRTUAL-WIDTH      = 156.6
         ALWAYS-ON-TOP      = yes
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



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
ASSIGN 
       edDesc:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       fiName:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "View".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Add viewer */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Add viewer */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel C-Win
ON CHOOSE OF btnCancel IN FRAME DEFAULT-FRAME /* Cancel */
DO:
  APPLY "close" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLaunch-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLaunch-1 C-Win
ON CHOOSE OF btnLaunch-1 IN FRAME DEFAULT-FRAME /* Klient filnavn */
DO:
  DEF VAR cFileName AS CHAR NO-UNDO.
  SYSTEM-DIALOG GET-FILE cFileName 
                FILTERS "Launch files" "*.*" 
                MUST-EXIST
                UPDATE bOk.
  ValidateProgram(cFileName,fiLaunch-1:HANDLE).  
  THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
  IF fiLaunch-1:MODIFIED THEN setTabDef().

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOk C-Win
ON CHOOSE OF btnOk IN FRAME DEFAULT-FRAME /* Ok */
DO:
  ocTabName = fiName:SCREEN-VALUE.
  ocCode = edCode:SCREEN-VALUE.

  IF ocTabName = "" THEN DO:
    MESSAGE "You must assign a name"
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN NO-APPLY.
  END.

  /*
  BROWSE {&BROWSE-NAME}:QUERY:GET-FIRST().
  REPEAT WHILE NOT BROWSE {&BROWSE-NAME}:QUERY:QUERY-OFF-END:
    IF ttProcs.bSelect THEN
      ASSIGN ocProcList = ocProcList + (IF ocProcList NE "" THEN "," ELSE "") + ttProcs.cName
             ocCodeList = ocCodeList + (IF ocCodeList NE "" THEN "|" ELSE "") + ttProcs.cCode.
    BROWSE {&BROWSE-NAME}:QUERY:GET-NEXT().
  END.
  */
  APPLY "close" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbApplyToQry
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbApplyToQry C-Win
ON VALUE-CHANGED OF cmbApplyToQry IN FRAME DEFAULT-FRAME /* Navigation Browse/Query object */
DO:
  setTabDef().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbLinkType-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbLinkType-1 C-Win
ON VALUE-CHANGED OF cmbLinkType-1 IN FRAME DEFAULT-FRAME /* Relation type */
DO:
  setTabDef().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiLaunch-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiLaunch-1 C-Win
ON DROP-FILE-NOTIFY OF fiLaunch-1 IN FRAME DEFAULT-FRAME /* Launch */
DO:
  ValidateProgram(SELF:GET-DROPPED-FILE(1),SELF:HANDLE).

  IF SELF:MODIFIED THEN setTabDef().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiLaunch-1 C-Win
ON F3 OF fiLaunch-1 IN FRAME DEFAULT-FRAME /* Launch */
DO:
  APPLY "choose" TO btnLaunch-1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiLaunch-1 C-Win
ON LEAVE OF fiLaunch-1 IN FRAME DEFAULT-FRAME /* Launch */
DO:
  IF SELF:MODIFIED THEN setTabDef().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiName
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiName C-Win
ON LEAVE OF fiName IN FRAME DEFAULT-FRAME /* Name */
DO:
  IF SELF:MODIFIED THEN DO:
    setTabDef().
    ReplaceWithObject().
  END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiNavQueryLinkFields
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiNavQueryLinkFields C-Win
ON LEAVE OF fiNavQueryLinkFields IN FRAME DEFAULT-FRAME /* Default query link fields */
DO:
  IF SELF:MODIFIED THEN setTabDef().
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
  RUN disable_UI.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

{incl\wintrigg.i}

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  RUN InitWindow.
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
  DISPLAY fiName edDesc cmbApplyToQry fiNavQueryLinkFields cmbLinkType-1 
          fiLaunch-1 edCode 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE fiName edDesc cmbApplyToQry fiNavQueryLinkFields cmbLinkType-1 
         fiLaunch-1 edCode btnOk btnLaunch-1 btnCancel 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
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

DO WITH FRAME {&FRAME-NAME}:

  DYNAMIC-FUNCTION("NewObject",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE,"procedure").

  ASSIGN cFirstQry = ENTRY(1,icQryList)
         cFirstFm  = ENTRY(1,icFmList)
         cFirstTb  = ENTRY(1,icTbList)
         iNumQry   = NUM-ENTRIES(icQryList)
         iNumFm    = NUM-ENTRIES(icFmList)
         iNumTb    = NUM-ENTRIES(icTbList)
         cmbApplyToQry:LIST-ITEMS = "," + icQryList
/*          cmbApplyToFm:LIST-ITEMS = "," + icFmList */
/*          cmbApplyToTb:LIST-ITEMS = "," + icTbList */
         edDesc:SCREEN-VALUE = 
                "A viewer object facilitates embedding a suppressed window in a container (or another suppressed window (i.e frame))."
                + CHR(10) + 'Link to a (optional) navigation query may either be of type 1 - 1 or 1 - N (parent-link):'
                + CHR(10) + "  Specify 1 - 1 if the child tab represents details for the navigation query record (viewer must have a query object)."
                + CHR(10) + "  Specify 1 - N if the child tab represents child records (child can have either browse or query)" + CHR(10)
                + CHR(10) + "If there is no default link mechanism (like for the document browse), create the viewer like this:" 
                + CHR(10) + '  o<ViewerName> = NEW JBoxViewer(<ViewerName>:HANDLE,"JBoxViewDocTreeBrw.w").' + CHR(10)
                + CHR(10) + "You can also set resize rules for widgets on the embedded frame by using methods in the viewer object, f.ex:" 
                + CHR(10) + "o<ViewerName>:setNoResizeY(o<ViewerName>:CHILD-BROWSE-HANDLE:NAME" 
                + CHR(10) + '    + "," + o<ViewerName>:VIEWER-FRAME:NAME).'
                + CHR(10) + "o<ViewerName>:setAddMoveY(o<ViewerName>:VIEWER-FRAME:NAME)." + CHR(10)
                + CHR(10) + "Note! If the viewer is used in a tab-folder remember to add o<ViewerName>:moveViewerToTop(). to the MoveToTop procedure."
         .

  IF cFirstQry NE "" THEN DO:
    cmbApplyToQry:SCREEN-VALUE = cFirstQry.
    IF cFirstQry BEGINS "oBrw" OR cFirstQry BEGINS "oQry" THEN 
      fiName:SCREEN-VALUE = "View" + SUBSTR(cFirstQry,5).
    ReplaceWithObject().  
  END. 

  DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,"edDesc").
  DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,620,420,0,0).

  SESSION:SET-WAIT-STATE("").
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fillFromConfig C-Win 
FUNCTION fillFromConfig RETURNS LOGICAL
  ( INPUT icConfigFile AS CHAR,
    INPUT icCat        AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cConfig     AS CHAR NO-UNDO.
DEF VAR cLine       AS CHAR NO-UNDO.
DEF VAR ix          AS INT  NO-UNDO.

IF SEARCH(icConfigFile) NE ? THEN DO:
  cConfig = ImportConfig(SEARCH(icConfigFile)).

  IF cConfig NE "" THEN DO ix = 1 TO NUM-ENTRIES(cConfig,CHR(10)):
    cLine = TRIM(ENTRY(ix,cConfig,CHR(10))).

    IF cLine BEGINS "<name>" THEN DO:
      CREATE ttProcs.
      ASSIGN ttProcs.cName  = SUBSTR(cLine,7)
             ttProcs.cCat   = icCat
             .
    END. 
    ELSE IF cLine BEGINS "<desc>" AND AVAIL ttProcs THEN 
      ttProcs.cDesc = ttProcs.cDesc + (IF ttProcs.cDesc NE "" THEN CHR(10) ELSE "") + SUBSTR(cLine,7).
    ELSE IF cLine BEGINS "<code>" AND AVAIL ttProcs THEN 
      ttProcs.cCode = ttProcs.cCode + (IF ttProcs.cCode NE "" THEN CHR(10) ELSE "") + SUBSTR(cLine,7).
  END.
  RETURN YES.  
END.
RETURN NO.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ImportConfig C-Win 
FUNCTION ImportConfig RETURNS CHARACTER
  ( INPUT icFileName AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cInput  AS CHAR NO-UNDO.
DEF VAR cReturn AS CHAR NO-UNDO.

INPUT FROM VALUE(icFileName).

REPEAT:
  IMPORT UNFORMATTED cInput.  
  cReturn = cReturn + cInput + CHR(10).
END.
INPUT CLOSE.

RETURN cReturn.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ReplaceWithObject C-Win 
FUNCTION ReplaceWithObject RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cQryObject AS CHAR NO-UNDO.
DEF VAR cFmObject  AS CHAR NO-UNDO.
DEF VAR cTbObject  AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  IF cmbApplyToQry:SCREEN-VALUE IN FRAME {&FRAME-NAME} NE "" AND cmbApplyToQry:SCREEN-VALUE NE ? THEN 
    cQryObject = cmbApplyToQry:SCREEN-VALUE.
  ELSE
    cQryObject = cFirstQry.

/*   IF cmbApplyToFm:SCREEN-VALUE NE "" AND cmbApplyToFm:SCREEN-VALUE NE ? THEN */
/*     cFmObject = cmbApplyToFm:SCREEN-VALUE.                                   */
/*   ELSE                                                                       */
    cFmObject = cFirstFm.

/*   IF cmbApplyToTb:SCREEN-VALUE NE "" AND cmbApplyToTb:SCREEN-VALUE NE ? THEN */
/*     cTbObject = cmbApplyToTb:SCREEN-VALUE.                                   */
/*   ELSE                                                                       */
    cTbObject = cFirstTb.

  IF cQryObject NE "" THEN
    ASSIGN edDesc:SCREEN-VALUE = REPLACE(edDesc:SCREEN-VALUE,"<QueryObject>",cQryObject)
           edCode:SCREEN-VALUE = REPLACE(edCode:SCREEN-VALUE,"<QueryObject>",cQryObject)
           .
  IF cFmObject NE "" THEN
    ASSIGN edDesc:SCREEN-VALUE = REPLACE(edDesc:SCREEN-VALUE,"<FieldMapObject>",cFmObject)
           edCode:SCREEN-VALUE = REPLACE(edCode:SCREEN-VALUE,"<FieldMapObject>",cFmObject) 
           .
  IF cTbObject NE "" THEN
    ASSIGN edDesc:SCREEN-VALUE = REPLACE(edDesc:SCREEN-VALUE,"<ToolbarObject>",cTbObject)
           edCode:SCREEN-VALUE = REPLACE(edCode:SCREEN-VALUE,"<ToolbarObject>",cTbObject) 
           .
  
  IF fiName:SCREEN-VALUE NE "" THEN         
    ASSIGN edDesc:SCREEN-VALUE = REPLACE(edDesc:SCREEN-VALUE,"<ViewerName>",fiName:SCREEN-VALUE)
           edCode:SCREEN-VALUE = REPLACE(edCode:SCREEN-VALUE,"<ViewerName>",fiName:SCREEN-VALUE) 
           .

END.
  
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setTabDef C-Win 
FUNCTION setTabDef RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cView     AS CHAR NO-UNDO.
DEF VAR cImgList AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  cView = "o" + fiName:SCREEN-VALUE.


  edCode:SCREEN-VALUE = "  " + cView + " = NEW JBoxViewer(" + fiName:SCREEN-VALUE + ":HANDLE"
                      + (IF cmbApplyToQry:SCREEN-VALUE NE "" THEN 
                           "," + cmbApplyToQry:SCREEN-VALUE 
                        +  "," + '"' + cmbLinkType-1:SCREEN-VALUE + '"' 
                        +  "," + '"' + fiNavQueryLinkFields:SCREEN-VALUE + '"' 
                         ELSE "")
                      + (IF fiLaunch-1:SCREEN-VALUE NE "" THEN
                          "," + '"' + fiLaunch-1:SCREEN-VALUE + '"'
                         ELSE "")
                      + ").".  

END.
  
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ValidateImage C-Win 
FUNCTION ValidateImage RETURNS LOGICAL
  ( INPUT icFileName    AS CHAR,
    INPUT ihTargetField AS HANDLE) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF icFileName = "" THEN RETURN NO.

IF icFileName MATCHES "*bmp" OR icFileName MATCHES "*ico" OR icFileName MATCHES "*.gif"
   THEN DO WITH FRAME {&FRAME-NAME}:
  ihTargetField:SCREEN-VALUE = ENTRY(NUM-ENTRIES(icFileName,"\"),icFileName,"\").
  IF SEARCH(ihTargetField:SCREEN-VALUE) = ? THEN DO ix = 1 TO 4:
    ihTargetField:SCREEN-VALUE = ENTRY(NUM-ENTRIES(icFileName,"\") - ix,icFileName,"\") + "\" + ihTargetField:SCREEN-VALUE.
    IF SEARCH(ihTargetField:SCREEN-VALUE) NE ? THEN LEAVE.
  END.
  IF SEARCH(ihTargetField:SCREEN-VALUE) = ? THEN DO:
    MESSAGE "Invalid image: " icFileName SKIP "(must be in PROPATH)"
             VIEW-AS ALERT-BOX ERROR.
    ihTargetField:SCREEN-VALUE = "".
    RETURN NO.
  END.    

  APPLY "any-printable" TO ihTargetField.
  THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().

  RETURN YES.
END.
ELSE 
  DYNAMIC-FUNCTION("DoMessage",0,0,"Invalid image file type: Must be bmp, gif or ico","","").
  
RETURN NO.
  
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ValidateProgram C-Win 
FUNCTION ValidateProgram RETURNS LOGICAL
  ( INPUT icFileName    AS CHAR,
    INPUT ihTargetField AS HANDLE) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF icFileName = "" THEN RETURN NO.

IF icFileName MATCHES "*.w" OR icFileName MATCHES "*.p"  THEN DO WITH FRAME {&FRAME-NAME}:
  ihTargetField:SCREEN-VALUE = ENTRY(NUM-ENTRIES(icFileName,"\"),icFileName,"\").
  IF SEARCH(ihTargetField:SCREEN-VALUE) = ? THEN DO ix = 1 TO 4:
    ihTargetField:SCREEN-VALUE = ENTRY(NUM-ENTRIES(icFileName,"\") - ix,icFileName,"\") + "\" + ihTargetField:SCREEN-VALUE.
    IF SEARCH(ihTargetField:SCREEN-VALUE) NE ? THEN LEAVE.
  END.
  IF SEARCH(ihTargetField:SCREEN-VALUE) = ? THEN 
    MESSAGE "Invalid program name: " icFileName SKIP "(should exist and be in PROPATH)"
             VIEW-AS ALERT-BOX WARNING.

  APPLY "any-printable" TO ihTargetField.
  THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
END.
  
RETURN YES.
  
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

