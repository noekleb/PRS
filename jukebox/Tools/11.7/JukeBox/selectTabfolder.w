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
&Scoped-Define ENABLED-OBJECTS fiName rsNavQuery cmbApplyToQry ~
rsPageOneQueryType fiNavQueryLinkFields fiLabel-1 cmbLinkType-1 edDesc ~
fiLaunch-1 fiImage-1 btnImage-1 fiLabel-2 cmbLinkType-2 fiLaunch-2 ~
fiImage-2 edCode btnOk btnCancel btnImage-2 btnLaunch-1 btnLaunch-2 
&Scoped-Define DISPLAYED-OBJECTS fiName rsNavQuery cmbApplyToQry ~
rsPageOneQueryType fiNavQueryLinkFields fiLabel-1 cmbLinkType-1 edDesc ~
fiLaunch-1 fiImage-1 fiLabel-2 cmbLinkType-2 fiLaunch-2 fiImage-2 edCode 

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getImgList C-Win 
FUNCTION getImgList RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

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

DEFINE BUTTON btnImage-1 
     IMAGE-UP FILE "bmp/open16e.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Image file name" 
     SIZE 4 BY 1.

DEFINE BUTTON btnImage-2 
     IMAGE-UP FILE "bmp/open16e.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Image file name" 
     SIZE 4 BY 1.

DEFINE BUTTON btnLaunch-1 
     IMAGE-UP FILE "bmp/open16e.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Klient filnavn" 
     SIZE 4 BY 1.

DEFINE BUTTON btnLaunch-2 
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
     LABEL "Type" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "1 - 1","oneToOne",
                     "1 - N","child",
                     "Shared","Shared"
     DROP-DOWN-LIST
     SIZE 11.4 BY 1 TOOLTIP "Query relation type" NO-UNDO.

DEFINE VARIABLE cmbLinkType-2 AS CHARACTER FORMAT "X(256)":U INITIAL "child" 
     LABEL "Type" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "1 - 1","oneToOne",
                     "1 - N","child",
                     "Shared","Shared"
     DROP-DOWN-LIST
     SIZE 11.4 BY 1 TOOLTIP "Query relation type" NO-UNDO.

DEFINE VARIABLE edCode AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 85 BY 9.76 NO-UNDO.

DEFINE VARIABLE edDesc AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL LARGE
     SIZE 85 BY 9.05 NO-UNDO.

DEFINE VARIABLE fiImage-1 AS CHARACTER FORMAT "x(50)" 
     LABEL "Image, tab 1 (opt)" 
     VIEW-AS FILL-IN 
     SIZE 44.4 BY 1 TOOLTIP "Tab image (optional)" DROP-TARGET.

DEFINE VARIABLE fiImage-2 AS CHARACTER FORMAT "x(50)" 
     LABEL "Image, tab 2 (opt)" 
     VIEW-AS FILL-IN 
     SIZE 44.4 BY 1 TOOLTIP "Tab image (optional)" DROP-TARGET.

DEFINE VARIABLE fiLabel-1 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Label, pg1" 
     VIEW-AS FILL-IN 
     SIZE 30.8 BY 1 NO-UNDO.

DEFINE VARIABLE fiLabel-2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Label, pg2" 
     VIEW-AS FILL-IN 
     SIZE 30.8 BY 1 NO-UNDO.

DEFINE VARIABLE fiLaunch-1 AS CHARACTER FORMAT "x(256)" 
     LABEL "Launch, pg 1" 
     VIEW-AS FILL-IN 
     SIZE 44.4 BY 1 TOOLTIP "Tab program" DROP-TARGET.

DEFINE VARIABLE fiLaunch-2 AS CHARACTER FORMAT "x(256)" 
     LABEL "Launch, pg 2" 
     VIEW-AS FILL-IN 
     SIZE 44.4 BY 1 TOOLTIP "Tab program" DROP-TARGET.

DEFINE VARIABLE fiName AS CHARACTER FORMAT "X(256)":U INITIAL "Tab" 
     LABEL "Name" 
     VIEW-AS FILL-IN 
     SIZE 33.8 BY 1 NO-UNDO.

DEFINE VARIABLE fiNavQueryLinkFields AS CHARACTER FORMAT "X(256)":U 
     LABEL "Default query link fields" 
     VIEW-AS FILL-IN 
     SIZE 33.4 BY 1 NO-UNDO.

DEFINE VARIABLE rsNavQuery AS CHARACTER INITIAL "container" 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Navigation browse/query in (this) container", "container",
"Navigation browse/query on page one", "page1"
     SIZE 44 BY 2.14 NO-UNDO.

DEFINE VARIABLE rsPageOneQueryType AS CHARACTER INITIAL "Browse" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Browse", "Browse",
"Query", "Query"
     SIZE 22.8 BY .95 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     fiName AT ROW 1.24 COL 32.6 COLON-ALIGNED
     rsNavQuery AT ROW 1.48 COL 79 NO-LABEL WIDGET-ID 12
     cmbApplyToQry AT ROW 2.29 COL 32.6 COLON-ALIGNED WIDGET-ID 4
     rsPageOneQueryType AT ROW 2.62 COL 123.2 NO-LABEL WIDGET-ID 90
     fiNavQueryLinkFields AT ROW 3.33 COL 32.6 COLON-ALIGNED
     fiLabel-1 AT ROW 4.81 COL 17.6 COLON-ALIGNED
     cmbLinkType-1 AT ROW 4.81 COL 54.6 COLON-ALIGNED WIDGET-ID 66
     edDesc AT ROW 4.81 COL 71 NO-LABEL WIDGET-ID 16
     fiLaunch-1 AT ROW 5.86 COL 17.6 COLON-ALIGNED
     fiImage-1 AT ROW 6.91 COL 17.6 COLON-ALIGNED
     btnImage-1 AT ROW 6.91 COL 64 WIDGET-ID 58
     fiLabel-2 AT ROW 8.19 COL 17.6 COLON-ALIGNED
     cmbLinkType-2 AT ROW 8.19 COL 54.6 COLON-ALIGNED WIDGET-ID 82
     fiLaunch-2 AT ROW 9.24 COL 17.6 COLON-ALIGNED
     fiImage-2 AT ROW 10.29 COL 17.6 COLON-ALIGNED
     edCode AT ROW 14.81 COL 71 NO-LABEL WIDGET-ID 18
     btnOk AT ROW 24.81 COL 126 WIDGET-ID 6
     btnCancel AT ROW 24.81 COL 141.4 WIDGET-ID 8
     btnImage-2 AT ROW 10.29 COL 64 WIDGET-ID 68
     btnLaunch-1 AT ROW 5.86 COL 64 WIDGET-ID 60
     btnLaunch-2 AT ROW 9.24 COL 64 WIDGET-ID 70
     "Code:" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 14.05 COL 71 WIDGET-ID 22
     "Description:" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 4.1 COL 71 WIDGET-ID 20
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 156.6 BY 25.33 WIDGET-ID 100.


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
         TITLE              = "Add tab folder (Jlw type - from controls.dll)"
         HEIGHT             = 25.33
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

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Add tab folder (Jlw type - from controls.dll) */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Add tab folder (Jlw type - from controls.dll) */
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


&Scoped-define SELF-NAME btnImage-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnImage-1 C-Win
ON CHOOSE OF btnImage-1 IN FRAME DEFAULT-FRAME /* Image file name */
DO:
  DEF VAR cFileName AS CHAR NO-UNDO.
  SYSTEM-DIALOG GET-FILE cFileName 
                FILTERS "Image files" "*.ico,*.bmp,*.gif" 
                MUST-EXIST
                UPDATE bOk.
  ValidateImage(cFileName,fiImage-1:HANDLE).
  THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
  IF fiImage-1:MODIFIED THEN setTabDef().

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnImage-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnImage-2 C-Win
ON CHOOSE OF btnImage-2 IN FRAME DEFAULT-FRAME /* Image file name */
DO:
  DEF VAR cFileName AS CHAR NO-UNDO.
  SYSTEM-DIALOG GET-FILE cFileName 
                FILTERS "Image files" "*.ico,*.bmp,*.gif" 
                MUST-EXIST
                UPDATE bOk.
  ValidateImage(cFileName,fiImage-2:HANDLE).
  THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
  IF fiImage-2:MODIFIED THEN setTabDef().
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


&Scoped-define SELF-NAME btnLaunch-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLaunch-2 C-Win
ON CHOOSE OF btnLaunch-2 IN FRAME DEFAULT-FRAME /* Klient filnavn */
DO:
  DEF VAR cFileName AS CHAR NO-UNDO.
  SYSTEM-DIALOG GET-FILE cFileName 
                FILTERS "Launch files" "*.*" 
                MUST-EXIST
                UPDATE bOk.
  ValidateProgram(cFileName,fiLaunch-2:HANDLE).  
  THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().

  IF fiLaunch-2:MODIFIED THEN setTabDef().
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
ON VALUE-CHANGED OF cmbLinkType-1 IN FRAME DEFAULT-FRAME /* Type */
DO:
  setTabDef().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbLinkType-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbLinkType-2 C-Win
ON VALUE-CHANGED OF cmbLinkType-2 IN FRAME DEFAULT-FRAME /* Type */
DO:
  setTabDef().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiImage-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiImage-1 C-Win
ON DROP-FILE-NOTIFY OF fiImage-1 IN FRAME DEFAULT-FRAME /* Image, tab 1 (opt) */
DO:
  ValidateImage(SELF:GET-DROPPED-FILE(1),SELF).
  IF SELF:MODIFIED THEN setTabDef().

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiImage-1 C-Win
ON F3 OF fiImage-1 IN FRAME DEFAULT-FRAME /* Image, tab 1 (opt) */
DO:
  APPLY "choose" TO btnImage-1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiImage-1 C-Win
ON LEAVE OF fiImage-1 IN FRAME DEFAULT-FRAME /* Image, tab 1 (opt) */
DO:
  IF SELF:MODIFIED THEN setTabDef().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiImage-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiImage-2 C-Win
ON DROP-FILE-NOTIFY OF fiImage-2 IN FRAME DEFAULT-FRAME /* Image, tab 2 (opt) */
DO:
  ValidateImage(SELF:GET-DROPPED-FILE(1),SELF).
  IF SELF:MODIFIED THEN setTabDef().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiImage-2 C-Win
ON F3 OF fiImage-2 IN FRAME DEFAULT-FRAME /* Image, tab 2 (opt) */
DO:
  APPLY "choose" TO btnImage-1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiImage-2 C-Win
ON LEAVE OF fiImage-2 IN FRAME DEFAULT-FRAME /* Image, tab 2 (opt) */
DO:
  IF SELF:MODIFIED THEN setTabDef().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiLabel-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiLabel-1 C-Win
ON LEAVE OF fiLabel-1 IN FRAME DEFAULT-FRAME /* Label, pg1 */
DO:
  IF SELF:MODIFIED THEN setTabDef().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiLabel-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiLabel-2 C-Win
ON LEAVE OF fiLabel-2 IN FRAME DEFAULT-FRAME /* Label, pg2 */
DO:
  IF SELF:MODIFIED THEN setTabDef().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiLaunch-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiLaunch-1 C-Win
ON DROP-FILE-NOTIFY OF fiLaunch-1 IN FRAME DEFAULT-FRAME /* Launch, pg 1 */
DO:
  ValidateProgram(SELF:GET-DROPPED-FILE(1),SELF:HANDLE).

  IF SELF:MODIFIED THEN setTabDef().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiLaunch-1 C-Win
ON F3 OF fiLaunch-1 IN FRAME DEFAULT-FRAME /* Launch, pg 1 */
DO:
  APPLY "choose" TO btnLaunch-1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiLaunch-1 C-Win
ON LEAVE OF fiLaunch-1 IN FRAME DEFAULT-FRAME /* Launch, pg 1 */
DO:
  IF SELF:MODIFIED THEN setTabDef().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiLaunch-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiLaunch-2 C-Win
ON DROP-FILE-NOTIFY OF fiLaunch-2 IN FRAME DEFAULT-FRAME /* Launch, pg 2 */
DO:
  ValidateProgram(SELF:GET-DROPPED-FILE(1),SELF:HANDLE).
  IF SELF:MODIFIED THEN setTabDef().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiLaunch-2 C-Win
ON F3 OF fiLaunch-2 IN FRAME DEFAULT-FRAME /* Launch, pg 2 */
DO:
  APPLY "choose" TO btnLaunch-2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiLaunch-2 C-Win
ON LEAVE OF fiLaunch-2 IN FRAME DEFAULT-FRAME /* Launch, pg 2 */
DO:
  IF SELF:MODIFIED THEN setTabDef().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiName
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiName C-Win
ON LEAVE OF fiName IN FRAME DEFAULT-FRAME /* Name */
DO:
  IF SELF:MODIFIED THEN setTabDef().
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


&Scoped-define SELF-NAME rsNavQuery
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsNavQuery C-Win
ON VALUE-CHANGED OF rsNavQuery IN FRAME DEFAULT-FRAME
DO:
  IF rsNavQuery:SCREEN-VALUE = "container" THEN DO:
    cmbApplyToQry:HIDDEN = NO.
    rsPageOneQueryType:HIDDEN = YES.    
    cmbLinkType-1:HIDDEN = NO.
  END.
  ELSE DO:
    cmbApplyToQry:HIDDEN = YES.
    rsPageOneQueryType:HIDDEN = NO.    
    cmbLinkType-1:HIDDEN = YES.
  END.

  setTabDef().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rsPageOneQueryType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsPageOneQueryType C-Win
ON VALUE-CHANGED OF rsPageOneQueryType IN FRAME DEFAULT-FRAME
DO:
  setTabDef().
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
  DISPLAY fiName rsNavQuery cmbApplyToQry rsPageOneQueryType 
          fiNavQueryLinkFields fiLabel-1 cmbLinkType-1 edDesc fiLaunch-1 
          fiImage-1 fiLabel-2 cmbLinkType-2 fiLaunch-2 fiImage-2 edCode 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE fiName rsNavQuery cmbApplyToQry rsPageOneQueryType 
         fiNavQueryLinkFields fiLabel-1 cmbLinkType-1 edDesc fiLaunch-1 
         fiImage-1 btnImage-1 fiLabel-2 cmbLinkType-2 fiLaunch-2 fiImage-2 
         edCode btnOk btnCancel btnImage-2 btnLaunch-1 btnLaunch-2 
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
/*                 "The Jlw tab (James Lee Williams) is part of Windows common controls and is available through controls.dll." */
/*                 + CHR(10) + */
                'Tabs can also be added to a page so "container" is not limited to window containers.'
                + CHR(10) + "Link information can be specified for each folder and also the type of relationship: "
                + CHR(10) + "  Specify 1 - 1 if the child tab represents details for the navigation query record (child tab must have a query object)."
                + CHR(10) + "  Specify 1 - N if the child tab represents child records (child can have either browse or query)"
                + CHR(10) + "If the navigation query is placed on the first tab add the tab and then fetch and assign it's query"
                + " as parent query."
                + CHR(10) + "The same query may be shared between multiple tabs."
         .

  IF cFirstQry NE "" THEN DO:
    cmbApplyToQry:SCREEN-VALUE = cFirstQry.
    IF cFirstQry BEGINS "oBrw" OR cFirstQry BEGINS "oQry" THEN 
      fiName:SCREEN-VALUE = "Tab" + SUBSTR(cFirstQry,5).
  END. 

  APPLY "value-changed" TO rsNavQuery.

  DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,"edDesc").
  DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,620,490,0,0).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getImgList C-Win 
FUNCTION getImgList RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cImgList AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  IF fiImage-1:SCREEN-VALUE NE "" OR fiImage-2:SCREEN-VALUE NE "" THEN
    cImgList = (IF fiImage-1:SCREEN-VALUE NE "" THEN fiImage-1:SCREEN-VALUE ELSE "")
             + (IF fiImage-2:SCREEN-VALUE NE "" THEN "," + fiImage-2:SCREEN-VALUE ELSE "")
               .

  cImgList = TRIM(cImgList,",").
END.

RETURN cImgList.

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
DEF VAR cTab     AS CHAR NO-UNDO.
DEF VAR cImgList AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  cTab = "o" + fiName:SCREEN-VALUE.

  cImgList = getImgList().

  edCode:SCREEN-VALUE = "  " + cTab + " = NEW JBoxMsTabs(" + fiName:SCREEN-VALUE + ":HANDLE"
                      + (IF cmbApplyToQry:SCREEN-VALUE NE "" THEN "," + cmbApplyToQry:SCREEN-VALUE ELSE "")
                      + (IF cImgList NE "" THEN ',"' + cImgList + '"' ELSE "") + ")."
                      + CHR(10)
                      + (IF rsNavQuery:SCREEN-VALUE = "container" THEN
                           "  " + cTab + ':pageOneType = "' + cmbLinkType-1:SCREEN-VALUE + '".'
                         ELSE
                          (IF rsPageOneQueryType:SCREEN-VALUE = "browse" THEN
                             "  " + cTab + ':PARENT-BROWSE-OBJECT = ' + cTab + ":getPageBrowseObject(1)."
                           ELSE
                             "  " + cTab + ':PARENT-QUERY-OBJECT = ' + cTab + ":getPageQueryObject(1).")
                         + CHR(10)
                         + "  " + cTab + ':pageTwoType = "' + cmbLinkType-2:SCREEN-VALUE + '".'
                         )
                      + CHR(10) 
                      + (IF fiNavQueryLinkFields:SCREEN-VALUE NE "" THEN
                           CHR(10) + "  " + cTab + ':setLinkFields("' + fiNavQueryLinkFields:SCREEN-VALUE + '").'
                         ELSE "")
                      + (IF fiLaunch-1:SCREEN-VALUE NE "" THEN
                          CHR(10) + "  " + cTab + ':AddPage("' + fiLabel-1:SCREEN-VALUE + '","' + fiLaunch-1:SCREEN-VALUE + '"'
                        + (IF fiImage-1:SCREEN-VALUE NE "" THEN ',"' + ENTRY(NUM-ENTRIES(fiImage-1:SCREEN-VALUE,"\"),fiImage-1:SCREEN-VALUE,"\") + '"' ELSE "") + ")."
                         ELSE "")
                      + (IF fiLaunch-2:SCREEN-VALUE NE "" THEN
                          CHR(10) + "  " + cTab + ':AddPage("' + fiLabel-2:SCREEN-VALUE + '","' + fiLaunch-2:SCREEN-VALUE + '"'
                        + (IF fiImage-2:SCREEN-VALUE NE "" THEN ',"' + ENTRY(NUM-ENTRIES(fiImage-2:SCREEN-VALUE,"\"),fiImage-2:SCREEN-VALUE,"\") + '"' ELSE "") + ")."
                         ELSE "")
                      .

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

