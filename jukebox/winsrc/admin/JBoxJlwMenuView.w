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
&SCOPED-DEFINE PureABLWin 1

DEF VAR bOk                AS LOG  NO-UNDO.
DEF VAR ix                 AS INT  NO-UNDO.
DEF VAR iReturn            AS INT  NO-UNDO.
                          
DEF VAR hFieldMap          AS HANDLE NO-UNDO.
DEF VAR hParent            AS HANDLE NO-UNDO.
DEF VAR hToolbar           AS HANDLE NO-UNDO.

DEF VAR hQuery             AS HANDLE NO-UNDO.
DEF VAR bCompanyMenuInst   AS LOG    NO-UNDO.
DEF VAR bAppModuleItemInst AS LOG    NO-UNDO.
DEF VAR bUserMenuInst      AS LOG    NO-UNDO.
DEF VAR hPrintPreview      AS HANDLE NO-UNDO.
DEF VAR bWriteAccessComp   AS LOG    NO-UNDO.
DEF VAR bWriteAccessUser   AS LOG    NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fiNodeIndex btnGetNextMenuNum cMenuType ~
iJBoxMenuId cMenuLabel iSeq bConfigurable bDefault iTvNavBarStyle cLaunch ~
cLaunchtype bMultiple cParameter cImage iImageSize cSelImage iSelImageSize ~
cStateImage iStateImageSize cMenuTooltip cAccelerator cMenuNumber ~
cFontStyle cTextColor UserLinks bLimitToSuperUsers CompanyLinks ~
bLimitToCompanyAdmins btnStateImage btnLaunch btnSelImage btnImage ~
fiUserLinksLabel fiCompanyLinksLabel ModuleLinks fiModuleLinksLabel ~
bLogExec tbMenu 
&Scoped-Define DISPLAYED-OBJECTS fiNodeIndex cMenuType iJBoxMenuId ~
cMenuLabel iSeq bConfigurable bDefault iTvNavBarStyle cLaunch cLaunchtype ~
bMultiple cParameter cImage iImageSize cSelImage iSelImageSize cStateImage ~
iStateImageSize cMenuTooltip cAccelerator cMenuNumber cFontStyle cTextColor ~
UserLinks bLimitToSuperUsers CompanyLinks bLimitToCompanyAdmins ~
fiUserLinksLabel fiCompanyLinksLabel ModuleLinks fiModuleLinksLabel ~
bLogExec 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFrameHandle C-Win 
FUNCTION getFrameHandle RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InitializeResize C-Win 
FUNCTION InitializeResize RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setEnabledFields C-Win 
FUNCTION setEnabledFields RETURNS LOGICAL
  ( INPUT icMenuType AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setParent C-Win 
FUNCTION setParent RETURNS LOGICAL
  ( INPUT ihParent AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setQuery C-Win 
FUNCTION setQuery RETURNS LOGICAL
  ( INPUT ihQuery AS HANDLE )  FORWARD.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ViewCompanyLinks C-Win 
FUNCTION ViewCompanyLinks RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ViewModuleLinks C-Win 
FUNCTION ViewModuleLinks RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ViewUserLinks C-Win 
FUNCTION ViewUserLinks RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnGetNextMenuNum 
     IMAGE-UP FILE "gif/ws_sorting.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 4 BY 1 TOOLTIP "Numeric: Next number in number sequence, else current largest string".

DEFINE BUTTON btnImage 
     IMAGE-UP FILE "bmp/open16e.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Image file name" 
     SIZE 4 BY 1.

DEFINE BUTTON btnLaunch 
     IMAGE-UP FILE "bmp/open16e.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Klient filnavn" 
     SIZE 4 BY 1.

DEFINE BUTTON btnSelImage 
     IMAGE-UP FILE "bmp/open16e.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Selected image file name" 
     SIZE 4 BY 1.

DEFINE BUTTON btnStateImage 
     IMAGE-UP FILE "bmp/open16e.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "State image file name" 
     SIZE 4 BY 1.

DEFINE VARIABLE cLaunchtype AS CHARACTER FORMAT "X(256)":U INITIAL "START-WINDOW" 
     LABEL "Launch type" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEMS "PROCEDURE","START-WINDOW","THIS-PROCEDURE","DATA-BROWSE","OS-COMMAND","URL" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE cTextColor AS CHARACTER FORMAT "X(256)":U 
     LABEL "Text color" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEMS "","AQUA","BLACK","BLUE","CREAM","DARKGRAY","FUCHSIA","GRAY","GREEN","LIMEGREEN","LIGHTGRAY","MAROON","MEDIUMGRAY","MINTGREEN","NAVYBLUE","OLIVE","PURPLE","RED","SILVER","SKYBLUE","TEAL","WHITE","YELLOW" 
     DROP-DOWN-LIST
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE iTvNavBarStyle AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Tv/Nav-bar" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Nav-bar",1,
                     "TreeView",2
     DROP-DOWN-LIST
     SIZE 18.8 BY 1 NO-UNDO.

DEFINE VARIABLE CompanyLinks AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 57 BY 2.19 NO-UNDO.

DEFINE VARIABLE ModuleLinks AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 57 BY 2.19 NO-UNDO.

DEFINE VARIABLE UserLinks AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 57 BY 2.19 NO-UNDO.

DEFINE VARIABLE cAccelerator AS CHARACTER FORMAT "x(8)" 
     LABEL "Accelerator" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1.

DEFINE VARIABLE cFontStyle AS CHARACTER FORMAT "x(30)" 
     LABEL "Font style" 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1.

DEFINE VARIABLE cImage AS CHARACTER FORMAT "x(30)" 
     LABEL "Image" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1 TOOLTIP "For nav-bar node: Default nav-bar image" DROP-TARGET.

DEFINE VARIABLE cLaunch AS CHARACTER FORMAT "x(256)" 
     LABEL "Launch" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1 TOOLTIP "MENU-ITEM: Program file or internal procedure to run. MENU: Menu program or URL" DROP-TARGET.

DEFINE VARIABLE cMenuLabel AS CHARACTER FORMAT "x(40)" 
     LABEL "Menu label" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1.

DEFINE VARIABLE cMenuNumber AS CHARACTER FORMAT "xxx" 
     LABEL "Menu num" 
     VIEW-AS FILL-IN 
     SIZE 8.6 BY 1 TOOLTIP "Menu-items: Set to enable go-to menu function".

DEFINE VARIABLE cMenuTooltip AS CHARACTER FORMAT "x(80)" 
     LABEL "Tooltip" 
     VIEW-AS FILL-IN 
     SIZE 56 BY 1.

DEFINE VARIABLE cMenuType AS CHARACTER FORMAT "x(12)" 
     LABEL "Type" 
     VIEW-AS FILL-IN 
     SIZE 14.6 BY 1.

DEFINE VARIABLE cParameter AS CHARACTER FORMAT "x(60)" 
     LABEL "Parameter" 
     VIEW-AS FILL-IN 
     SIZE 56 BY 1.

DEFINE VARIABLE cSelImage AS CHARACTER FORMAT "x(30)" 
     LABEL "Image selected" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1 TOOLTIP "For nav-bar: Default node image" DROP-TARGET.

DEFINE VARIABLE cStateImage AS CHARACTER FORMAT "x(30)" 
     LABEL "Ribbon png image" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1 DROP-TARGET.

DEFINE VARIABLE fiCompanyLinksLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Company links:" 
      VIEW-AS TEXT 
     SIZE 14.8 BY .62 NO-UNDO.

DEFINE VARIABLE fiModuleLinksLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Module links:" 
      VIEW-AS TEXT 
     SIZE 13 BY .62 NO-UNDO.

DEFINE VARIABLE fiNodeIndex AS CHARACTER FORMAT "X(256)":U 
     LABEL "Idx" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE fiUserLinksLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Group/User links:" 
      VIEW-AS TEXT 
     SIZE 16.8 BY .62 NO-UNDO.

DEFINE VARIABLE iImageSize AS INTEGER FORMAT "->,>>>,>>9" INITIAL 0 
     LABEL "Size" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1.

DEFINE VARIABLE iJBoxMenuId AS INTEGER FORMAT "->>>>>9" INITIAL 0 
     LABEL "Id" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1.

DEFINE VARIABLE iSelImageSize AS INTEGER FORMAT "->,>>>,>>9" INITIAL 0 
     LABEL "Size" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1.

DEFINE VARIABLE iSeq AS INTEGER FORMAT "->>9":U INITIAL 0 
     LABEL "Sequence in group / sub-menu" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 TOOLTIP "Display sequence" NO-UNDO.

DEFINE VARIABLE iStateImageSize AS INTEGER FORMAT "->,>>>,>>9" INITIAL 0 
     LABEL "Size" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1.

DEFINE RECTANGLE tbMenu
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 12.2 BY .95.

DEFINE VARIABLE bConfigurable AS LOGICAL INITIAL no 
     LABEL "Start maximized" 
     VIEW-AS TOGGLE-BOX
     SIZE 18.6 BY .81 NO-UNDO.

DEFINE VARIABLE bDefault AS LOGICAL INITIAL no 
     LABEL "Default" 
     VIEW-AS TOGGLE-BOX
     SIZE 12.4 BY 1 TOOLTIP "If no other restriction apply, select this menu".

DEFINE VARIABLE bLimitToCompanyAdmins AS LOGICAL INITIAL no 
     LABEL "Limit to company admins" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY .76.

DEFINE VARIABLE bLimitToSuperUsers AS LOGICAL INITIAL no 
     LABEL "Limit to super users" 
     VIEW-AS TOGGLE-BOX
     SIZE 27 BY .81.

DEFINE VARIABLE bLogExec AS LOGICAL INITIAL no 
     LABEL "Log execution" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .81.

DEFINE VARIABLE bMultiple AS LOGICAL INITIAL no 
     LABEL "Allow multiple instances" 
     VIEW-AS TOGGLE-BOX
     SIZE 28.4 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     fiNodeIndex AT ROW 1.19 COL 94.8 COLON-ALIGNED
     btnGetNextMenuNum AT ROW 12.29 COL 88
     cMenuType AT ROW 1.19 COL 95 COLON-ALIGNED
     iJBoxMenuId AT ROW 2.48 COL 102.6 COLON-ALIGNED
     cMenuLabel AT ROW 2.48 COL 54 COLON-ALIGNED
     iSeq AT ROW 3.52 COL 54 COLON-ALIGNED
     bConfigurable AT ROW 3.67 COL 66.6
     bDefault AT ROW 3.57 COL 87.6 HELP
          "If no other restriction apply, select this menu"
     iTvNavBarStyle AT ROW 3.57 COL 91.8 COLON-ALIGNED
     cLaunch AT ROW 4.62 COL 54 COLON-ALIGNED
     cLaunchtype AT ROW 5.71 COL 54 COLON-ALIGNED
     bMultiple AT ROW 5.81 COL 82.6
     cParameter AT ROW 6.76 COL 54 COLON-ALIGNED
     cImage AT ROW 7.95 COL 54 COLON-ALIGNED
     iImageSize AT ROW 7.95 COL 98 COLON-ALIGNED
     cSelImage AT ROW 9 COL 54 COLON-ALIGNED
     iSelImageSize AT ROW 9 COL 98 COLON-ALIGNED
     cStateImage AT ROW 10.1 COL 54 COLON-ALIGNED
     iStateImageSize AT ROW 10.1 COL 98 COLON-ALIGNED
     cMenuTooltip AT ROW 11.14 COL 54 COLON-ALIGNED
     cAccelerator AT ROW 12.29 COL 54 COLON-ALIGNED
     cMenuNumber AT ROW 12.29 COL 77.4 COLON-ALIGNED
     cFontStyle AT ROW 13.33 COL 54 COLON-ALIGNED
     cTextColor AT ROW 13.33 COL 89 COLON-ALIGNED
     UserLinks AT ROW 16.1 COL 56 NO-LABEL
     bLimitToSuperUsers AT ROW 14.62 COL 56
     CompanyLinks AT ROW 19.1 COL 56 NO-LABEL
     bLimitToCompanyAdmins AT ROW 14.62 COL 84
     btnStateImage AT ROW 10.1 COL 88.2
     btnLaunch AT ROW 4.62 COL 98.2
     btnSelImage AT ROW 9 COL 88.2
     btnImage AT ROW 7.95 COL 88.2
     fiUserLinksLabel AT ROW 15.48 COL 54 COLON-ALIGNED NO-LABEL
     fiCompanyLinksLabel AT ROW 18.48 COL 54 COLON-ALIGNED NO-LABEL
     ModuleLinks AT ROW 21.91 COL 56 NO-LABEL
     fiModuleLinksLabel AT ROW 21.29 COL 54 COLON-ALIGNED NO-LABEL
     bLogExec AT ROW 3.67 COL 88.4 WIDGET-ID 4
     tbMenu AT ROW 1.14 COL 1.6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.05
         SCROLLABLE SIZE 114.8 BY 23.19.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* SUPPRESS Window definition (used by the UIB) 
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Customer"
         HEIGHT             = 23.19
         WIDTH              = 114.8
         MAX-HEIGHT         = 57.14
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 57.14
         VIRTUAL-WIDTH      = 320
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
                                                                        */
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME
ASSIGN C-Win = CURRENT-WINDOW.


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{incl/devMode.i}
{incl/CustDevMode.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME Custom                                                    */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 23.19
       FRAME DEFAULT-FRAME:WIDTH            = 114.8.

ASSIGN 
       bLimitToCompanyAdmins:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       bLimitToSuperUsers:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       CompanyLinks:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE
       CompanyLinks:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE
       CompanyLinks:RESIZABLE IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       fiCompanyLinksLabel:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       fiModuleLinksLabel:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       fiNodeIndex:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       fiUserLinksLabel:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       ModuleLinks:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE
       ModuleLinks:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE
       ModuleLinks:RESIZABLE IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       UserLinks:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE
       UserLinks:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE
       UserLinks:RESIZABLE IN FRAME DEFAULT-FRAME        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Customer */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Customer */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bConfigurable
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bConfigurable C-Win
ON BACK-TAB OF bConfigurable IN FRAME DEFAULT-FRAME /* Start maximized */
DO:
  IF iSeq:SENSITIVE THEN DO:
    APPLY "entry" TO iSeq.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnGetNextMenuNum
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnGetNextMenuNum C-Win
ON CHOOSE OF btnGetNextMenuNum IN FRAME DEFAULT-FRAME /* ... */
DO:
  DEF VAR cNextNum AS CHAR NO-UNDO.
  cNextNum = DYNAMIC-FUNCTION("getNextMenuNum" IN hParent).
  IF cNextNum NE "" THEN DO:
    cMenuNumber:SCREEN-VALUE = cNextNum.
    APPLY "ANY-PRINTABLE" TO cMenuNumber.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnImage
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnImage C-Win
ON CHOOSE OF btnImage IN FRAME DEFAULT-FRAME /* Image file name */
DO:
  DEF VAR cFileName AS CHAR NO-UNDO.
  SYSTEM-DIALOG GET-FILE cFileName 
                FILTERS "Image files" "*.ico,*.bmp,*.png" 
                MUST-EXIST
                UPDATE bOk.
  ValidateImage(cFileName,cImage:HANDLE).
  THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLaunch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLaunch C-Win
ON CHOOSE OF btnLaunch IN FRAME DEFAULT-FRAME /* Klient filnavn */
DO:
  DEF VAR cFileName AS CHAR NO-UNDO.
  SYSTEM-DIALOG GET-FILE cFileName 
                FILTERS "Launch files" "*.*" 
                MUST-EXIST
                UPDATE bOk.
  ValidateProgram(cFileName,cLaunch:HANDLE).  
  THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSelImage
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSelImage C-Win
ON CHOOSE OF btnSelImage IN FRAME DEFAULT-FRAME /* Selected image file name */
DO:
  DEF VAR cFileName AS CHAR NO-UNDO.
  SYSTEM-DIALOG GET-FILE cFileName 
                FILTERS "Ico files" "*.ico","Bmp files" "*.bmp"
                MUST-EXIST
                UPDATE bOk.
  ValidateImage(cFileName,cSelImage:HANDLE).
  THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnStateImage
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnStateImage C-Win
ON CHOOSE OF btnStateImage IN FRAME DEFAULT-FRAME /* State image file name */
DO:
  DEF VAR cFileName AS CHAR NO-UNDO.
  SYSTEM-DIALOG GET-FILE cFileName 
                FILTERS "Ico files" "*.ico","Bmp files" "*.bmp"
                MUST-EXIST
                UPDATE bOk.
  ValidateImage(cFileName,cStateImage:HANDLE).
  THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cImage
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cImage C-Win
ON BACK-TAB OF cImage IN FRAME DEFAULT-FRAME /* Image */
DO:
  IF iSeq:SENSITIVE AND NOT cLaunch:SENSITIVE AND bConfigurable:HIDDEN THEN DO:
    APPLY "entry" TO iSeq.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cImage C-Win
ON DROP-FILE-NOTIFY OF cImage IN FRAME DEFAULT-FRAME /* Image */
DO:
  ValidateImage(cImage:GET-DROPPED-FILE(1),cImage:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cImage C-Win
ON F3 OF cImage IN FRAME DEFAULT-FRAME /* Image */
DO:
  APPLY "choose" TO btnImage.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cLaunch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cLaunch C-Win
ON DROP-FILE-NOTIFY OF cLaunch IN FRAME DEFAULT-FRAME /* Launch */
DO:
  ValidateProgram(cLaunch:GET-DROPPED-FILE(1),cLaunch:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cLaunch C-Win
ON F3 OF cLaunch IN FRAME DEFAULT-FRAME /* Launch */
DO:
  APPLY "choose" TO btnLaunch.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cMenuLabel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cMenuLabel C-Win
ON TAB OF cMenuLabel IN FRAME DEFAULT-FRAME /* Menu label */
DO:
  IF iSeq:SENSITIVE THEN DO:
    APPLY "entry" TO iSeq.
    RETURN NO-APPLY.
  END.
  ELSE IF NOT bConfigurable:HIDDEN AND bConfigurable:SENSITIVE THEN DO:
    APPLY "entry" TO bConfigurable.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CompanyLinks
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CompanyLinks C-Win
ON END-MOVE OF CompanyLinks IN FRAME DEFAULT-FRAME
DO:
  ASSIGN fiCompanyLinksLabel:X = CompanyLinks:X 
         fiCompanyLinksLabel:Y = CompanyLinks:Y - 13
         .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CompanyLinks C-Win
ON END-RESIZE OF CompanyLinks IN FRAME DEFAULT-FRAME
DO:
  ASSIGN fiCompanyLinksLabel:X = CompanyLinks:X 
         fiCompanyLinksLabel:Y = CompanyLinks:Y - 13
         .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cSelImage
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cSelImage C-Win
ON DROP-FILE-NOTIFY OF cSelImage IN FRAME DEFAULT-FRAME /* Image selected */
DO:
  ValidateImage(cSelImage:GET-DROPPED-FILE(1),cSelImage:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cSelImage C-Win
ON F3 OF cSelImage IN FRAME DEFAULT-FRAME /* Image selected */
DO:
  APPLY "choose" TO btnSelImage.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cStateImage
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cStateImage C-Win
ON DROP-FILE-NOTIFY OF cStateImage IN FRAME DEFAULT-FRAME /* Ribbon png image */
DO:
  ValidateImage(cStateImage:GET-DROPPED-FILE(1),cStateImage:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cStateImage C-Win
ON F3 OF cStateImage IN FRAME DEFAULT-FRAME /* Ribbon png image */
DO:
  APPLY "choose" TO btnStateImage.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME iSeq
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL iSeq C-Win
ON BACK-TAB OF iSeq IN FRAME DEFAULT-FRAME /* Sequence in group / sub-menu */
DO:
  APPLY "entry" TO cMenuLabel.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL iSeq C-Win
ON TAB OF iSeq IN FRAME DEFAULT-FRAME /* Sequence in group / sub-menu */
DO:
  IF bConfigurable:SENSITIVE THEN DO:
    APPLY "entry" TO bConfigurable.
    RETURN NO-APPLY.
  END.
  ELSE IF cLaunch:SENSITIVE THEN DO:
    APPLY "entry" TO cLaunch.
    RETURN NO-APPLY.
  END.
  ELSE DO:
    APPLY "entry" TO cImage.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME iTvNavBarStyle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL iTvNavBarStyle C-Win
ON VALUE-CHANGED OF iTvNavBarStyle IN FRAME DEFAULT-FRAME /* Tv/Nav-bar */
DO:
  setEnabledFields(cMenuType:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ModuleLinks
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ModuleLinks C-Win
ON END-MOVE OF ModuleLinks IN FRAME DEFAULT-FRAME
DO:
  ASSIGN fiModuleLinksLabel:X = ModuleLinks:X 
         fiModuleLinksLabel:Y = ModuleLinks:Y - 13
         .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ModuleLinks C-Win
ON END-RESIZE OF ModuleLinks IN FRAME DEFAULT-FRAME
DO:
  ASSIGN fiModuleLinksLabel:X = ModuleLinks:X 
         fiModuleLinksLabel:Y = ModuleLinks:Y - 13
         .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME UserLinks
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL UserLinks C-Win
ON END-MOVE OF UserLinks IN FRAME DEFAULT-FRAME
DO:
  ASSIGN fiUserLinksLabel:X = UserLinks:X 
         fiUserLinksLabel:Y = UserLinks:Y - 13
         .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL UserLinks C-Win
ON END-RESIZE OF UserLinks IN FRAME DEFAULT-FRAME
DO:
  ASSIGN fiUserLinksLabel:X = UserLinks:X 
         fiUserLinksLabel:Y = UserLinks:Y - 13
         .
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
  PUBLISH "InvalidateHandle".
  IF VALID-HANDLE(hPrintPreview) THEN
    APPLY "close" TO hPrintPreview.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  RUN enable_UI.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

{incl/supptrigg.i hQuery}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeleteLink C-Win 
PROCEDURE DeleteLink :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF NOT DYNAMIC-FUNCTION("getActionPermission",THIS-PROCEDURE:FILE-NAME,"","delete") THEN DO:
  DYNAMIC-FUNCTION("DoMessage",0,0,"Sorry, you don't have permission to do this operation","",""). 
  RETURN.  
END.

IF DYNAMIC-FUNCTION("getRecordCount","JBoxMenuToMenu","WHERE JBoxMenuToMenu.iFromMenuId = " + STRING(hFieldMap:BUFFER-FIELD("iJBoxMenuId"):BUFFER-VALUE)) = 1 THEN DO:
  DYNAMIC-FUNCTION("DoMessage",0,0,"Cannot delete last link to parent menu node","","").
  RETURN.
END.
IF hFieldMap:AVAIL AND DYNAMIC-FUNCTION("DoMessage",0,1,"Delete link to parent?","","") = 1 THEN DO:
  IF NOT DYNAMIC-FUNCTION("DoDelete","JBoxMenuToMenu","",
                          "iToMenuId,iFromMenuId",
                          DYNAMIC-FUNCTION("getParentId" IN hParent) + "|" + STRING(hFieldMap:BUFFER-FIELD("iJBoxMenuId"):BUFFER-VALUE),
                          YES) THEN DO:
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
    RETURN.
  END.
END.
ELSE RETURN.

DYNAMIC-FUNCTION("setSelectNode" IN hParent,DYNAMIC-FUNCTION("getParentNodeIndex" IN hParent)).
RUN FillTreeView IN hParent.

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
DEF VAR cNodeCount AS CHAR NO-UNDO.

IF NOT DYNAMIC-FUNCTION("getActionPermission",THIS-PROCEDURE:FILE-NAME,"","delete") THEN DO:
  DYNAMIC-FUNCTION("DoMessage",0,0,"Sorry, you don't have permission to do this operation","",""). 
  RETURN.  
END.

IF DYNAMIC-FUNCTION("runProc","jbsetup_menunodecount.p",STRING(hFieldMap:BUFFER-FIELD("iJBoxMenuId"):BUFFER-VALUE),?) THEN
  cNodeCount = DYNAMIC-FUNCTION("getTransactionMessage").
ELSE DO:
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
  RETURN.
END.

IF DYNAMIC-FUNCTION("DoMessage",0,1,"Delete menu (branch - total of " + cNodeCount + " unique nodes)?"
                                  + (IF INT(cNodeCount) > 5 THEN "??" ELSE "")
                   ,"","") = 1 THEN DO:

  IF INT(cNodeCount) > 5 AND 
     DYNAMIC-FUNCTION("DoMessage",0,1,
                      "Please confirm that you want to delete a branch containing " + cNodeCount + " unique nodes",
                      "CONFIRM","") = 2 THEN RETURN. 

  IF NOT DYNAMIC-FUNCTION("runproc","jbsetup_delmenustruct.p",STRING(hFieldMap:BUFFER-FIELD("iJBoxMenuId"):BUFFER-VALUE),?) THEN DO:
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
    RETURN.
  END.
END.
ELSE RETURN.

DYNAMIC-FUNCTION("setSelectNode" IN hParent,DYNAMIC-FUNCTION("getParentNodeIndex" IN hParent)).
RUN FillTreeView IN hParent.
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
  /* Hide all frames. */
  HIDE FRAME DEFAULT-FRAME.
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
IF hFieldMap:AVAIL THEN DO WITH FRAME {&FRAME-NAME}:
  IF hFieldMap:BUFFER-FIELD("cMenuType"):BUFFER-VALUE = "menu" THEN
    cLaunchType:LIST-ITEMS = "PLAIN,BUTTONPANEL,TREEVIEW,NAV-BAR,RIBBON".
  ELSE
    cLaunchType:LIST-ITEMS = "PROCEDURE,START-WINDOW,THIS-PROCEDURE,DATA-BROWSE,URL".

  setEnabledFields(hFieldMap:BUFFER-FIELD("cMenuType"):BUFFER-VALUE).
END.

RUN SUPER.

IF hFieldMap:AVAIL THEN DO:
  IF DYNAMIC-FUNCTION("getActionPermission",THIS-PROCEDURE:FILE-NAME,"","save") THEN
    setEnabledFields(hFieldMap:BUFFER-FIELD("cMenuType"):BUFFER-VALUE).
  RUN ValueChangedField("cLaunchType").
END.
ELSE 
  setEnabledFields("none").

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
  DISPLAY fiNodeIndex cMenuType iJBoxMenuId cMenuLabel iSeq bConfigurable 
          bDefault iTvNavBarStyle cLaunch cLaunchtype bMultiple cParameter 
          cImage iImageSize cSelImage iSelImageSize cStateImage iStateImageSize 
          cMenuTooltip cAccelerator cMenuNumber cFontStyle cTextColor UserLinks 
          bLimitToSuperUsers CompanyLinks bLimitToCompanyAdmins fiUserLinksLabel 
          fiCompanyLinksLabel ModuleLinks fiModuleLinksLabel bLogExec 
      WITH FRAME DEFAULT-FRAME.
  ENABLE fiNodeIndex btnGetNextMenuNum cMenuType iJBoxMenuId cMenuLabel iSeq 
         bConfigurable bDefault iTvNavBarStyle cLaunch cLaunchtype bMultiple 
         cParameter cImage iImageSize cSelImage iSelImageSize cStateImage 
         iStateImageSize cMenuTooltip cAccelerator cMenuNumber cFontStyle 
         cTextColor UserLinks bLimitToSuperUsers CompanyLinks 
         bLimitToCompanyAdmins btnStateImage btnLaunch btnSelImage btnImage 
         fiUserLinksLabel fiCompanyLinksLabel ModuleLinks fiModuleLinksLabel 
         bLogExec tbMenu 
      WITH FRAME DEFAULT-FRAME.
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
DEF VAR hParameter      AS HANDLE NO-UNDO.
DEF VAR hMultiple       AS HANDLE NO-UNDO.
DEF VAR hLimitCompAdmin AS HANDLE NO-UNDO.
DEF VAR hLimitSuperUser AS HANDLE NO-UNDO.
DEF VAR hDefaultMenu    AS HANDLE NO-UNDO.
DEF VAR hLogExec        AS HANDLE NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:

  DYNAMIC-FUNCTION("NewObject",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE,"procedure").

  ASSIGN bUserMenuInst              = DYNAMIC-FUNCTION("getTempTable","","JBoxUserMenu|where false",?) NE ?
         bCompanyMenuInst           = DYNAMIC-FUNCTION("getTempTable","","JBoxCompanyMenu|where false",?) NE ?
         bAppModuleItemInst         = DYNAMIC-FUNCTION("getTempTable","","JBoxAppModuleItem|where false",?) NE ?
         fiNodeIndex:HIDDEN         = YES
         CompanyLinks:HIDDEN        = NOT bCompanyMenuInst
         fiCompanyLinksLabel:HIDDEN = NOT bCompanyMenuInst
         UserLinks:HIDDEN           = NOT bUserMenuInst
         fiUserLinksLabel:HIDDEN    = NOT bUserMenuInst
         ModuleLinks:HIDDEN         = NOT bAppModuleItemInst
         fiModuleLinksLabel:HIDDEN  = NOT bAppModuleItemInst
         bWriteAccessComp  = DYNAMIC-FUNCTION("IsFieldNameInTable","JBoxCompanyMenu","bWriteAccess")
         bWriteAccessUser  = DYNAMIC-FUNCTION("IsFieldNameInTable","JBoxUserMenu","bWriteAccess")
         .


  IF NOT UserLinks:HIDDEN THEN
    DYNAMIC-FUNCTION("NewMenuBand",UserLinks:HANDLE
                    ,"MoveUserLinks;Move"
                    ,"").
  IF NOT CompanyLinks:HIDDEN THEN DO:
    DYNAMIC-FUNCTION("NewMenuBand",CompanyLinks:HANDLE
                    ,"MoveCompanyLinks;Move"
                    ,"").
    IF DYNAMIC-FUNCTION("getAttribute",SESSION,"CompanyIsRole") = "yes" THEN
      fiCompanyLinksLabel:SCREEN-VALUE = "Role links:".
  END.
  IF NOT ModuleLinks:HIDDEN THEN
    DYNAMIC-FUNCTION("NewMenuBand",ModuleLinks:HANDLE
                    ,"MoveModuleLinks;Move"
                    ,"").

  iSeq:MOVE-AFTER-TAB-ITEM(cMenuLabel:HANDLE).

  hQuery = DYNAMIC-FUNCTION("NewQuery"
          ,1
          ,""
          ,"JBoxMenu"
         + ",JBoxMenuToMenu"
          ,"WHERE false,FIRST JBoxMenuToMenu NO-LOCK WHERE iFromMenuId = iJBoxMenuId OUTER-JOIN"
          ,"").
  DYNAMIC-FUNCTION("createObjectLink",hQuery,THIS-PROCEDURE).

  hParameter      = hQuery:GET-BUFFER-HANDLE(1):BUFFER-FIELD("cParameter") NO-ERROR.
  hMultiple       = hQuery:GET-BUFFER-HANDLE(1):BUFFER-FIELD("bMultiple") NO-ERROR.
  hLimitCompAdmin = hQuery:GET-BUFFER-HANDLE(1):BUFFER-FIELD("bLimitToCompanyAdmins") NO-ERROR.
  hLimitSuperUser = hQuery:GET-BUFFER-HANDLE(1):BUFFER-FIELD("bLimitToSuperUsers") NO-ERROR.
  hDefaultMenu    = hQuery:GET-BUFFER-HANDLE(1):BUFFER-FIELD("bDefault") NO-ERROR.
  hLogExec        = hQuery:GET-BUFFER-HANDLE(1):BUFFER-FIELD("bLogExec") NO-ERROR.

  IF NOT VALID-HANDLE(hLimitSuperUser) THEN bLimitToSuperUsers:HIDDEN = YES.
  IF NOT VALID-HANDLE(hLimitCompAdmin) THEN bLimitToCompanyAdmins:HIDDEN = YES.
  IF NOT VALID-HANDLE(hDefaultMenu)    THEN bDefault:HIDDEN = YES.

  /* A FieldMap describes the relationship between the retrieved record (in browse or query) and the screen input and disp. fields */
  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap",
                             hQuery,
                             FRAME {&FRAME-NAME}:HANDLE,
                             "cMenuLabel,cAccelerator,cLaunch,cLaunchType,iSeq,bConfigurable,cImage,cSelImage,cStateImage,cMenuTooltip,cTextColor,cMenuNumber,cFontStyle,iTvNavBarStyle"
                           + (IF VALID-HANDLE(hParameter)      THEN ",cParameter" ELSE "")
                           + (IF VALID-HANDLE(hMultiple)       THEN ",bMultiple" ELSE "")
                           + (IF VALID-HANDLE(hLimitCompAdmin) THEN ",bLimitToCompanyAdmins" ELSE "")
                           + (IF VALID-HANDLE(hLimitSuperUser) THEN ",bLimitToSuperUsers" ELSE "")
                           + (IF VALID-HANDLE(hDefaultMenu)    THEN ",bDefault" ELSE "")
                           + (IF VALID-HANDLE(hLogExec)        THEN ",bLogExec" ELSE "")
                             ,"", 
                             "cMenuType,iJBoxMenuId,iImageSize,iSelImageSize,iStateImageSize,iSeq","",
                             "btnLaunch,btnImage,btnSelImage,btnStateImage,btnGetNextMenuNum").          

  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"bufferextrafields","cMenuType,iImageSize,iSelImageSize,iStateImageSize").
  DYNAMIC-FUNCTION("CreateObjectLink",hQuery,hFieldMap).
  DYNAMIC-FUNCTION("createObjectLink",hFieldMap,THIS-PROCEDURE).

  hToolbar = DYNAMIC-FUNCTION("NewToolbar",
                             tbMenu:HANDLE,
                             "",
                             "delete,undo,save"
                           + (IF DYNAMIC-FUNCTION("getAttribute",SESSION,"RestrictedUnlessAllowed") = "yes" THEN
                               (IF bUserMenuInst THEN ",MenuGroupAccess;Allow for user groups" ELSE "")
                             + (IF bUserMenuInst THEN ",MenuUserAccess;Allow for users" ELSE "")
                             + (IF bCompanyMenuInst THEN ",MenuCompAccess;Allow for " 
                               + (IF DYNAMIC-FUNCTION("getAttribute",SESSION,"CompanyIsRole") = "yes" THEN "roles" ELSE "companies")
                                ELSE "")
                              ELSE
                               (IF bUserMenuInst THEN ",MenuGroupAccess;Limit to user groups" ELSE "")
                             + (IF bUserMenuInst THEN ",MenuUserAccess;Limit to users" ELSE "")
                             + (IF bCompanyMenuInst THEN ",MenuCompAccess;Limit to " 
                               + (IF DYNAMIC-FUNCTION("getAttribute",SESSION,"CompanyIsRole") = "yes" THEN "roles" ELSE "companies")
                                ELSE "")
                              )
                           + (IF bAppModuleItemInst THEN ",MenuModuleAccess;Limit to modules" ELSE "")
                            ,"maxborder").
  DYNAMIC-FUNCTION("CreateObjectLink",hToolbar,hQuery).
  DYNAMIC-FUNCTION("CreateObjectLink",hToolbar,hFieldMap).

END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MenuCompAccessRecord C-Win 
PROCEDURE MenuCompAccessRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cCompanyRowIdList AS CHAR NO-UNDO.
DEF VAR cCompanyIdList    AS CHAR NO-UNDO.

THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.

cCompanyRowIdList = DYNAMIC-FUNCTION("getRowIdList","JBoxCompanyMenu,JBoxCompany","",
                                     "WHERE iJBoxMenuId = " + STRING(hFieldMap:BUFFER-FIELD("iJBoxMenuId"):BUFFER-VALUE)
                                   + ",FIRST JBoxCompany OF JBoxCompanyMenu NO-LOCK"). 

RUN JBoxSelector.w (THIS-PROCEDURE,0,
                    "JBoxCompany;cCompanyName;!iJBoxCompanyId"
                  + (IF bWriteAccessComp THEN
                      ";+WriteAccess|LOGICAL|yes/no|jbadmin_companywriteaccess.p(ROWID"
                    + STRING(hFieldMap:BUFFER-FIELD("iJBoxMenuId"):BUFFER-VALUE)
                    + ")|Write access"
                     ELSE "")
                   ,"where true",
                    INPUT-OUTPUT cCompanyRowIdList,
                    "iJBoxCompanyId" + (IF bWriteAccessComp THEN ",WriteAccess" ELSE ""),
                    INPUT-OUTPUT cCompanyIdList,
                    "","",
                    OUTPUT bOK).

THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

IF bOk THEN DO:
  IF NOT DYNAMIC-FUNCTION("runproc","jbadmin_editmenu_to_company.p",
                          STRING(hFieldMap:BUFFER-FIELD("iJBoxMenuId"):BUFFER-VALUE) + ";" + cCompanyIdList,?) THEN
    DYNAMIC-FUNCTION("DoMessage",0,1,DYNAMIC-FUNCTION("getTransactionMessage"),"Error","").
  ELSE ViewCompanyLinks().
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MenuGroupAccessRecord C-Win 
PROCEDURE MenuGroupAccessRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cGroupRowIdList AS CHAR NO-UNDO.
DEF VAR cGroupIdList    AS CHAR NO-UNDO.

THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.

cGroupRowIdList = DYNAMIC-FUNCTION("getRowIdList","JBoxUserMenu,JBoxUserGroup","",
                                   "WHERE iJBoxMenuId = " + STRING(hFieldMap:BUFFER-FIELD("iJBoxMenuId"):BUFFER-VALUE) + ",FIRST JBoxUserGroup OF JBoxUserMenu NO-LOCK"). 

RUN JBoxSelector.w (THIS-PROCEDURE,0,
                    "JBoxUserGroup;cUserGroupName|Gruppe;!iJBoxUserGroupId"
                  + (IF bWriteAccessUser THEN
                       ";+WriteAccess|LOGICAL|yes/no|jbadmin_usergroupwriteaccess.p(ROWID"
                      + STRING(hFieldMap:BUFFER-FIELD("iJBoxMenuId"):BUFFER-VALUE)
                      + ")|Write access"
                     ELSE "")
                   ,"where true",
                    INPUT-OUTPUT cGroupRowIdList,
                    "iJBoxUserGroupId" + (IF bWriteAccessUser THEN ",WriteAccess" ELSE ""),
                    INPUT-OUTPUT cGroupIdList,
                    "","",
                    OUTPUT bOK).

THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

IF bOk THEN DO:
  IF NOT DYNAMIC-FUNCTION("runproc","jbadmin_editmenu_to_groups.p",
                          STRING(hFieldMap:BUFFER-FIELD("iJBoxMenuId"):BUFFER-VALUE) + ";" + cGroupIdList,?) THEN
    DYNAMIC-FUNCTION("DoMessage",0,1,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil","").
  ELSE ViewUserLinks().
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MenuModuleAccessRecord C-Win 
PROCEDURE MenuModuleAccessRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cModuleRowIdList AS CHAR NO-UNDO.
DEF VAR cModuleIdList    AS CHAR NO-UNDO.

THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.

cModuleRowIdList = DYNAMIC-FUNCTION("getRowIdList","JBoxAppModuleItem,JboxAppModule","",
                                     "WHERE iJBoxMenuId = " + STRING(hFieldMap:BUFFER-FIELD("iJBoxMenuId"):BUFFER-VALUE)
                                   + ",FIRST JboxAppModule OF JBoxAppModuleItem NO-LOCK"). 

RUN JBoxSelector.w (THIS-PROCEDURE,0,
                    "JboxAppModule;cModuleName;!iJboxAppModuleId",
                    "where true",
                    INPUT-OUTPUT cModuleRowIdList,
                    "iJboxAppModuleId",
                    INPUT-OUTPUT cModuleIdList,
                    "","",
                    OUTPUT bOK).

THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

IF bOk THEN DO:
  IF NOT DYNAMIC-FUNCTION("runproc","jbadmin_editmenu_to_module.p",
                          STRING(hFieldMap:BUFFER-FIELD("iJBoxMenuId"):BUFFER-VALUE) + ";" + cModuleIdList,?) THEN
    DYNAMIC-FUNCTION("DoMessage",0,1,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil","").
  ELSE ViewModuleLinks().
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MenuUserAccessRecord C-Win 
PROCEDURE MenuUserAccessRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cUserRowIdList AS CHAR NO-UNDO.
DEF VAR cUserIdList    AS CHAR NO-UNDO.

THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.

cUserRowIdList = DYNAMIC-FUNCTION("getRowIdList","JBoxUserMenu,JBoxUser","",
                                   "WHERE iJBoxMenuId = " + STRING(hFieldMap:BUFFER-FIELD("iJBoxMenuId"):BUFFER-VALUE) + ",FIRST JBoxUser OF JBoxUserMenu NO-LOCK"). 

RUN JBoxSelector.w (THIS-PROCEDURE,0,
                    "JBoxUser;cJBoxUserId;cUserName"
                  + (IF bWriteAccessUser THEN
                       ";+WriteAccess|LOGICAL|yes/no|jbadmin_usergroupwriteaccess.p(ROWID"
                      + STRING(hFieldMap:BUFFER-FIELD("iJBoxMenuId"):BUFFER-VALUE)
                      + ")|Write access"
                     ELSE "")
                   ,"where true",
                    INPUT-OUTPUT cUserRowIdList,
                    "cJBoxUserId" + (IF bWriteAccessUser THEN ",WriteAccess" ELSE ""),
                    INPUT-OUTPUT cUserIdList,
                    "","",
                    OUTPUT bOK).

THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

IF bOk THEN DO:
  IF NOT DYNAMIC-FUNCTION("runproc","jbadmin_editmenu_to_users.p",
                          STRING(hFieldMap:BUFFER-FIELD("iJBoxMenuId"):BUFFER-VALUE) + ";" + cUserIdList,?) THEN
    DYNAMIC-FUNCTION("DoMessage",0,1,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil","").
  ELSE ViewUserLinks().
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MoveCompanyLinksRecord C-Win 
PROCEDURE MoveCompanyLinksRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
ASSIGN CompanyLinks:MOVABLE IN FRAME {&FRAME-NAME} = YES
       CompanyLinks:SELECTABLE = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MoveModuleLinksRecord C-Win 
PROCEDURE MoveModuleLinksRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
ASSIGN ModuleLinks:MOVABLE IN FRAME {&FRAME-NAME} = YES
       ModuleLinks:SELECTABLE = YES.
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
FRAME {&FRAME-NAME}:MOVE-TO-TOP().
/*
IF bCompanyMenuInst AND NOT bAppModuleItemInst THEN 
  ASSIGN fiCompanyLinksLabel:Y = UserLinks:Y + UserLinks:HEIGHT-PIXELS + 10 
         CompanyLinks:Y = UserLinks:Y + UserLinks:HEIGHT-PIXELS + 10
         bLimitToCompanyAdmins:Y = UserLinks:Y + UserLinks:HEIGHT-PIXELS + 20
         .
ELSE IF NOT bCompanyMenuInst AND bAppModuleItemInst THEN 
  ASSIGN ModuleLinks:Y = UserLinks:Y + UserLinks:HEIGHT-PIXELS + 10 
         fiModuleLinksLabel:Y = UserLinks:Y + UserLinks:HEIGHT-PIXELS + 10 
         .
ELSE IF bCompanyMenuInst AND bAppModuleItemInst THEN 
  ASSIGN fiCompanyLinksLabel:Y = UserLinks:Y + UserLinks:HEIGHT-PIXELS + 10 
         CompanyLinks:Y = UserLinks:Y + UserLinks:HEIGHT-PIXELS + 10
         bLimitToCompanyAdmins:Y = UserLinks:Y + UserLinks:HEIGHT-PIXELS + 20

         ModuleLinks:Y = CompanyLinks:Y + CompanyLinks:HEIGHT-PIXELS + 10 
         fiModuleLinksLabel:Y = CompanyLinks:Y + CompanyLinks:HEIGHT-PIXELS + 10 
         .
*/         
APPLY "entry" TO FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MoveUserLinksRecord C-Win 
PROCEDURE MoveUserLinksRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
ASSIGN UserLinks:MOVABLE IN FRAME {&FRAME-NAME} = YES
       UserLinks:SELECTABLE = YES.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NewMenuItem C-Win 
PROCEDURE NewMenuItem :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icMenuType AS CHAR NO-UNDO.

IF NOT DYNAMIC-FUNCTION("getActionPermission",THIS-PROCEDURE:FILE-NAME,"","new") THEN DO:
  DYNAMIC-FUNCTION("DoMessage",0,0,"Sorry, you don't have permission to do this operation","",""). 
  RETURN.  
END.

RUN InvokeMethod(hFieldMap,"NewRecord").
cMenuType:SCREEN-VALUE IN FRAME {&FRAME-NAME} = icMenuType.
setEnabledFields(icMenuType).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SaveRecord C-Win 
PROCEDURE SaveRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cTBstate      AS CHAR NO-UNDO.
DEF VAR cCurrMenuId   AS CHAR NO-UNDO.
DEF VAR cSeq          AS CHAR NO-UNDO.
DEF VAR cSelectNode   AS CHAR NO-UNDO.
DEF VAR iImgSize      AS INT  NO-UNDO.
DEF VAR iSelImgSize   AS INT  NO-UNDO.
DEF VAR iStateImgSize AS INT  NO-UNDO.
DEF VAR cParentId     AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:

  IF NOT CAN-DO("menu,tv-nav-bar,menubar",cMenuType:SCREEN-VALUE) THEN DO:
    IF NOT DYNAMIC-FUNCTION("ValidateImage" IN hParent,cImage:SCREEN-VALUE) THEN RETURN.
    IF NOT DYNAMIC-FUNCTION("ValidateImage" IN hParent,cSelImage:SCREEN-VALUE) THEN RETURN.
    IF NOT DYNAMIC-FUNCTION("ValidateImage" IN hParent,cStateImage:SCREEN-VALUE) THEN RETURN.
  END.
  ELSE IF CAN-DO("menu",cMenuType:SCREEN-VALUE) THEN
    IF NOT DYNAMIC-FUNCTION("ValidateImage" IN hParent,cStateImage:SCREEN-VALUE) THEN RETURN.
  
  cTBstate = DYNAMIC-FUNCTION("getToolbarState",hToolbar).
  
  IF cImage:SCREEN-VALUE NE "" THEN DO:
    FILE-INFO:FILE-NAME = cImage:SCREEN-VALUE.
    iImgSize = FILE-INFO:FILE-SIZE.
    IF iImgSize = ? THEN iImgSize = 0.
  END.
  IF cSelImage:SCREEN-VALUE NE "" THEN DO:
    FILE-INFO:FILE-NAME = cSelImage:SCREEN-VALUE.
    iSelImgSize = FILE-INFO:FILE-SIZE.
    IF iSelImgSize = ? THEN iSelImgSize = 0.
  END.
  IF cStateImage:SCREEN-VALUE NE "" THEN DO:
    FILE-INFO:FILE-NAME = cStateImage:SCREEN-VALUE.
    iStateImgSize = FILE-INFO:FILE-SIZE.
    IF iStateImgSize = ? THEN iStateImgSize = 0.
  END.
  
  /* Grab the sequence value for the structure file before save (which does an automatic re-display). 
     Also grab the non-updateable fields that should be assigned to the buffer: */
  cSeq = iSeq:SCREEN-VALUE.
  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"bufferextravalues",
                    cMenuType:SCREEN-VALUE + "|" + STRING(iImgSize) + "|" + STRING(iSelImgSize) + "|" + STRING(iStateImgSize)).
END.

IF hFieldMap:AVAIL THEN
  cCurrMenuId = STRING(hFieldMap:BUFFER-FIELD("iJBoxMenuId"):BUFFER-VALUE).

ASSIGN cSelectNode = DYNAMIC-FUNCTION("getNodeIndex" IN hParent)
       cParentId   = DYNAMIC-FUNCTION("getParentId" IN hParent).  

RUN SUPER.

/* Creation and update of the link table (structure table) is done here to demonstrate
   usage of update functionality from the clien. Normally the whole save operation
   would be done by calling a .p on the server, using the 'runproc' function in as-lib (see DeleteRecord proc) */
  

  /* Add parent link to current menu if adding anything but a menu: */
IF cTBstate = "new" AND cMenuType:SCREEN-VALUE NE "MENU" THEN DO:
  IF NOT DYNAMIC-FUNCTION("DoCreate","JBoxMenuToMenu" /* buffer */
                             ,""               /* validation / bl parameter  */
                             ,"iToMenuId,iFromMenuId,iSeq"  /* update fields */
                             ,cCurrMenuId + "|" 
                            + STRING(hFieldMap:BUFFER-FIELD("iJBoxMenuId"):BUFFER-VALUE) + "|" 
                            + cSeq /* Update values */
                             ,TRUE) THEN DO:
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
    RETURN.
  END.
END.
ELSE IF NOT CAN-DO("menu,menubar,nav-bar",cMenuType:SCREEN-VALUE) THEN DO:
  /* Update the sequence number stored in the structure file: */
  IF NOT DYNAMIC-FUNCTION("DoUpdate","JBoxMenuToMenu"  /* buffer */
                             ,"" /* validation / bl parameter */
                             ,"iToMenuId,iFromMenuId" /* Key fileds - for retrieval */
                             ,cParentId + "|" + 
                              STRING(hFieldMap:BUFFER-FIELD("iJBoxMenuId"):BUFFER-VALUE)  /* Key values */
                             ,"iSeq"      /* Update fields */
                             ,cSeq        /* Update values */
                             ,TRUE) THEN DO:
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
    RETURN.
  END.
END.
ELSE IF cMenuType:SCREEN-VALUE = "menu" THEN DO:
  DYNAMIC-FUNCTION("RefreshMenuDropDown" IN hParent).
  RETURN.    
END.

DYNAMIC-FUNCTION("setSelectNode" IN hParent,cSelectNode).
RUN FillTreeView IN hParent.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSelectorAttributes C-Win 
PROCEDURE setSelectorAttributes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihBrwSource AS HANDLE NO-UNDO.
DEF INPUT PARAM ihBrwTarget AS HANDLE NO-UNDO.

DEF VAR hColumn   AS HANDLE NO-UNDO.
DEF VAR hDropDown AS HANDLE NO-UNDO.
DEF VAR hSelWin   AS HANDLE NO-UNDO.

hSelWin = ihBrwSource:WINDOW.

hColumn = DYNAMIC-FUNCTION("getBrowseColumn",ihBrwSource,"WriteAccess").  
IF VALID-HANDLE(hColumn) THEN DO:

  hColumn:VISIBLE = NO.
  ihBrwSource:GET-BROWSE-COLUMN(1):WIDTH-PIXELS = ihBrwSource:GET-BROWSE-COLUMN(1):WIDTH-PIXELS - 60.
  ihBrwTarget:GET-BROWSE-COLUMN(1):WIDTH-PIXELS = ihBrwTarget:GET-BROWSE-COLUMN(1):WIDTH-PIXELS - 60.

  IF ihBrwSource:QUERY:GET-BUFFER-HANDLE(1):NAME = "JBoxUser" THEN
    ASSIGN ihBrwSource:GET-BROWSE-COLUMN(2):WIDTH-PIXELS = ihBrwSource:GET-BROWSE-COLUMN(2):WIDTH-PIXELS - 60
           ihBrwTarget:GET-BROWSE-COLUMN(2):WIDTH-PIXELS = ihBrwTarget:GET-BROWSE-COLUMN(2):WIDTH-PIXELS - 60.

  hSelWin:WIDTH-PIXELS = hSelWin:WIDTH-PIXELS + 200.
  APPLY "window-resized" TO hSelWin.
                       
  hDropDown = DYNAMIC-FUNCTION("NewBrowseDropDown",ihBrwTarget,"WriteAccess","WriteAccess"
             ,"","","Yes|yes|No|no").
             
  DYNAMIC-FUNCTION("CreateOverlayLink",ihBrwTarget,hDropDown,"WriteAccess").
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValueChangedField C-Win 
PROCEDURE ValueChangedField :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icFieldName AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  CASE icFieldName:
    WHEN "cLaunchtype" THEN bMultiple:SENSITIVE = NOT bMultiple:HIDDEN AND cLaunchType:SCREEN-VALUE = "start-window".
  END CASE.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WhereUsed C-Win 
PROCEDURE WhereUsed :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF NOT hFieldMap:AVAIL THEN RETURN.

IF NOT VALID-HANDLE(hPrintPreview) THEN DO:
  RUN JBoxTextFilePreview.w PERSIST SET hPrintPreview.
  /* To remove Excel button (Excel only applies for column reports with regular column headers): */
  DYNAMIC-FUNCTION("setEnableExcel" IN hPrintPreview,NO).
  RUN InitializeObject IN hPrintPreview.
END.

DYNAMIC-FUNCTION("setWindowTitle" IN hPrintPreview,"Where used: " + hFieldMap:BUFFER-FIELD("cMenuLabel"):BUFFER-VALUE).

/* If the header looks nasty (like here) in Excel it can be skipped from the export: */
DYNAMIC-FUNCTION("setViewHeadersInExcel" IN hPrintPreview,NO).

/*
/* If (like here) the column headers don't occur on the first page, give the start of the column header line so
   column headers may be viewed the first time they occur: */
DYNAMIC-FUNCTION("setStartColumnHeader" IN hPrintPreview,"Line Num ").

/* For pages after the first, instuct the preview to hide header lines, including the column headers */
DYNAMIC-FUNCTION("setEndColumnHeader" IN hPrintPreview,"-----"). /* <- the beginning of the end */

/* To hide the footer from the preview: */
DYNAMIC-FUNCTION("setFooterLineNum" IN hPrintPreview,39). 
*/

DYNAMIC-FUNCTION("LoadPreviewFromTT" IN hPrintPreview,
                 DYNAMIC-FUNCTION("getTempTable","jbsetup_menuwhereused.p"
                                   ,STRING(hFieldMap:BUFFER-FIELD("iJBoxMenuId"):BUFFER-VALUE),?)
                ,"",NO).

RUN MoveToTop IN hPrintPreview.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFrameHandle C-Win 
FUNCTION getFrameHandle RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN FRAME {&FRAME-NAME}:HANDLE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InitializeResize C-Win 
FUNCTION InitializeResize RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,"UserLinks,CompanyLinks,ModuleLinks").
DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,"ModuleLinks").
DYNAMIC-FUNCTION("setAddMoveY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,"ModuleLinks,fiModuleLinksLabel").
  
DYNAMIC-FUNCTION("setResizeYGroup", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,50,"UserLinks,CompanyLinks").
DYNAMIC-FUNCTION("setMoveYGroup", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,50,"fiCompanyLinksLabel,CompanyLinks").

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setEnabledFields C-Win 
FUNCTION setEnabledFields RETURNS LOGICAL
  ( INPUT icMenuType AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hWidget         AS HANDLE NO-UNDO.
DEF VAR cMenuWidgetList AS CHAR   NO-UNDO.

ASSIGN cMenuWidgetList = DYNAMIC-FUNCTION("getAttribute",hFieldMap,"screenupdatewidgets") + "," 
                       + DYNAMIC-FUNCTION("getAttribute",hFieldMap,"extraupdatewidgets")
       cParameter:HIDDEN IN FRAME {&FRAME-NAME} = YES
       bMultiple:HIDDEN IN FRAME {&FRAME-NAME} = YES
       .

DO ix = 1 TO NUM-ENTRIES(cMenuWidgetList):
  hWidget = WIDGET-HANDLE(ENTRY(ix,cMenuWidgetList)).

  IF CAN-DO("bConfigurable,bDefault,bLogExec",hWidget:NAME) THEN hWidget:HIDDEN = YES.

  IF CAN-DO("menu,menubar,tv-nav-bar,placeholder",icMenuType) THEN DO:
    IF CAN-DO("cLaunchType,iSeq,cStateImage,cAccelerator,cMenuNumber,cMenuTooltip,cTextColor,cFontStyle,btnStateImage",hWidget:NAME) THEN
      hWidget:SENSITIVE = NO.
    ELSE
      hWidget:SENSITIVE = YES.

    IF icMenuType NE "tv-nav-bar" AND CAN-DO("iTvNavBarStyle",hWidget:NAME) THEN
      hWidget:SENSITIVE = NO.
    IF icMenuType NE "tv-nav-bar" AND CAN-DO("cSelImage,btnSelImage",hWidget:NAME) THEN
      hWidget:SENSITIVE = NO.
    IF CAN-DO("menubar,placeholder",icMenuType) AND CAN-DO("cImage,btnImage,cLaunchType",hWidget:NAME) THEN
      hWidget:SENSITIVE = NO.
    IF NOT CAN-DO("menu,placeholder",icMenuType) AND CAN-DO("cLaunch",hWidget:NAME) THEN
      hWidget:SENSITIVE = NO.
    IF NOT CAN-DO("menu",icMenuType) AND CAN-DO("btnLaunch",hWidget:NAME) THEN
      hWidget:SENSITIVE = NO.
    IF icMenuType = "menu" AND CAN-DO("bConfigurable,cMenuNumber,bDefault",hWidget:NAME) THEN
      ASSIGN hWidget:HIDDEN = NO
             hWidget:SENSITIVE = YES.
    IF CAN-DO("placeholder",icMenuType) AND hWidget:NAME = "iSeq" THEN
      hWidget:SENSITIVE = YES.
    IF CAN-DO("menu",icMenuType) AND hWidget:NAME = "cLaunchType" THEN
      hWidget:SENSITIVE = YES.
  END.
  ELSE IF CAN-DO("sub-menu",icMenuType) THEN DO:
    cParameter:HIDDEN = NO.
    IF CAN-DO("cLaunch,cLaunchType,bConfigurable,cAccelerator,cMenuNumber,btnLaunch,iTvNavBarStyle",hWidget:NAME) THEN
      hWidget:SENSITIVE = NO.
    ELSE
      hWidget:SENSITIVE = YES.
  END.
  ELSE IF CAN-DO("rule",icMenuType) THEN DO:
    IF CAN-DO("iSeq",hWidget:NAME) THEN
      hWidget:SENSITIVE = YES.
    ELSE
      hWidget:SENSITIVE = NO.
  END.
  ELSE IF CAN-DO("menu-item",icMenuType) THEN DO:
    IF CAN-DO("iTvNavBarStyle",hWidget:NAME) THEN
      hWidget:SENSITIVE = NO.
    IF CAN-DO("cParameter,bMultiple,bLogExec",hWidget:NAME) THEN 
      hWidget:HIDDEN = NO.
    IF hWidget:NAME = "bConfigurable" THEN
      ASSIGN hWidget:HIDDEN = NO
             hWidget:SENSITIVE = YES.
  END.

END.

IF NOT cLaunch:SENSITIVE IN FRAME {&FRAME-NAME} THEN cLaunch:SCREEN-VALUE = ?.
IF NOT iTvNavBarStyle:SENSITIVE IN FRAME {&FRAME-NAME} THEN iTvNavBarStyle:SCREEN-VALUE = ?.

DO WITH FRAME {&FRAME-NAME}:
  ASSIGN cLaunch:LABEL  = "Launch"
/*          UserLinks:HIDDEN = YES */
/*          fiUserLinksLabel:HIDDEN = YES */
         iTvNavBarStyle:HIDDEN = YES
         cStateImage:LABEL = "Ribbon image"
/*          fiNodeIndex:SCREEN-VALUE = DYNAMIC-FUNCTION("getNodeIndex" IN hParent)  */
         .
  CASE icMenuType:
    WHEN "tv-nav-bar" THEN DO:
      iTvNavBarStyle:HIDDEN = NO.
      IF iTvNavBarStyle:SCREEN-VALUE = "1" THEN
        ASSIGN cImage:LABEL    = "Default nav-bar img"
               cSelImage:LABEL = "Default nav-bar item (node) img"
               cStateImage:SENSITIVE = YES
               btnStateImage:SENSITIVE = YES
               cStateImage:LABEL = "Background image"
               .
      ELSE
        ASSIGN cImage:LABEL      = "Default treeview node img"
               cSelImage:LABEL   = "Default selected tv-node img"
               .

      cImage:TOOLTIP  = "Use .ico files for large nodes, .bmp for small". 
    END. 
    WHEN "menu" THEN DO:
      ASSIGN cImage:LABEL   = "Default image for windows"
             cImage:TOOLTIP = "Used if window image is not set"
             cLaunch:LABEL  = "Launch URL/HTML file or Panel prog"
             cStateImage:SENSITIVE = YES
             cStateImage:LABEL = "Logo file"
             btnStateImage:SENSITIVE = YES.
    END.
    WHEN "placeholder" THEN
      cLaunch:LABEL  = "Id (tag) for placeholder".
    OTHERWISE 
      ASSIGN cImage:LABEL     = "Navbar: Bar/node img. Tv: node img"
             cSelImage:LABEL  = "Navbar: N/A. Treeview: Sel.node img"
             cImage:TOOLTIP   = "If .ico on sub-menu: Default image for child node windows" 
             cParameter:LABEL = "Parameter"
             .
  END CASE.
END.

ViewUserLinks().
ViewCompanyLinks().
ViewModuleLinks().

RETURN TRUE. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setParent C-Win 
FUNCTION setParent RETURNS LOGICAL
  ( INPUT ihParent AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
hParent = ihParent.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setQuery C-Win 
FUNCTION setQuery RETURNS LOGICAL
  ( INPUT ihQuery AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
hQuery = ihQuery.

RETURN TRUE.

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

IF icFileName MATCHES "*bmp" OR icFileName MATCHES "*ico" OR icFileName MATCHES "*.png"  OR icFileName MATCHES "*.gif"
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

  FILE-INFO:FILE-NAME = icFileName.
  CASE ihTargetField:NAME:
    WHEN "cImage"      THEN iImageSize:SCREEN-VALUE      = STRING(FILE-INFO:FILE-SIZE).
    WHEN "cSelImage"   THEN iSelImageSize:SCREEN-VALUE   = STRING(FILE-INFO:FILE-SIZE).
    WHEN "cStateImage" THEN iStateImageSize:SCREEN-VALUE = STRING(FILE-INFO:FILE-SIZE).
  END CASE.

  APPLY "any-printable" TO ihTargetField.
  THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().

  RETURN YES.
END.
ELSE 
  DYNAMIC-FUNCTION("DoMessage",0,0,"Invalid image file type: Must be bmp, png, gif or ico","","").
  
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
  IF SEARCH(ihTargetField:SCREEN-VALUE) = ? THEN DO:
    MESSAGE "Invalid program: " icFileName SKIP "(must be in PROPATH)"
             VIEW-AS ALERT-BOX ERROR.
    ihTargetField:SCREEN-VALUE = "".
    RETURN NO.
  END.    

  APPLY "any-printable" TO ihTargetField.
  THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().

/*   RETURN YES. */
END.
/* ELSE                                                                                      */
/*   DYNAMIC-FUNCTION("DoMessage",0,0,"Invalid image file type: Must be bmp or ico","","").  */
  
RETURN YES.
  
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ViewCompanyLinks C-Win 
FUNCTION ViewCompanyLinks RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  IF hFieldMap:AVAIL AND DYNAMIC-FUNCTION("runProc","jbsetup_getlinked_companies_formenu.p",STRING(hFieldMap:BUFFER-FIELD("iJBoxMenuId"):BUFFER-VALUE),?) THEN
    ASSIGN 
/*            CompanyLinks:HIDDEN = NO        */
/*            fiCompanyLinksLabel:HIDDEN = NO */
           CompanyLinks:SCREEN-VALUE = DYNAMIC-FUNCTION("getTransactionMessage").
  ELSE CompanyLinks:SCREEN-VALUE = "".
/*     ASSIGN CompanyLinks:HIDDEN = YES          */
/*            fiCompanyLinksLabel:HIDDEN = YES.  */
END.
  
RETURN YES.   

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ViewModuleLinks C-Win 
FUNCTION ViewModuleLinks RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  IF hFieldMap:AVAIL AND DYNAMIC-FUNCTION("runProc","jbsetup_getlinked_modules_formenu.p",STRING(hFieldMap:BUFFER-FIELD("iJBoxMenuId"):BUFFER-VALUE),?) THEN
    ModuleLinks:SCREEN-VALUE = DYNAMIC-FUNCTION("getTransactionMessage").
  ELSE ModuleLinks:SCREEN-VALUE = "".
END.
  
RETURN YES.   

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ViewUserLinks C-Win 
FUNCTION ViewUserLinks RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  IF hFieldMap:AVAIL AND DYNAMIC-FUNCTION("runProc","jbsetup_getlinked_users_formenu.p",STRING(hFieldMap:BUFFER-FIELD("iJBoxMenuId"):BUFFER-VALUE),?) THEN
    UserLinks:SCREEN-VALUE = DYNAMIC-FUNCTION("getTransactionMessage").
  ELSE UserLinks:SCREEN-VALUE = "".
END.
  
RETURN YES.   

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

