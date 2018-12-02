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

DEF VAR bOk                AS LOG    NO-UNDO.
DEF VAR ix                 AS INT    NO-UNDO.
DEF VAR hBrwApplication    AS HANDLE NO-UNDO.
DEF VAR hBrwPackage        AS HANDLE NO-UNDO.
DEF VAR hFMApplication     AS HANDLE NO-UNDO.
DEF VAR hTBApplication     AS HANDLE NO-UNDO.
DEF VAR hTBPackage         AS HANDLE NO-UNDO.
DEF VAR hSearchField       AS HANDLE NO-UNDO.

DEF VAR hPathOverlay       AS HANDLE NO-UNDO.
DEF VAR hPropathSeqOverlay AS HANDLE NO-UNDO.
DEF VAR hInstallSeqOverlay AS HANDLE NO-UNDO.
DEF VAR hSignatureOverlay  AS HANDLE NO-UNDO.

DEF VAR cRoot              AS CHAR   NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnSplitBarY rectBrwApplication ~
rectTBApplication RectBrowseSearch rectBrwPackage rectTBPackage cAppName ~
cLanguages cAppTitle cBaseLanguage dCreated cCreatedBy 
&Scoped-Define DISPLAYED-OBJECTS cAppName cLanguages cAppTitle ~
cBaseLanguage dCreated cCreatedBy 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getRootDir C-Win 
FUNCTION getRootDir RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LoadFiles C-Win 
FUNCTION LoadFiles RETURNS LOGICAL
  ( INPUT icFileNames AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnSplitBarY 
     IMAGE-UP FILE "bmp/tabdown.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Button 1" 
     SIZE 129 BY .43.

DEFINE VARIABLE cAppName AS CHARACTER FORMAT "x(40)" 
     LABEL "Name" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1.

DEFINE VARIABLE cAppTitle AS CHARACTER FORMAT "x(30)" 
     LABEL "App.title" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE cBaseLanguage AS CHARACTER FORMAT "xx" 
     LABEL "Base language" 
     VIEW-AS FILL-IN 
     SIZE 6.4 BY 1 TOOLTIP "Development language".

DEFINE VARIABLE cCreatedBy AS CHARACTER FORMAT "X(8)" 
     LABEL "By" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1.

DEFINE VARIABLE cLanguages AS CHARACTER FORMAT "x(30)" 
     LABEL "Languages" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1 TOOLTIP "Comma-separated list of available language codes".

DEFINE VARIABLE dCreated AS DATE FORMAT "99/99/9999" 
     LABEL "Created" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.

DEFINE RECTANGLE RectBrowseSearch
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 17 BY .95.

DEFINE RECTANGLE rectBrwApplication
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 129 BY 5.62.

DEFINE RECTANGLE rectBrwPackage
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 129 BY 6.67.

DEFINE RECTANGLE rectSplitBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 130.6 BY 18.52.

DEFINE RECTANGLE rectTBApplication
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 17 BY .95.

DEFINE RECTANGLE rectTBPackage
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 17 BY .95.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnSplitBarY AT ROW 11.86 COL 2
     cAppName AT ROW 9.38 COL 8.6 COLON-ALIGNED
     cLanguages AT ROW 9.38 COL 64 COLON-ALIGNED
     cAppTitle AT ROW 10.52 COL 8.6 COLON-ALIGNED
     cBaseLanguage AT ROW 10.52 COL 64 COLON-ALIGNED
     dCreated AT ROW 10.52 COL 97 COLON-ALIGNED
     cCreatedBy AT ROW 10.52 COL 116 COLON-ALIGNED
     rectBrwApplication AT ROW 2.52 COL 2
     rectTBApplication AT ROW 8.29 COL 2.4
     RectBrowseSearch AT ROW 1.29 COL 2.2
     rectBrwPackage AT ROW 13.86 COL 1.8
     rectTBPackage AT ROW 12.57 COL 2.4
     rectSplitBar AT ROW 2.19 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 130.6 BY 19.71.


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
         TITLE              = "<insert window title>"
         HEIGHT             = 19.71
         WIDTH              = 130.6
         MAX-HEIGHT         = 50
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 50
         VIRTUAL-WIDTH      = 320
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
       btnSplitBarY:MOVABLE IN FRAME DEFAULT-FRAME          = TRUE.

/* SETTINGS FOR RECTANGLE rectSplitBar IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       rectSplitBar:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <insert window title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* <insert window title> */
DO:
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"resize","").
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


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
  IF NOT DYNAMIC-FUNCTION("DoCleanUpObjects",THIS-PROCEDURE:CURRENT-WINDOW) THEN
    RETURN NO-APPLY.
  DYNAMIC-FUNCTION("setCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
  RUN disable_UI.
  PUBLISH "InvalidateHandle" (THIS-PROCEDURE).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ChangeCompany C-Win 
PROCEDURE ChangeCompany :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DYNAMIC-FUNCTION("setAttribute",hBrwApplication,"basequery","WHERE iJBoxCompanyId = " + STRING(DYNAMIC-FUNCTION("getCompanyId"))).
DYNAMIC-FUNCTION("setCurrentObject",hBrwApplication).
RUN OpenQuery.
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
  DISPLAY cAppName cLanguages cAppTitle cBaseLanguage dCreated cCreatedBy 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btnSplitBarY rectBrwApplication rectTBApplication RectBrowseSearch 
         rectBrwPackage rectTBPackage cAppName cLanguages cAppTitle 
         cBaseLanguage dCreated cCreatedBy 
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
DEF VAR hNewPackageButton AS HANDLE NO-UNDO.

RUN enable_UI.

DYNAMIC-FUNCTION("CreateStatusBar",THIS-PROCEDURE:CURRENT-WINDOW,"",0,YES,?).

DO WITH FRAME {&FRAME-NAME}:
  hBrwApplication = DYNAMIC-FUNCTION("NewBrowse",
                                 rectBrwApplication:HANDLE,
                                 100,
                                 "",
                                 "JBoxApplication"
                               + ";iJBoxApplicationId|App.id"
                               + ";cAppName"
                               + ";cAppTitle"
                               + ";cLanguages"
                               + ";cBaseLanguage"
                               + ";dCreated|Created"
                               + ";cCreatedBy|By"
                               + ";dModified|Modified"
                               + ";cModifiedBy|By"
                                ,"WHERE false"
                                ,"SORT|cAppName").
  hBrwApplication:NAME = "brwPackage".

  hSearchField = DYNAMIC-FUNCTION("NewBrowseSearchField",RectBrowseSearch:HANDLE,hBrwApplication,2).
  DYNAMIC-FUNCTION("createObjectLink",hBrwApplication,hSearchField).

  hFMApplication = DYNAMIC-FUNCTION("NewFieldMap",
                                hBrwApplication:QUERY,
                                FRAME {&FRAME-NAME}:HANDLE,
                                "cAppName,cAppTitle,cLanguages,cBaseLanguage","",
                                "dCreated,cCreatedBy","",
                                "").
  DYNAMIC-FUNCTION("setAttribute",hFMApplication,"bufferextrafields","iJBoxCompanyId").
  DYNAMIC-FUNCTION("createObjectLink",hBrwApplication,hFMApplication).

  hTBApplication = DYNAMIC-FUNCTION("NewToolbar",
                                rectTBApplication:HANDLE,
                                "Application",
                                "New,Copy,Undo,Delete,Save,Filter,FlatView,Excel"
                              + ",Test;Install;Test installation"
                               ,"").
  DYNAMIC-FUNCTION("createObjectLink",hBrwApplication,hTBApplication).
  DYNAMIC-FUNCTION("createObjectLink",hFMApplication,hTBApplication).


  hBrwPackage = DYNAMIC-FUNCTION("NewBrowse",
                                 rectBrwPackage:HANDLE,
                                 100,
                                 "",
                                 "JBoxAppPackage"
                                 + ";cRelativePath"
                                 + ";iInstallSeq"
                                 + ";iPropathSeq"
                                 + ";cSignature"
                                 + ";!iJBoxPackageId"
                                 + ";!iJBoxApplicationId"
                               + ",JBoxPackage"
                                 + ";bComplete"
                                 + ";bDownLoad"
                                 + ";bProPath"
                                 + ";bRestart"
                                 + ";cPackageName@1"
                                 + ";cVersion@2"
                                 + ";+TotSize|DECIMAL|>><>>9.99|package_totsize.p|KB"
                                 + ";dCreated|Created (link)"
                                 + ";cCreatedBy|By"
                                 + ";dModified|Modified (link)"
                                 + ";cModifiedBy|By"
                                ,"WHERE FALSE"
                               + ",FIRST JBoxPackage OF JBoxAppPackage"
                                ,"SORT|cSignature DESC").
/*   hBrwPackage:MOVE-COLUMN(9,1).  */
/*   hBrwPackage:MOVE-COLUMN(10,2). */
  hBrwPackage:GET-BROWSE-COLUMN(1):WIDTH-PIXELS = 100.
  hBrwPackage:GET-BROWSE-COLUMN(2):WIDTH-PIXELS = 100.
  hBrwPackage:GET-BROWSE-COLUMN(3):WIDTH-PIXELS = 100.
  
  DYNAMIC-FUNCTION("setAttribute",hBrwPackage,"enableOnDblClick","yes").

  DYNAMIC-FUNCTION("setNoColumnSort",hBrwPackage,"TotSize"). 
  DYNAMIC-FUNCTION("CreateParentLink",hBrwPackage,hBrwApplication,"iJBoxApplicationId").

  hPathOverlay = DYNAMIC-FUNCTION("NewBrowseFillIn",
                    hBrwPackage,          
                    "cRelativePath",     
                    "cRelativePath",     
                    "","","","").                
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrwPackage,hPathOverlay,"cRelativePath").

  hInstallSeqOverlay = DYNAMIC-FUNCTION("NewBrowseFillIn",
                    hBrwPackage,          
                    "iInstallSeq",     
                    "iInstallSeq",     
                    "","","","").                
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrwPackage,hInstallSeqOverlay,"iInstallSeq").

  hPropathSeqOverlay = DYNAMIC-FUNCTION("NewBrowseFillIn",
                    hBrwPackage,          
                    "iPropathSeq",     
                    "iPropathSeq",     
                    "","","","").                
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrwPackage,hPropathSeqOverlay,"iPropathSeq").

  hSignatureOverlay = DYNAMIC-FUNCTION("NewBrowseFillIn",
                    hBrwPackage,          
                    "cSignature",     
                    "cSignature",     
                    "","","","").                
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrwPackage,hSignatureOverlay,"cSignature").

  hTBPackage = DYNAMIC-FUNCTION("NewToolBar",
                    rectTBPackage:HANDLE,             /* Rectangle to define coordinates for toolbar */
                    "Package",                          /* Corresponding menu label - no menu if blank */
/*                     "new;Edit packages;Add/remove packages for application;editRecord;bmp/edit16e.bmp" */
                    "new,excel"
                    ,"").
  hNewPackageButton = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hTBPackage,"buttonNew")).
  hNewPackageButton:LOAD-IMAGE("bmp/edit16e.bmp").

  DYNAMIC-FUNCTION("CreateObjectLink",hBrwPackage,hTBPackage).

  DYNAMIC-FUNCTION("setNoResizeX",THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:HANDLE,"RectBrowseSearch").
  DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:HANDLE,"rectBrwApplication,brwPackage,rectTBApplication,rectTBPackage").
  DYNAMIC-FUNCTION("setSplitBarY",THIS-PROCEDURE:CURRENT-WINDOW,btnSplitBarY:HANDLE IN FRAME {&FRAME-NAME},NO).

  DYNAMIC-FUNCTION("setFollowSplitBarY",THIS-PROCEDURE:CURRENT-WINDOW,btnSplitBarY:HANDLE IN FRAME {&FRAME-NAME}, 
                    DYNAMIC-FUNCTION("getWidgetsByLasso",rectSplitBar:HANDLE,0,"rectangle,browse,button,fill-in,toggle-box")
                    ).
  DYNAMIC-FUNCTION("setSplitBarYlimits",btnSplitBarY:HANDLE,220,150).
  DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,330,1000,0,0).

  SUBSCRIBE TO "ChangeCompany" ANYWHERE.
  RUN ChangeCompany.

  THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.
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
APPLY "entry" TO hBrwApplication.

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
DEF VAR cPackageRowIdList AS CHAR NO-UNDO.
DEF VAR cPackageIdList    AS CHAR NO-UNDO.

IF DYNAMIC-FUNCTION("getCurrentObject") = hTBPackage THEN DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  IF hBrwPackage:QUERY:IS-OPEN THEN DO:
    hBrwPackage:QUERY:GET-FIRST().
    REPEAT WHILE NOT hBrwPackage:QUERY:QUERY-OFF-END:
      cPackageRowIdList = cPackageRowIdList + hBrwPackage:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("RowIdent2"):BUFFER-VALUE + ",".
      hBrwPackage:QUERY:GET-NEXT().
    END.
    cPackageRowIdList = TRIM(cPackageRowIdList,",").
  END.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "JboxPackage;cPackageName;cVersion;bComplete;bDownLoad;bProPath;bRestart;!iJBoxPackageId;!iJBoxCompanyId",
                      "WHERE iJBoxCompanyId = " + STRING(DYNAMIC-FUNCTION("getCompanyId")),
                      INPUT-OUTPUT cPackageRowIdList,
                      "iJBoxPackageId",
                      INPUT-OUTPUT cPackageIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.
  
  IF bOk THEN DO:
    IF NOT DYNAMIC-FUNCTION("runproc","edit_app_package.p",
                            STRING(hFMApplication:BUFFER-FIELD("iJBoxApplicationId"):BUFFER-VALUE) + "|" + cPackageIdList,?) THEN
      DYNAMIC-FUNCTION("DoMessage",0,1,DYNAMIC-FUNCTION("getTransactionMessage"),"Error","").
    ELSE 
      APPLY "value-changed" TO hBrwApplication.
  END.
END.
ELSE RUN SUPER.

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
DYNAMIC-FUNCTION("setAttribute",hFMApplication,"bufferextravalues",STRING(DYNAMIC-FUNCTION("getCompanyId"))).

RUN SUPER.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TestRecord C-Win 
PROCEDURE TestRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
cRoot = "".
SYSTEM-DIALOG GET-DIR cRoot INITIAL-DIR "c:\temp" TITLE "Install to".

IF cRoot NE "" THEN DO:
  ASSIGN cRoot = TRIM(cRoot,"\") + "\"
         bOk   = YES.

  IF SEARCH(cRoot + "signature.txt") NE ? THEN 
    MESSAGE "Signature file exists. Create new (for complete reinstall)?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE bOk.
  IF bOk THEN DO:
    OUTPUT TO VALUE(cRoot + "signature.txt").
    PUT UNFORMATTED STRING(hFMApplication:BUFFER-FIELD("iJBoxApplicationId"):BUFFER-VALUE) 
                  + "|0|" 
                  + hFMApplication:BUFFER-FIELD("cAppTitle"):BUFFER-VALUE + "|" 
                  + hFMApplication:BUFFER-FIELD("cLanguages"):BUFFER-VALUE + "|" 
                  + hFMApplication:BUFFER-FIELD("cBaseLanguage"):BUFFER-VALUE
                  SKIP.
    OUTPUT CLOSE.
  END.
  PUBLISH "setSensitiveWin" (FALSE).
  RUN JBoxStartup.p.
  PUBLISH "setSensitiveWin" (TRUE).

  IF SEARCH("gzip.exe") NE ? THEN DO:
    IF INDEX(cRoot,":") > 0 THEN DO:
      FILE-INFO:FILE-NAME = SEARCH("gzip.exe").
      OUTPUT TO .\uncomp.bat.
      PUT UNFORMATTED SUBSTR(cRoot,1,2) SKIP.
      PUT UNFORMATTED "cd " + QUOTER(cRoot) SKIP.
      PUT UNFORMATTED QUOTER(FILE-INFO:FULL-PATHNAME) + " -dfr *.*" SKIP.
      OUTPUT CLOSE.
      OS-COMMAND SILENT .\uncomp.bat.
    END.
    ELSE OS-COMMAND SILENT VALUE(QUOTER(SEARCH("gzip.exe") + " -dfr " + cRoot + "*.*")). 
  END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getRootDir C-Win 
FUNCTION getRootDir RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN cRoot.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LoadFiles C-Win 
FUNCTION LoadFiles RETURNS LOGICAL
  ( INPUT icFileNames AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DYNAMIC-FUNCTION("LoadDocs",icFileNames,
                 hFMApplication:NAME,STRING(hFMApplication:BUFFER-FIELD("iJBoxApplicationId"):BUFFER-VALUE),hFMApplication:BUFFER-FIELD("cPackageName"):BUFFER-VALUE).
APPLY "value-changed" TO hBrwApplication.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

