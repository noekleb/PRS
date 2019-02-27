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

DEF VAR bOk             AS LOG    NO-UNDO.
DEF VAR ix              AS INT    NO-UNDO.
DEF VAR hBrowse         AS HANDLE NO-UNDO.
DEF VAR hQuery          AS HANDLE NO-UNDO.
DEF VAR hToolbar        AS HANDLE NO-UNDO.
DEF VAR hFieldMap       AS HANDLE NO-UNDO.
DEF VAR hBrwCompLink    AS HANDLE NO-UNDO.
DEF VAR hBrwMenuLink AS HANDLE NO-UNDO.
DEF VAR hBrwProgramLink AS HANDLE NO-UNDO.
DEF VAR hTbProgramLink  AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tbModule brwModule brwCompLink brwMenuLink ~
brwProgramLink tbProgramLink cModuleName dCreated cCreatedBy dModified ~
cModifiedBy 
&Scoped-Define DISPLAYED-OBJECTS cModuleName dCreated cCreatedBy dModified ~
cModifiedBy 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AdjustBrowseColumns C-Win 
FUNCTION AdjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse     AS HANDLE,
    INPUT icBrowseName AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE cCreatedBy AS CHARACTER FORMAT "x(8)" 
     LABEL "Av" 
     VIEW-AS FILL-IN 
     SIZE 13.6 BY 1.

DEFINE VARIABLE cModifiedBy AS CHARACTER FORMAT "x(8)" 
     LABEL "Av" 
     VIEW-AS FILL-IN 
     SIZE 13.6 BY 1.

DEFINE VARIABLE cModuleName AS CHARACTER FORMAT "x(40)" 
     LABEL "Module name" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1.

DEFINE VARIABLE dCreated AS DATE FORMAT "99/99/9999" 
     LABEL "Opprettet" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE dModified AS DATE FORMAT "99/99/9999" 
     LABEL "Endret" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE RECTANGLE brwCompLink
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 38.5 BY 10.

DEFINE RECTANGLE brwMenuLink
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 38.5 BY 10.

DEFINE RECTANGLE brwModule
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 58 BY 8.1.

DEFINE RECTANGLE brwProgramLink
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 38.5 BY 8.81.

DEFINE RECTANGLE tbModule
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 9.8 BY .95.

DEFINE RECTANGLE tbProgramLink
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 9.8 BY .91.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     cModuleName AT ROW 2.91 COL 76 COLON-ALIGNED
     dCreated AT ROW 8.81 COL 76 COLON-ALIGNED
     cCreatedBy AT ROW 8.86 COL 100 COLON-ALIGNED
     dModified AT ROW 9.81 COL 76 COLON-ALIGNED
     cModifiedBy AT ROW 9.86 COL 100 COLON-ALIGNED
     tbModule AT ROW 1.48 COL 4
     brwModule AT ROW 2.91 COL 3
     brwCompLink AT ROW 11.48 COL 3
     brwMenuLink AT ROW 11.48 COL 42.2
     brwProgramLink AT ROW 11.48 COL 81.8
     tbProgramLink AT ROW 20.43 COL 82.2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 120.2 BY 20.86.


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
         HEIGHT             = 20.86
         WIDTH              = 120.2
         MAX-HEIGHT         = 23.52
         MAX-WIDTH          = 120.2
         VIRTUAL-HEIGHT     = 23.52
         VIRTUAL-WIDTH      = 120.2
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
       FRAME DEFAULT-FRAME:HEIGHT           = 20.86
       FRAME DEFAULT-FRAME:WIDTH            = 120.2.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DefaultActionBrowse C-Win 
PROCEDURE DefaultActionBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF DYNAMIC-FUNCTION("getCurrentObject") = hBrwProgramLink THEN
  RUN InvokeMethod(hTbProgramLink,"EditRecord").
ELSE RUN SUPER.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EditRecord C-Win 
PROCEDURE EditRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF DYNAMIC-FUNCTION("getCurrentObject") = hTbProgramLink THEN DO:
  RUN JBoxDAppModuleLinkProps.w ("edit",
                                 hBrwProgramLink:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("iJBoxAppModuleItemId"):BUFFER-VALUE,
                                 0,
                                 hFieldMap:BUFFER-FIELD("iJBoxAppModuleId"):BUFFER-VALUE,
                                 OUTPUT bOk).
  IF bOk THEN DYNAMIC-FUNCTION("RefreshRowids",hBrwProgramLink,hBrwProgramLink:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).
END.
ELSE RUN SUPER.
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
  DISPLAY cModuleName dCreated cCreatedBy dModified cModifiedBy 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE tbModule brwModule brwCompLink brwMenuLink brwProgramLink 
         tbProgramLink cModuleName dCreated cCreatedBy dModified cModifiedBy 
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
        ,brwModule:HANDLE
        ,100
        ,""
        ,"JBoxAppModule"
          + ";cModuleName"
          + ";dCreated"
          + ";cCreatedBy"
          + ";dModified"
          + ";cModifiedBy"
          + ";!iJBoxAppModuleId"
        ,"WHERE false"
        ,"").

  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap"
        ,hBrowse:QUERY
        ,FRAME {&FRAME-NAME}:HANDLE
        ,"cModuleName",""
        ,"cCreatedBy,cModifiedBy,dCreated,dModified",""
        ,"").

  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hFieldMap).

  hToolbar = DYNAMIC-FUNCTION("NewToolBar"
        ,TbModule:HANDLE
        ,"Fil"
        ,"new,undo,delete,save,excel"
       + ",LinkCompany;Link companies"
       + ",LinkMenues;Link menues"
       + ",LinkPrograms;Link programs;Link programs for default (0) company"
        ,"maxborder").

  DYNAMIC-FUNCTION("CreateObjectLink",hFieldMap,hToolbar).
  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hToolbar).

  hBrwCompLink = DYNAMIC-FUNCTION("NewBrowse"
        ,brwCompLink:HANDLE
        ,100
        ,"TITLE|Company links"
        ,"JBoxAppModuleCompany"
         + ";!iJBoxAppModuleId"
       + ",JBoxCompany"
         + ";cCompanyName"
         + ";iJBoxCompanyId"
        ,"WHERE false"
       + ",FIRST JBoxCompany NO-LOCK OF JBoxAppModuleCompany"
        ,"").

  DYNAMIC-FUNCTION("CreateParentLink",hBrwCompLink,hBrowse,"iJBoxAppModuleId").

  hBrwMenuLink = DYNAMIC-FUNCTION("NewBrowse"
        ,BrwMenuLink:HANDLE
        ,100
        ,"TITLE|Sub-menu links"
        ,"JBoxAppModuleItem"
         + ";!iJBoxAppModuleItemId"
       + ",JBoxMenu"
         + ";cMenuLabel"
         + ";cMenuType"
         + ";!iJBoxMenuId"
       + ",JBoxMenuToMenu"
         + ";!iFromMenuId"
         + ";!iToMenuId"
       + ",buf1_JBoxMenu"
         + ";cMenuLabel|Parent menu"
        ,"WHERE false"
       + ",FIRST JBoxMenu NO-LOCK OF JBoxAppModuleItem"
       + ",FIRST JBoxMenuToMenu NO-LOCK WHERE iFromMenuId = JBoxMenu.iJBoxMenuId"
       + ",FIRST buf1_JBoxMenu NO-LOCK WHERE buf1_JBoxMenu.iJBoxMenuId = iToMenuId"
        ,"").
  

  DYNAMIC-FUNCTION("CreateParentLink",hBrwMenuLink,hBrowse,"iJBoxAppModuleId").

  hBrwProgramLink = DYNAMIC-FUNCTION("NewBrowse"
          ,brwProgramLink:HANDLE
          ,100
          ,"TITLE|Programs"
          ,"JBoxAppModuleItem"
           + ";!iJBoxAppModuleItemId"
           + ";!iJBoxAppModuleId"
           + ";!iJBoxAppProgramId"
           + ";cConfigParam1"
           + ";cConfigParam2"
           + ";cConfigParam3"
           + ";cConfigParam4"
           + ";cConfigParam5"
         + ",JBoxAppProgram" 
           + ";cFileName@1"
         + ",JBoxCompany" 
           + ";cCompanyName|Company@2"
          ,"WHERE false"
         + ",FIRST JBoxAppProgram NO-LOCK OF JBoxAppModuleItem"
         + ",FIRST JBoxCompany NO-LOCK OF JBoxAppModuleItem OUTER-JOIN"
          ,"").

  DYNAMIC-FUNCTION("CreateParentLink",hBrwProgramLink,hBrowse,"iJBoxAppModuleId").

  hTbProgramLink = DYNAMIC-FUNCTION("NewToolBar"
        ,tbProgramLink:HANDLE
        ,"File"
        ,"new;New company link,edit;Edit company link,delete;Delete company link"
        ,"border").
  DYNAMIC-FUNCTION("CreateObjectLink",hTbProgramLink,hBrwProgramLink).

  RUN InvokeMethod(hBrowse,"OpenQuery").

END.


DYNAMIC-FUNCTION("setAddMoveX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,"cCreatedBy,cModifiedBy,cModuleName,dCreated,dModified").
DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,
                 "brwModule," + hBrowse:NAME).
DYNAMIC-FUNCTION("setResizeXGroup", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,
                 33.33,"brwCompLink,brwMenuLink,brwProgramLink," + hBrwCompLink:NAME + "," + hBrwMenuLink:NAME + "," + hBrwProgramLink:NAME).
DYNAMIC-FUNCTION("setMoveXGroup", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,33.33,"brwMenuLink," + hBrwMenuLink:NAME).
DYNAMIC-FUNCTION("setMoveXGroup", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,66.66,
                 "brwProgramLink," + hBrwProgramLink:NAME + ",tbProgramLink," + DYNAMIC-FUNCTION("getToolBarNames",hTbProgramLink,"")).

DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,550,300,0,0).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LinkCompanyRecord C-Win 
PROCEDURE LinkCompanyRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cRowIdList  AS CHAR NO-UNDO.
DEF VAR cIdList     AS CHAR NO-UNDO.  
DEF VAR bOk         AS LOG  NO-UNDO.

cRowIdList = DYNAMIC-FUNCTION("getRowIdList",
                              "JBoxAppModuleCompany,JBoxCompany",       /* Buffer(list) for query */
                              "",              /* Name of buffer to fetch rowid from. Blank: Last buffer in bufferlist) */
                              "where iJBoxAppModuleId = " + STRING(hFieldMap:BUFFER-FIELD("iJBoxAppModuleId"):BUFFER-VALUE)
                            + ",FIRST JBoxCompany NO-LOCK OF JBoxAppModuleCompany"
                               ).

THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
RUN JBoxSelector.w (THIS-PROCEDURE,0,
                    "JBoxCompany"      
                    + ";iJBoxCompanyId"  
                    + ";cCompanyName"
                    ,"where true",
                    INPUT-OUTPUT cRowIdList,
                    "iJBoxCompanyId", /* Primary key */
                    INPUT-OUTPUT cIdList,
                    "","",
                    OUTPUT bOK).
THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

IF bOk THEN DO:
  IF NOT DYNAMIC-FUNCTION("runproc","jbadmin_editcompany_to_modules.p",
                          STRING(hFieldMap:BUFFER-FIELD("iJBoxAppModuleId"):BUFFER-VALUE) + ";" + cIdList,?) THEN
    DYNAMIC-FUNCTION("DoMessage",0,1,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil","").
  ELSE APPLY "value-changed" TO hBrowse.
END.
ELSE APPLY "entry" TO hBrowse.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LinkMenuesRecord C-Win 
PROCEDURE LinkMenuesRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cRowIdList  AS CHAR NO-UNDO.
DEF VAR cIdList     AS CHAR NO-UNDO.  
DEF VAR bOk         AS LOG  NO-UNDO.


cRowIdList = DYNAMIC-FUNCTION("getRowIdList",
                              "JBoxAppModuleItem,JBoxMenu",       /* Buffer(list) for query */
                              "",              /* Name of buffer to fetch rowid from. Blank: Last buffer in bufferlist) */
                              "where iJBoxAppModuleId = " + STRING(hFieldMap:BUFFER-FIELD("iJBoxAppModuleId"):BUFFER-VALUE)
                            + ",FIRST JBoxMenu NO-LOCK OF JBoxAppModuleItem WHERE LOOKUP(cMenuType,'menu-item,sub-menu') > 0"
                               ).

THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
RUN JBoxSelector.w (THIS-PROCEDURE,0,
                    "JBoxMenu"      
                    + ";cMenuLabel"
                    + ";!iJBoxMenuId"  
                    + ";cMenuType"
                  + ",JBoxMenuToMenu"
                    + ";!iFromMenuId"
                    + ";!iToMenuId"
                  + ",buf1_JBoxMenu"
                    + ";cMenuLabel|Parent menu"
                    ,"WHERE LOOKUP(cMenuType,'menu-item,sub-menu') > 0"
                   + ",FIRST JBoxMenuToMenu NO-LOCK WHERE iFromMenuId = JBoxMenu.iJBoxMenuId"
                   + ",FIRST buf1_JBoxMenu NO-LOCK WHERE buf1_JBoxMenu.iJBoxMenuId = iToMenuId",
                    INPUT-OUTPUT cRowIdList,
                    "iJBoxMenuId", /* Primary key */
                    INPUT-OUTPUT cIdList,
                    "","",
                    OUTPUT bOK).
THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

IF bOk THEN DO:
  IF NOT DYNAMIC-FUNCTION("runproc","jbadmin_editmenu_to_modules.p",
                          STRING(hFieldMap:BUFFER-FIELD("iJBoxAppModuleId"):BUFFER-VALUE) + ";" + cIdList,?) THEN
    DYNAMIC-FUNCTION("DoMessage",0,1,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil","").
  ELSE APPLY "value-changed" TO hBrowse.
END.
ELSE APPLY "entry" TO hBrowse.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LinkProgramsRecord C-Win 
PROCEDURE LinkProgramsRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cRowIdList  AS CHAR NO-UNDO.
DEF VAR cIdList     AS CHAR NO-UNDO.  
DEF VAR bOk         AS LOG  NO-UNDO.

cRowIdList = DYNAMIC-FUNCTION("getRowIdList",
                              "JBoxAppModuleItem,JBoxAppProgram",       /* Buffer(list) for query */
                              "",              /* Name of buffer to fetch rowid from. Blank: Last buffer in bufferlist) */
                              "WHERE iJBoxAppModuleId = " + STRING(hFieldMap:BUFFER-FIELD("iJBoxAppModuleId"):BUFFER-VALUE)
                            + " AND iJBoxCompanyId = 0"
                            + ",FIRST JBoxAppProgram NO-LOCK OF JBoxAppModuleItem"
                               ).

THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
RUN JBoxSelector.w (THIS-PROCEDURE,0,
                    "JBoxAppProgram"      
                    + ";cFileName"
                    + ";!iJBoxAppProgramId"  
                    ,"where true",
                    INPUT-OUTPUT cRowIdList,
                    "iJBoxAppProgramId", /* Primary key */
                    INPUT-OUTPUT cIdList,
                    "","",
                    OUTPUT bOK).
THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

IF bOk THEN DO:
  IF NOT DYNAMIC-FUNCTION("runproc","jbadmin_editmodule_to_program.p",
                          STRING(hFieldMap:BUFFER-FIELD("iJBoxAppModuleId"):BUFFER-VALUE) + ";" + cIdList,?) THEN
    DYNAMIC-FUNCTION("DoMessage",0,1,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil","").
  ELSE APPLY "value-changed" TO hBrowse.
END.
ELSE APPLY "entry" TO hBrowse.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NewRecord C-Win 
PROCEDURE NewRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF DYNAMIC-FUNCTION("getCurrentObject") = hTbProgramLink THEN DO:
  RUN JBoxDAppModuleLinkProps.w ("new",0,0,hFieldMap:BUFFER-FIELD("iJBoxAppModuleId"):BUFFER-VALUE,OUTPUT bOk).
  IF bOk THEN RUN InvokeMethod(hBrowse,"DisplayRecord").
END.
ELSE RUN SUPER.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AdjustBrowseColumns C-Win 
FUNCTION AdjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse     AS HANDLE,
    INPUT icBrowseName AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hColumn AS HANDLE NO-UNDO.
IF icBrowseName = "brwProgramLink" THEN DO:
  hColumn = DYNAMIC-FUNCTION("getBrowseColumn",ihBrowse,"cFileName").
  hColumn:WIDTH-PIXELS = 120.
  hColumn = DYNAMIC-FUNCTION("getBrowseColumn",ihBrowse,"cCompanyName").
  hColumn:WIDTH-PIXELS = 120.
  hColumn = DYNAMIC-FUNCTION("getBrowseColumn",ihBrowse,"cConfigParam1").
  hColumn:WIDTH-PIXELS = 120.
  hColumn = DYNAMIC-FUNCTION("getBrowseColumn",ihBrowse,"cConfigParam2").
  hColumn:WIDTH-PIXELS = 120.
  hColumn = DYNAMIC-FUNCTION("getBrowseColumn",ihBrowse,"cConfigParam3").
  hColumn:WIDTH-PIXELS = 120.
  hColumn = DYNAMIC-FUNCTION("getBrowseColumn",ihBrowse,"cConfigParam4").
  hColumn:WIDTH-PIXELS = 120.
  hColumn = DYNAMIC-FUNCTION("getBrowseColumn",ihBrowse,"cConfigParam5").
  hColumn:WIDTH-PIXELS = 120.
END.
  
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

