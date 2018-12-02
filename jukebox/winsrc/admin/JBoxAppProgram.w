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
DEF VAR hBrwModuleLink  AS HANDLE NO-UNDO.
DEF VAR hTbModuleLink   AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnImage brwAppProgram tbAppProgram ~
brwModules brwSearch tbModuleLink cFileName cFileType cDescription ~
iAltAppProgramId cConfigParam1 cConfigParam2 cConfigParam3 cConfigParam4 ~
cConfigParam5 dCreated cCreatedBy dModified cModifiedBy 
&Scoped-Define DISPLAYED-OBJECTS cFileName cFileType cDescription ~
iAltAppProgramId cConfigParam1 cConfigParam2 cConfigParam3 cConfigParam4 ~
cConfigParam5 dCreated cCreatedBy dModified cModifiedBy 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AdjustBrowseColumns C-Win 
FUNCTION AdjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse     AS HANDLE,
    INPUT icBrowseName AS CHAR)  FORWARD.

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
DEFINE BUTTON btnImage 
     IMAGE-UP FILE "bmp/open16e.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Image file name" 
     SIZE 4 BY 1.

DEFINE VARIABLE cFileType AS CHARACTER FORMAT "x(15)" 
     LABEL "Type" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEMS "Container","Suppressed" 
     DROP-DOWN-LIST
     SIZE 21.6 BY 1.

DEFINE VARIABLE iAltAppProgramId AS INTEGER FORMAT "->,>>>,>>9" INITIAL 0 
     LABEL "Alternative" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "",0
     DROP-DOWN-LIST
     SIZE 48 BY 1.

DEFINE VARIABLE cConfigParam1 AS CHARACTER FORMAT "x(80)" 
     LABEL "Config param 1" 
     VIEW-AS FILL-IN 
     SIZE 82 BY 1.

DEFINE VARIABLE cConfigParam2 AS CHARACTER FORMAT "x(80)" 
     LABEL "Config param 2" 
     VIEW-AS FILL-IN 
     SIZE 82 BY 1.

DEFINE VARIABLE cConfigParam3 AS CHARACTER FORMAT "x(80)" 
     LABEL "Config param 3" 
     VIEW-AS FILL-IN 
     SIZE 82 BY 1.

DEFINE VARIABLE cConfigParam4 AS CHARACTER FORMAT "x(80)" 
     LABEL "Config param 4" 
     VIEW-AS FILL-IN 
     SIZE 82 BY 1.

DEFINE VARIABLE cConfigParam5 AS CHARACTER FORMAT "x(80)" 
     LABEL "Config Param 5" 
     VIEW-AS FILL-IN 
     SIZE 82 BY 1.

DEFINE VARIABLE cCreatedBy AS CHARACTER FORMAT "x(8)" 
     LABEL "Av" 
     VIEW-AS FILL-IN 
     SIZE 13.6 BY 1.

DEFINE VARIABLE cDescription AS CHARACTER FORMAT "x(60)" 
     LABEL "Description" 
     VIEW-AS FILL-IN 
     SIZE 62 BY 1.

DEFINE VARIABLE cFileName AS CHARACTER FORMAT "x(40)" 
     LABEL "File name" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1.

DEFINE VARIABLE cModifiedBy AS CHARACTER FORMAT "x(8)" 
     LABEL "Av" 
     VIEW-AS FILL-IN 
     SIZE 13.6 BY 1.

DEFINE VARIABLE dCreated AS DATE FORMAT "99/99/9999" 
     LABEL "Opprettet" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE dModified AS DATE FORMAT "99/99/9999" 
     LABEL "Endret" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE RECTANGLE brwAppProgram
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 141 BY 9.76.

DEFINE RECTANGLE brwModules
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 40 BY 9.76.

DEFINE RECTANGLE brwSearch
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 18.4 BY .91.

DEFINE RECTANGLE tbAppProgram
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 16 BY .91.

DEFINE RECTANGLE tbModuleLink
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 9.8 BY .91.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnImage AT ROW 13.05 COL 61.4
     cFileName AT ROW 13.05 COL 17 COLON-ALIGNED
     cFileType AT ROW 14.05 COL 17 COLON-ALIGNED
     cDescription AT ROW 15.05 COL 17 COLON-ALIGNED
     iAltAppProgramId AT ROW 16.24 COL 17 COLON-ALIGNED
     cConfigParam1 AT ROW 17.48 COL 3.6
     cConfigParam2 AT ROW 18.48 COL 3.6
     cConfigParam3 AT ROW 19.48 COL 3.6
     cConfigParam4 AT ROW 20.48 COL 3.6
     cConfigParam5 AT ROW 21.48 COL 3.4
     dCreated AT ROW 22.91 COL 17 COLON-ALIGNED
     cCreatedBy AT ROW 22.91 COL 37.8 COLON-ALIGNED
     dModified AT ROW 22.91 COL 64.2 COLON-ALIGNED
     cModifiedBy AT ROW 22.91 COL 85.4 COLON-ALIGNED
     brwAppProgram AT ROW 2.67 COL 2
     tbAppProgram AT ROW 1.48 COL 22.4
     brwModules AT ROW 12.91 COL 103
     brwSearch AT ROW 1.48 COL 2.6
     tbModuleLink AT ROW 22.95 COL 103
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 142.6 BY 23.14.


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
         TITLE              = "Application programs"
         HEIGHT             = 23.14
         WIDTH              = 142.6
         MAX-HEIGHT         = 23.52
         MAX-WIDTH          = 142.6
         VIRTUAL-HEIGHT     = 23.52
         VIRTUAL-WIDTH      = 142.6
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
       FRAME DEFAULT-FRAME:HEIGHT           = 23.14
       FRAME DEFAULT-FRAME:WIDTH            = 142.6.

/* SETTINGS FOR FILL-IN cConfigParam1 IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN cConfigParam2 IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN cConfigParam3 IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN cConfigParam4 IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN cConfigParam5 IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Application programs */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Application programs */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnImage
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnImage C-Win
ON CHOOSE OF btnImage IN FRAME DEFAULT-FRAME /* Image file name */
DO:
  DEF VAR cProgram AS CHAR NO-UNDO.
  SYSTEM-DIALOG GET-FILE cProgram 
                FILTERS "Launch files" "*.*" 
                MUST-EXIST
                UPDATE bOk.
  ValidateProgram(cProgram,cFileName:HANDLE).  
  THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
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
IF DYNAMIC-FUNCTION("getCurrentObject") = hBrwModuleLink THEN
  RUN InvokeMethod(hTbModuleLink,"EditRecord").
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
IF DYNAMIC-FUNCTION("getCurrentObject") = hTbModuleLink THEN DO:
  RUN JBoxDAppModuleLinkProps.w ("edit",
                                 hBrwModuleLink:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("iJBoxAppModuleItemId"):BUFFER-VALUE,
                                 hFieldMap:BUFFER-FIELD("iJBoxAppProgramId"):BUFFER-VALUE,
                                 0,
                                 OUTPUT bOk).
  IF bOk THEN DYNAMIC-FUNCTION("RefreshRowids",hBrwModuleLink,hBrwModuleLink:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).
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
  DISPLAY cFileName cFileType cDescription iAltAppProgramId cConfigParam1 
          cConfigParam2 cConfigParam3 cConfigParam4 cConfigParam5 dCreated 
          cCreatedBy dModified cModifiedBy 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btnImage brwAppProgram tbAppProgram brwModules brwSearch tbModuleLink 
         cFileName cFileType cDescription iAltAppProgramId cConfigParam1 
         cConfigParam2 cConfigParam3 cConfigParam4 cConfigParam5 dCreated 
         cCreatedBy dModified cModifiedBy 
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
DEF VAR hSearchField AS HANDLE NO-UNDO.
RUN enable_UI.

DYNAMIC-FUNCTION("CreateStatusBar",THIS-PROCEDURE:CURRENT-WINDOW,"",0,YES,?).

DO WITH FRAME {&FRAME-NAME}:
  ASSIGN iAltAppProgramId:DELIMITER = "|"
         iAltAppProgramId:LIST-ITEM-PAIRS = RIGHT-TRIM("|0|" + 
                                                       DYNAMIC-FUNCTION("getFieldList","JBoxAppProgram;cFileName;iJBoxAppProgramId","WHERE true")
                                                       ,"|")
         .

  hBrowse = DYNAMIC-FUNCTION("NewBrowse"
          ,brwAppProgram:HANDLE
          ,100
          ,""
          ,"JBoxAppProgram"
            + ";cFileName"
            + ";cFileType"
            + ";cDescription"
            + ";cConfigParam1"
            + ";cConfigParam2"
            + ";cConfigParam3"
            + ";cConfigParam4"
            + ";cConfigParam5"
            + ";dCreated"
            + ";cCreatedBy"
            + ";dModified"
            + ";cModifiedBy"
            + ";!iAltAppProgramId"
            + ";!iJBoxAppProgramId"
          ,"WHERE false"
          ,"").

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"filterdropdownfields_cModuleName","JBoxAppModule;cModuleName;cModuleName").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"filterdropdownquery_cModuleName","where true BY cModuleName").

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"extraFilterFields",
                   "JBoxAppModule.cModuleName|Module").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"prescanqueryJBoxAppModule","EACH JBoxAppModuleItem OF JBoxAppModule NO-LOCK,FIRST JBoxAppProgram OF JBoxAppModuleItem NO-LOCK"). 

  DYNAMIC-FUNCTION("setSortString",hBrowse,"cFileName").

  hSearchField = DYNAMIC-FUNCTION("NewBrowseSearchField",BrwSearch:HANDLE,hBrowse,1).
  DYNAMIC-FUNCTION("createObjectLink",hBrowse,hSearchField).

  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap"
          ,hBrowse:QUERY
          ,FRAME {&FRAME-NAME}:HANDLE
          ,"cFileName,cFileType,iAltAppProgramId,cConfigParam1,cConfigParam2,cConfigParam3,cConfigParam4,cConfigParam5,cDescription",""
          ,"cCreatedBy,cModifiedBy,dCreated,dModified",""
          ,"").

  DYNAMIC-FUNCTION("CreateObjectLink",hFieldMap,hBrowse).


  hToolbar = DYNAMIC-FUNCTION("NewToolBar"
          ,TbAppProgram:HANDLE
          ,"File"
          ,"new,undo,delete,save,filter,flatview,excel;Eksporter til E&xcel"
         + ",-,LinkModule;Link to Module;Link to module for default (0) company"
          ,"maxborder").

  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hToolbar).
  DYNAMIC-FUNCTION("CreateObjectLink",hFieldMap,hToolbar).

  hBrwModuleLink = DYNAMIC-FUNCTION("NewBrowse"
          ,brwModules:HANDLE
          ,100
          ,""
          ,"JBoxAppModuleItem"
           + ";!iJBoxAppModuleItemId"
           + ";!iJBoxAppModuleId"
           + ";!iJBoxAppProgramId"
           + ";cConfigParam1"
           + ";cConfigParam2"
           + ";cConfigParam3"
           + ";cConfigParam4"
           + ";cConfigParam5"
         + ",JBoxAppModule" 
           + ";cModuleName@1"
         + ",JBoxCompany" 
           + ";cCompanyName|Company@2"
          ,"WHERE false"
         + ",FIRST JBoxAppModule NO-LOCK OF JBoxAppModuleItem"
         + ",FIRST JBoxCompany NO-LOCK OF JBoxAppModuleItem OUTER-JOIN"
          ,"").

  DYNAMIC-FUNCTION("CreateParentLink",hBrwModuleLink,hBrowse,"iJBoxAppProgramId").

  hTbModuleLink = DYNAMIC-FUNCTION("NewToolBar"
        ,tbModuleLink:HANDLE
        ,"File"
        ,"new;New company link,edit;Edit company link,delete;Delete company link"
        ,"border").
  DYNAMIC-FUNCTION("CreateObjectLink",hTbModuleLink,hBrwModuleLink).

END.

DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,
                 "brwModules," + hBrwModuleLink:NAME).

DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,600,200,0,0).
  
RUN InvokeMethod(hBrowse,"OpenQuery").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LinkModuleRecord C-Win 
PROCEDURE LinkModuleRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cRowIdList  AS CHAR NO-UNDO.
DEF VAR cIdList     AS CHAR NO-UNDO.  
DEF VAR bOk         AS LOG  NO-UNDO.

cRowIdList = DYNAMIC-FUNCTION("getRowIdList",
                              "JBoxAppModuleItem,JBoxAppModule",       /* Buffer(list) for query */
                              "",              /* Name of buffer to fetch rowid from. Blank: Last buffer in bufferlist) */
                              "WHERE iJBoxAppProgramId = " + STRING(hFieldMap:BUFFER-FIELD("iJBoxAppProgramId"):BUFFER-VALUE)
                            + " AND iJBoxCompanyId = 0"
                            + ",FIRST JBoxAppModule NO-LOCK OF JBoxAppModuleItem"
                               ).

THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
RUN JBoxSelector.w (THIS-PROCEDURE,0,
                    "JBoxAppModule"      
                    + ";cModuleName"
                    + ";!iJBoxAppModuleId"  
                    ,"where true",
                    INPUT-OUTPUT cRowIdList,
                    "iJBoxAppModuleId", /* Primary key */
                    INPUT-OUTPUT cIdList,
                    "","",
                    OUTPUT bOK).
THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

IF bOk THEN DO:
  IF NOT DYNAMIC-FUNCTION("runproc","jbadmin_editprogram_to_module.p",
                          STRING(hFieldMap:BUFFER-FIELD("iJBoxAppProgramId"):BUFFER-VALUE) + ";" + cIdList,?) THEN
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
IF DYNAMIC-FUNCTION("getCurrentObject") = hTbModuleLink THEN DO:
  RUN JBoxDAppModuleLinkProps.w ("new",0,hFieldMap:BUFFER-FIELD("iJBoxAppProgramId"):BUFFER-VALUE,0,OUTPUT bOk).
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
    INPUT icBrowseName AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hColumn AS HANDLE NO-UNDO.
IF icBrowseName = "brwModules" THEN DO:
  hColumn = DYNAMIC-FUNCTION("getBrowseColumn",ihBrowse,"cModuleName").
  hColumn:WIDTH-PIXELS = 120.
END.

RETURN FALSE.   /* Function return value. */

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
END.
  
RETURN YES.
  
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

