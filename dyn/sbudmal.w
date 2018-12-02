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

DEF VAR bOk          AS LOG    NO-UNDO.
DEF VAR ix           AS INT    NO-UNDO.
DEF VAR hBrowse      AS HANDLE NO-UNDO.
DEF VAR hToolbar     AS HANDLE NO-UNDO.
DEF VAR hFieldMap    AS HANDLE NO-UNDO.
DEF VAR hSearchField AS HANDLE NO-UNDO.
DEF VAR hTabFolder   AS HANDLE NO-UNDO.
DEF VAR hBrowse2     AS HANDLE NO-UNDO.
DEFINE VARIABLE iCL AS INTEGER NO-UNDO.

DEF VAR hbcAktiv    AS HANDLE NO-UNDO.

DEF VAR iFontWingdings    AS INT    NO-UNDO.
/*iFontWingdings = DYNAMIC-FUNCTION("setAppFont","Wingdings, size=11 Script=symbol","") NO-ERROR.*/

DEF VAR hCurrTabProc    AS HANDLE NO-UNDO.
DEF VAR hCurrTabFrame   AS HANDLE NO-UNDO.
DEF VAR hCurrTabQuery   AS HANDLE NO-UNDO.
DEF VAR iCurrTab        AS INT NO-UNDO.

DEF VAR iCurrentButikkNr AS INTEGER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rectWinToolbar rectFolder 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AdjustBrowseColumns C-Win 
FUNCTION AdjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse     AS HANDLE,
    INPUT icBrowseName AS CHAR) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InitializeResize C-Win 
FUNCTION InitializeResize RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD TabChanged C-Win 
FUNCTION TabChanged RETURNS LOGICAL
  ( INPUT iiTab AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE FILL-IN-20 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Aktivera för att kunna översätta" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .71 NO-UNDO.

DEFINE RECTANGLE rectFolder
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 204 BY 30.14.

DEFINE RECTANGLE rectWinToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9.8 BY .91.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     FILL-IN-20 AT ROW 1.24 COL 80 COLON-ALIGNED
     rectWinToolbar AT ROW 1.24 COL 196
     rectFolder AT ROW 2.29 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 206 BY 31.62.


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
         TITLE              = "Salgsbudsjett mal"
         HEIGHT             = 31.62
         WIDTH              = 206
         MAX-HEIGHT         = 53.24
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 53.24
         VIRTUAL-WIDTH      = 320
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
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
/* SETTINGS FOR FILL-IN FILL-IN-20 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-20:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Salgsbudsjett mal */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Salgsbudsjett mal */
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

DYNAMIC-FUNCTION("EmbedMe" IN SOURCE-PROCEDURE,THIS-PROCEDURE) NO-ERROR.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
  IF NOT DYNAMIC-FUNCTION("DoCleanUpObjects",THIS-PROCEDURE:CURRENT-WINDOW) THEN RETURN NO-APPLY.
  DYNAMIC-FUNCTION("SetCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
  PUBLISH "InvalidateHandle" (THIS-PROCEDURE).
  RUN disable_UI.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  {lng.i}

  &IF DEFINED(UIB_is_Running) NE 0 &THEN
    RUN InitializeObject.
  &ENDIF

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

{incl/wintrigg.i}
{incl/conttrigg.i hCurrTabQuery}

ON 'window-resized':U OF {&WINDOW-NAME} DO:
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").
  RUN MoveToTop IN hCurrTabProc NO-ERROR.
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
/*   DYNAMIC-FUNCTION("setAttribute",hBrowse,"basequery","WHERE iJBoxCompanyId = " + STRING(DYNAMIC-FUNCTION("getCompanyId")). */
/*   DYNAMIC-FUNCTION("setCurrentObject",hBrowse).                                                                             */
/*   RUN OpenQuery.                                                                                                            */

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
  ENABLE rectWinToolbar rectFolder 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeHead C-Win 
PROCEDURE InitializeHead :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  DYNAMIC-FUNCTION("NewObject",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE,"container").
  
/*   ASSIGN                                                                                                                                 */
/*     KampEierId:DELIMITER = "|"                                                                                                           */
/*     KampEierId:LIST-ITEM-PAIRS = "|0|" + DYNAMIC-FUNCTION("getFieldList","KampanjeEier;KampEierId|KampEierNavn;KampEierId","where true") */
/*   .                                                                                                                                      */

/*   hFieldMap = DYNAMIC-FUNCTION("NewFieldMap",      /* A fieldmap object holds extra info for display and input fields (fill-ins)       */
/*                                                      and their corresponding buffer columns return handle equals the buffer handle */  */
/*                   hBrowse:QUERY,                                                                                                       */
/*                   FRAME {&FRAME-NAME}:HANDLE,     /* Frame for the input/display fields (might not be the same frame as the browse) */ */
/*                   "",   /* Update columns in buffer */                                                                                 */
/*                     "",                           /* Corresponding input fields (fill-in..). blank if equal to update columns */       */
/*                   "EkstVPILevNr,FilId,Beskrivelse,ImportDato",        /* Additional buffer and displ.fields - not updateable*/         */
/*                     "",                           /* Corresponding fill-ins */                                                         */
/*                   "").                            /* other input widgets and lookup buttons for update fill-ins */                     */

/*   DYNAMIC-FUNCTION("CreateObjectLink",hFieldMap,hBrowse).  */


  DYNAMIC-FUNCTION("NewToolBar",
                    rectWinToolBar:HANDLE,          /* Rectangle to define coordinates for toolbar */
                    "Fil",                         /* Corresponding menu label - no menu if blank */
                    "close",
                    "right,enable").                      /* Misc - enable, maxborder.. */

END.

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
  DEF VAR iDefaultButikkNr AS INTEGER NO-UNDO.


  ASSIGN iCL = INTEGER(DYNAMIC-FUNCTION("getFieldValues","SysPara","WHERE SysHId = 5 and SysGr = 1 and ParaNr = 1","Parameter1")) NO-ERROR.


DO WITH FRAME {&FRAME-NAME}:
  DEF VAR hPage AS HANDLE NO-UNDO.

  DYNAMIC-FUNCTION("NewObject",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE,"procedure").

  /* Bruker tab folder i dll istedenfor ocx tab folder */
/*   IF SEARCH("controls.dll") NE ? THEN                                              */
/*     DYNAMIC-FUNCTION("setAttribute",SESSION,"tabFolderProg","JBoxJlwTabFolder.w"). */
  IF SEARCH("controls.dll") NE ? THEN
    DYNAMIC-FUNCTION("setAttribute",rectFolder:HANDLE,"tabFolderProg","JBoxJlwTabFolder.w").

  hTabFolder = DYNAMIC-FUNCTION("NewTabFolder",rectFolder:HANDLE).
  
  /* X and Y limits for move of widget are not yet set for the window. 
     Since we want the tabs to resize according to the window size these values must be set here and
     they must be exact the same as in setOrwWinSize (see InitResize function) 
     Here the values are set to not give any automatic move of widgets */
     
  DYNAMIC-FUNCTION("setMinXYmove",1000,1000). 

  /* Bygger først første side og knytter opp browser. */
  DYNAMIC-FUNCTION("InitPages" IN hTabFolder,
                   "Budsjettmal|sbudmalhode_brw.w"
                   ,?).
  /* Henter tilbake navigerings query (fra brows) */
  hBrowse  = DYNAMIC-FUNCTION("getPageQuery" IN hTabFolder,1).
  /* Setter denne som mater query for folderen */
                    
  DYNAMIC-FUNCTION("setParentQuery" IN hTabFolder,hBrowse).
  /* Deretter kan vi legge til neste side. */
  DYNAMIC-FUNCTION("addPage" IN hTabFolder,"Budsjettmal linjer","sbudmallinje_brw.w","").

  /*
  DYNAMIC-FUNCTION("buildFolder" IN hTabFolder).
  
  hBrowse  = DYNAMIC-FUNCTION("getPageQuery" IN hTabFolder,1).
  
  hPage = DYNAMIC-FUNCTION("getPageHandle" IN hTabFolder,2).
  DYNAMIC-FUNCTION("setQuery" IN hPage,hBrowse).
*/

  /*Setting some initializing values for tellehode_brw.w*/
  ASSIGN
    iDefaultButikkNr = INT(DYNAMIC-FUNCTION("getFieldValues","bruker","WHERE brukerid = " + QUOTER(DYNAMIC-FUNCTION("getASuserId")),"ButikkNr"))
    /*iDefaultButikkNr = INT(DYNAMIC-FUNCTION("getFieldValues","bruker","WHERE brukerid = " + QUOTER(userid('skotex')),"ButikkNr"))*/
    iDefaultButikkNr = IF iDefaultButikkNr = 0 THEN iCL ELSE iDefaultButikkNr 
  .
    
  PUBLISH 'setButikkNr' (iDefaultButikkNr).
  SUBSCRIBE PROCEDURE DYNAMIC-FUNCTION("getPageHandle" IN hTabFolder,1) TO 'setTransferBtn' ANYWHERE.
  
  InitializeResize().
  
  RUN InitializeHead.
  
  RUN InvokeMethod (hBrowse,'OpenQuery').

END.

DYNAMIC-FUNCTION("initTranslation",THIS-PROCEDURE:CURRENT-WINDOW).

DYNAMIC-FUNCTION("TabFolderChanged" IN hTabFolder,1).

THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RowDisplayBrowse C-Win 
PROCEDURE RowDisplayBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*   IF VALID-HANDLE(hbcAktiv) THEN                    */
/*   ASSIGN                                            */
/*     hbcAktiv:FONT      = iFontWingdings             */
/*     hbcAktiv:FORMAT    = CHR(254) + "/"  + CHR(168) */
/*   .                                                 */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setButikkNr C-Win 
PROCEDURE setButikkNr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipButikkNr AS INT NO-UNDO.

  iCurrentButikkNr = ipButikkNr.
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
/*   CASE icFieldName: */
/*     WHEN "" THEN    */
/*     DO:             */
/*     END.            */
/*   END CASE.         */
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AdjustBrowseColumns C-Win 
FUNCTION AdjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse     AS HANDLE,
    INPUT icBrowseName AS CHAR):
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
/* ihBrowse:MOVE-COLUM(6,5).                      */
/* hbcAktiv      = ihBrowse:GET-BROWSE-COLUMN(6). */

RETURN TRUE.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InitializeResize C-Win 
FUNCTION InitializeResize RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  - Create the splitbar and grab the widgets that should follow it's move.
            - Set resize rules for this frame 
            - Set resize rules for window (and load previous settings for size and pos)
    Notes:  
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:

  DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectBrowse,searchField," + hBrowse:NAME).
  DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectToolbar").
  DYNAMIC-FUNCTION("setAddMoveX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, DYNAMIC-FUNCTION("getToolbarNames",rectWinToolBar:HANDLE,"")).

  DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,900,400,0,0).
END.

RETURN TRUE.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION TabChanged C-Win 
FUNCTION TabChanged RETURNS LOGICAL
  ( INPUT iiTab AS INT ) :
/*------------------------------------------------------------------------------
  Purpose: We don't want to sync all tabfolders when changing the tab-folder
           Therefore we delete and re-establish links accordingly 
------------------------------------------------------------------------------*/
DEF VAR hCurrTabQuery AS HANDLE NO-UNDO.

IF iCurrTab NE 0 THEN
  DYNAMIC-FUNCTION("DeleteObjectLink",hBrowse,DYNAMIC-FUNCTION("getPageQuery" IN hTabFolder,iCurrTab)).

/* If the navigation query is on page 1 */
IF iiTab = 1 THEN DO:
  /* Refresh the row on page 1 in case any updates on subsequent tabs has changed something in the parent record */
  IF hBrowse:QUERY:GET-BUFFER-HANDLE(1):AVAIL AND iiTab NE 2 THEN
    DYNAMIC-FUNCTION("RefreshRowids",hBrowse,hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).
  RETURN NO.   
END. 

ASSIGN hCurrTabQuery = DYNAMIC-FUNCTION("getPageQuery" IN hTabFolder,iiTab)
       iCurrTab      = iiTab.

/* If the navigation query is on page 1 */
IF iCurrTab = 1 THEN
  /*DYNAMIC-FUNCTION("CreateOneToOneLink",hCurrTabQuery,hBrowse,"<child keyfld>[;parent keyfld][,<child keyfld..]")*/.
ELSE
  DYNAMIC-FUNCTION("CreateParentLink",hCurrTabQuery,hBrowse,"MalId").

RUN InvokeMethod(hBrowse,"DisplayRecord").

RETURN TRUE.


/*
IF iiTab = 1 THEN 
DO:
  ASSIGN hCurrTabProc  = DYNAMIC-FUNCTION("getPageHandle" IN hTabFolder,iiTab)
         hCurrTabFrame = DYNAMIC-FUNCTION("getPageFrame" IN hTabFolder,iiTab)
         iCurrTab      = iiTab.
  RETURN FALSE.
END.

IF iCurrTab NE 0 THEN
  DYNAMIC-FUNCTION("DeleteObjectLink",hBrowse,DYNAMIC-FUNCTION("getPageQuery" IN hTabFolder,iCurrTab)).
  
hCurrTabQuery = DYNAMIC-FUNCTION("getPageQuery" IN hTabFolder,iiTab).

ASSIGN hCurrTabProc  = DYNAMIC-FUNCTION("getPageHandle" IN hTabFolder,iiTab)
       hCurrTabFrame = DYNAMIC-FUNCTION("getPageFrame" IN hTabFolder,iiTab)
       iCurrTab      = iiTab.

RUN InvokeMethod (hCurrTabQuery,'OpenQuery').

DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
RUN DisplayRecord.

RETURN TRUE.
*/
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

