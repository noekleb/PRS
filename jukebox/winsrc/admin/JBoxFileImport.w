&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
/* Procedure Description
"Basic Window Template

Use this template to create a new window. Alter this default template or create new ones to accomodate your needs for different default sizes and/or attributes."
*/
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*********************************************************************
* Copyright (C) 2001 by Progress Software Corporation ("PSC"),       *
* 14 Oak Park, Bedford, MA 01730, and other contributors as listed   *
* below.  All Rights Reserved.                                       *
*                                                                    *
* The Initial Developer of the Original Code is PSC.  The Original   *
* Code is Progress IDE code released to open source December 1, 2000.*
*                                                                    *
* The contents of this file are subject to the Possenet Public       *
* License Version 1.0 (the "License"); you may not use this file     *
* except in compliance with the License.  A copy of the License is   *
* available as of the date of this notice at                         *
* http://www.possenet.org/license.html                               *
*                                                                    *
* Software distributed under the License is distributed on an "AS IS"*
* basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. You*
* should refer to the License for the specific language governing    *
* rights and limitations under the License.                          *
*                                                                    *
* Contributors:                                                      *
*                                                                    *
*********************************************************************/
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

DEF VAR bOK             AS LOG NO-UNDO.
DEF VAR ix              AS INT NO-UNDO.

DEF VAR hToolbar        AS HANDLE NO-UNDO.
DEF VAR hBrowse         AS HANDLE NO-UNDO.
DEF VAR hSearchField    AS HANDLE NO-UNDO.
DEF VAR hTabFolder      AS HANDLE NO-UNDO.
 
DEF VAR hCurrTabProc    AS HANDLE NO-UNDO.
DEF VAR hCurrTabFrame   AS HANDLE NO-UNDO.
DEF VAR iCurrTab        AS INT    NO-UNDO.
DEF VAR cMenuCatParam   AS CHAR   NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rectFolder rectBrowse rectToolBar ~
searchField 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getMenuCatParameter C-Win 
FUNCTION getMenuCatParameter RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

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
DEFINE RECTANGLE rectBrowse
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 38.6 BY 19.14.

DEFINE RECTANGLE rectFolder
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 94.4 BY 19.14.

DEFINE RECTANGLE rectToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 25 BY .95.

DEFINE RECTANGLE searchField
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 19 BY .95.

DEFINE BUTTON btnSplitBarX 
     IMAGE-UP FILE "bmp/tabright.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 1.6 BY 19.05
     BGCOLOR 12 FGCOLOR 12 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     rectFolder AT ROW 2.57 COL 42.6
     rectBrowse AT ROW 2.57 COL 1.2
     rectToolBar AT ROW 1.29 COL 42.4
     searchField AT ROW 1.24 COL 2.2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 136.8 BY 20.76.

DEFINE FRAME frSplitBarX
     btnSplitBarX AT ROW 1 COL 9.4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 31.6 ROW 2.62
         SIZE 23.2 BY 19.1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window Template
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "File import management"
         HEIGHT             = 20.76
         WIDTH              = 136.8
         MAX-HEIGHT         = 52.38
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 52.38
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

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME frSplitBarX
/* Query rebuild information for FRAME frSplitBarX
     _Query            is NOT OPENED
*/  /* FRAME frSplitBarX */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* File import management */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* File import management */
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
  DEF VAR hFrame AS HANDLE NO-UNDO.

  DYNAMIC-FUNCTION("DoLockWindow",THIS-PROCEDURE:CURRENT-WINDOW).
  DYNAMIC-FUNCTION("setSplitBarX" , THIS-PROCEDURE:CURRENT-WINDOW, btnSplitBarX:HANDLE IN FRAME frSplitBarX,NO).
  hFrame = DYNAMIC-FUNCTION("getPageFrame" IN hTabFolder,DYNAMIC-FUNCTION("getCurrentTab" IN hTabFolder)).
  IF VALID-HANDLE(hFrame) THEN 
    hFrame:MOVE-TO-TOP().
  DYNAMIC-FUNCTION("DoLockWindow",?).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
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

  cMenuCatParam = DYNAMIC-FUNCTION("getParameter" IN SOURCE-PROCEDURE) NO-ERROR.
  &IF DEFINED(UIB_is_Running) NE 0 &THEN
    RUN InitializeObject.
    RUN MoveToTop.
  &ENDIF

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

{incl/wintrigg.i}
{incl/conttrigg.i hBrowse}

ON 'window-resized':U OF {&WINDOW-NAME} DO:
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").

  RUN MoveToTop IN hCurrTabProc NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
    hCurrTabFrame:MOVE-TO-TOP().
END.


ON 'alt-1':U OF FRAME {&FRAME-NAME} ANYWHERE DO:
  DYNAMIC-FUNCTION("TabFolderChanged" IN hTabFolder,1).
  RUN MoveToTop IN hCurrTabProc NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
    hCurrTabFrame:MOVE-TO-TOP().
  RETURN.
END.

ON 'alt-2':U OF FRAME {&FRAME-NAME} ANYWHERE DO:
  DYNAMIC-FUNCTION("TabFolderChanged" IN hTabFolder,2).
  RUN MoveToTop IN hCurrTabProc NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
    hCurrTabFrame:MOVE-TO-TOP().
  RETURN.
END.

ON 'alt-3':U OF FRAME {&FRAME-NAME} ANYWHERE DO:
  DYNAMIC-FUNCTION("TabFolderChanged" IN hTabFolder,3).
  RUN MoveToTop IN hCurrTabProc NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
    hCurrTabFrame:MOVE-TO-TOP().
  RETURN.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplayRecord C-Win 
PROCEDURE DisplayRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN SUPER.

DYNAMIC-FUNCTION("DoMyPublish" IN DYNAMIC-FUNCTION("getPageHandle" IN hTabFolder,1)).

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
  ENABLE rectFolder rectBrowse rectToolBar searchField 
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
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hPageObject AS HANDLE NO-UNDO.
  
RUN enable_UI.

DYNAMIC-FUNCTION("CreateStatusBar",THIS-PROCEDURE:CURRENT-WINDOW,"",0,YES,?).

DO WITH FRAME {&FRAME-NAME}:
  DYNAMIC-FUNCTION("NewObject",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE,"container").

  hBrowse = DYNAMIC-FUNCTION("NewBrowse", 
                    rectBrowse:HANDLE,
                    100, 
                    "", 
                    "JBoxFileImportHeader;cDescription;cImportProg;cTargetDBtable;cDelimiter;iHeaderLines;cImportType;cComment"
                    + ";cDataSourceCatalog;cMoveImportedFilesTo;bLogFilesInDB;bIgnoreDuplicates"
                    + ";!iJBoxFileImportHeaderId"
                    + ";!bAllowRecordDeleteFromImportLog"
                   ,"WHERE false", 
                    "sort|cDescription").  
  hBrowse:NAME = "brwImportHeader".
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"baseQuery",
                   "WHERE iJBoxCompanyId = " + DYNAMIC-FUNCTION("getCompany")
                 + " AND cMenuCategory = '" + cMenuCatParam + "'"
                   ).

  hSearchField = DYNAMIC-FUNCTION("NewBrowseSearchField",SearchField:HANDLE,hBrowse,1).
  DYNAMIC-FUNCTION("CreateObjectLink",hSearchField,hBrowse).

  DYNAMIC-FUNCTION("createObjectLink",hBrowse,THIS-PROCEDURE).

  hTabFolder = DYNAMIC-FUNCTION("NewTabFolder",rectFolder:HANDLE).
  
  /* X and Y limits for move of widget are not yet set for the window. 
     Since we want the tabs to resize according to the window size these values must be provided here and
     they must be exact the same as in setOrwWinSize (under) */
     
  DYNAMIC-FUNCTION("setMinXYmove",2000,1200). 

  DYNAMIC-FUNCTION("InitPages" IN hTabFolder
                   ,"Import header|JBoxFileImportHeaderView.w|File spec|JBoxFileImportSpecBrw.w|Import log|JBoxFileImportLogBrw.w"
                   ,hBrowse).
  
  DYNAMIC-FUNCTION("buildFolder" IN hTabFolder).

  hToolbar = DYNAMIC-FUNCTION("NewToolBar",
                    rectToolBar:HANDLE,   
                    "File",               
                    "new,copy,undo,delete,save,excel",
                    "maxborder"). 


  DYNAMIC-FUNCTION("setCompanyHeader",THIS-PROCEDURE:CURRENT-WINDOW).

  InitializeResize().

  RUN InvokeMethod(hBrowse,"OpenQuery").

END.

DYNAMIC-FUNCTION("initTranslation",THIS-PROCEDURE:CURRENT-WINDOW).
DYNAMIC-FUNCTION("TabFolderChanged" IN hTabFolder,1).


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
THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.
{&WINDOW-NAME}:WINDOW-STATE = 3.
{&WINDOW-NAME}:MOVE-TO-TOP().
APPLY "entry" TO hBrowse.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getMenuCatParameter C-Win 
FUNCTION getMenuCatParameter RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
RETURN cMenuCatParam.

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
DO WITH FRAME {&FRAME-NAME}:
  DYNAMIC-FUNCTION("setSplitBarX" , THIS-PROCEDURE:CURRENT-WINDOW, btnSplitBarX:HANDLE IN FRAME frSplitBarX,NO).

  DYNAMIC-FUNCTION("setFollowSplitBarX",THIS-PROCEDURE:CURRENT-WINDOW,btnSplitBarX:HANDLE IN FRAME frSplitBarX, 
                    DYNAMIC-FUNCTION("getToolBarHandles",hToolBar,"button,rule") + "," +
                    DYNAMIC-FUNCTION("getWidgetsByLasso",rectBrowse:HANDLE IN FRAME {&FRAME-NAME},0,"frame,control-frame,rectangle,browse") + "," +
                    DYNAMIC-FUNCTION("getWidgetsByLasso",rectFolder:HANDLE IN FRAME {&FRAME-NAME},0,"frame,browse,control-frame,rectangle,editor,fill-in")
                    ).
  DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectBrowse,brwImportHeader").
  DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectToolbar").
  DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,700,300,0,350).
END.
  
RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION TabChanged C-Win 
FUNCTION TabChanged RETURNS LOGICAL
  ( INPUT iiTab AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DYNAMIC-FUNCTION("DeleteLinksFrom",hToolbar).
DYNAMIC-FUNCTION("DeleteLinksFrom",hBrowse).

DYNAMIC-FUNCTION("setAttribute",hToolbar,"disabledevents","").

CASE iiTab:
  WHEN 1 THEN DO:
    DYNAMIC-FUNCTION("CreateOneToOneLink",DYNAMIC-FUNCTION("getPageQuery" IN hTabFolder,1),hBrowse,"iJBoxFileImportHeaderId").
    DYNAMIC-FUNCTION("CreateObjectLink",hToolbar,DYNAMIC-FUNCTION("getPageQuery" IN hTabFolder,1)).
    DYNAMIC-FUNCTION("CreateObjectLink",hToolbar,DYNAMIC-FUNCTION("getPageFieldMap" IN hTabFolder,1)).
  END.
  WHEN 2 THEN DO:
    DYNAMIC-FUNCTION("CreateParentLink",DYNAMIC-FUNCTION("getPageQuery" IN hTabFolder,2),hBrowse,"iJBoxFileImportHeaderId").
    DYNAMIC-FUNCTION("CreateObjectLink",hToolbar,DYNAMIC-FUNCTION("getPageQuery" IN hTabFolder,2)).
  END.
  WHEN 3 THEN DO:
    DYNAMIC-FUNCTION("CreateParentLink",DYNAMIC-FUNCTION("getPageQuery" IN hTabFolder,3),hBrowse,"iJBoxFileImportHeaderId").
    DYNAMIC-FUNCTION("CreateObjectLink",hToolbar,DYNAMIC-FUNCTION("getPageQuery" IN hTabFolder,3)).
    DYNAMIC-FUNCTION("setAttribute",hToolbar,"disabledevents","new,copy").
  END.
END CASE.

hCurrTabProc  = DYNAMIC-FUNCTION("getPageHandle" IN hTabFolder,iiTab).
hCurrTabFrame = DYNAMIC-FUNCTION("getPageFrame" IN hTabFolder,iiTab).

DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
RUN DisplayRecord.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

