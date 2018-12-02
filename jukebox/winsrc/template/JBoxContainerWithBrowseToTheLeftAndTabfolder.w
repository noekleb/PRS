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

  File:             CustOrders.w

  Description:      JukeBox sample program for nav.query and detail window usage with tab-folder

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author:           Brynjar Hasle, Chemistry as

  Created:          06.10.05

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
&IF "{1}" = "Developer_Studio_is_Running" &THEN
  &SCOPED-DEFINE UIB_is_Running 1 
&ENDIF   
/* Uncomment to enable use of .Net components: */
/* &SCOPED-DEFINE AdvGuiWin */ 

/* Uncomment to enable use of .Net components: */
/* &SCOPED-DEFINE AdvGuiWin */ 

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
DEF VAR hCurrTabQuery   AS HANDLE NO-UNDO.
DEF VAR iCurrTab        AS INT NO-UNDO.
DEF VAR iStatus         AS INT NO-UNDO.

{JukeBoxGradient.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME CustomerFrame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rectTabFolder brwBrowse NavToolbar ~
searchField 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

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
DEFINE RECTANGLE brwBrowse
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 38.6 BY 22.71.

DEFINE RECTANGLE NavToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 15.4 BY .95.

DEFINE RECTANGLE rectTabFolder
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 116.4 BY 22.62.

DEFINE RECTANGLE searchField
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 19 BY .95.

DEFINE BUTTON btnSplitBarX 
     IMAGE-UP FILE "bmp/tabright.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE .8 BY 22.62
     BGCOLOR 12 FGCOLOR 12 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME CustomerFrame
     rectTabFolder AT ROW 2.67 COL 42.6
     brwBrowse AT ROW 2.57 COL 1.2
     NavToolbar AT ROW 1.24 COL 22
     searchField AT ROW 1.24 COL 2.2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 158.8 BY 24.33.

DEFINE FRAME frSplitBarX
     btnSplitBarX AT ROW 1 COL 9.2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 31.6 ROW 2.62
         SCROLLABLE SIZE 18.4 BY 22.67.


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
         TITLE              = "Container sample"
         HEIGHT             = 24.38
         WIDTH              = 158.8
         MAX-HEIGHT         = 57.14
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 57.14
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
ASSIGN FRAME frSplitBarX:FRAME = FRAME CustomerFrame:HANDLE.

/* SETTINGS FOR FRAME CustomerFrame
   FRAME-NAME                                                           */
ASSIGN 
       FRAME CustomerFrame:HEIGHT           = 24.33
       FRAME CustomerFrame:WIDTH            = 158.8.

/* SETTINGS FOR FRAME frSplitBarX
                                                                        */
ASSIGN 
       FRAME frSplitBarX:HEIGHT           = 22.67
       FRAME frSplitBarX:WIDTH            = 18.4.

ASSIGN 
       btnSplitBarX:MOVABLE IN FRAME frSplitBarX          = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME CustomerFrame
/* Query rebuild information for FRAME CustomerFrame
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME CustomerFrame */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME frSplitBarX
/* Query rebuild information for FRAME frSplitBarX
     _Query            is NOT OPENED
*/  /* FRAME frSplitBarX */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Container sample */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Container sample */
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
  RUN MoveToTop IN hCurrTabProc NO-ERROR.

  DYNAMIC-FUNCTION("ResizeKeepsWindowLocked",?).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSplitBarX C-Win
ON START-MOVE OF btnSplitBarX IN FRAME frSplitBarX
DO:
  DYNAMIC-FUNCTION("ResizeKeepsWindowLocked",THIS-PROCEDURE:CURRENT-WINDOW).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME CustomerFrame
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
  PUBLISH "InvalidateHandle" (THIS-PROCEDURE).
  DYNAMIC-FUNCTION("setCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
  RUN disable_UI.
END.

{incl/wintrigg.i}
{incl/conttrigg.i hCurrTabQuery} 

ON 'window-resized':U OF {&WINDOW-NAME} DO:
  DYNAMIC-FUNCTION("ResizeKeepsWindowLocked",THIS-PROCEDURE:CURRENT-WINDOW).
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").

  RUN MoveToTop IN hCurrTabProc NO-ERROR.

  DYNAMIC-FUNCTION("ResizeKeepsWindowLocked",?).
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:


  &IF DEFINED(UIB_is_Running) NE 0 &THEN
  
    DYNAMIC-FUNCTION("BuildTableCache","JBoxUserSetting|WHERE cJBoxUserId = '" + DYNAMIC-FUNCTION("getASuserId") + "'"
                   + ";jbserv_getfieldcache.p").
    RUN InitializeObject.
    RUN MoveToTop.
    IF DYNAMIC-FUNCTION("getAttribute",THIS-PROCEDURE,"advGuiWin") = "YES" THEN
      RUN WaitForForm NO-ERROR.
  &ENDIF


  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addTabRecord C-Win 
PROCEDURE addTabRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Not compatible with .Net UI components (24.09.13)  
------------------------------------------------------------------------------*/
DEF VAR iNewTab# AS INT NO-UNDO.

iNewTab# = DYNAMIC-FUNCTION("addPage" IN hTabFolder,"Invoices","InvoiceBrw.w","bullet.bmp").

DYNAMIC-FUNCTION("setFollowSplitBarX",THIS-PROCEDURE:CURRENT-WINDOW,btnSplitBarX:HANDLE IN FRAME frSplitBarX, 
                 "+," + DYNAMIC-FUNCTION("getPageFrame" IN hTabFolder,iNewTab#) + "," + DYNAMIC-FUNCTION("getPageQuery" IN hTabFolder,iNewTab#)
                 ).

DYNAMIC-FUNCTION("TabFolderChanged" IN hTabFolder,3).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deleteTabRecord C-Win 
PROCEDURE deleteTabRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iLastTab# AS INT NO-UNDO.

iLastTab# = DYNAMIC-FUNCTION("deletePage" IN hTabFolder,0,"InvoiceBrw.w").

DYNAMIC-FUNCTION("TabFolderChanged" IN hTabFolder,iLastTab#).
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
  ENABLE rectTabFolder brwBrowse NavToolbar searchField 
      WITH FRAME CustomerFrame IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-CustomerFrame}
  ENABLE btnSplitBarX 
      WITH FRAME frSplitBarX IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frSplitBarX}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExtraFlatViewRecord C-Win 
PROCEDURE ExtraFlatViewRecord :
/*------------------------------------------------------------------------------
  Purpose:    Set initial parameters for the browse 
  Parameters:  <none>
  Notes:      The browse itself is not created yet so the handle we get here is really to the placeholder (rectangle).
              (Hence we cannot access the Progress attributes and methods.)
              Attributes set on the placeholder will be moved to the browse once created.                
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihFlatView   AS HANDLE NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO INIT TRUE.

DEF VAR hFlatBrw    AS HANDLE NO-UNDO.

hFlatBrw  = DYNAMIC-FUNCTION("getBrowseHandle" IN ihFlatView).

/* Enable accumulation pr week for orderdata: */
DYNAMIC-FUNCTION("setAttribute",hFlatBrw,"addperiodOrderdate","mq"). /* Possible: wmqy (week,month,quarter,year) */

/* Change the filter prescan query definition for salesrep joined from order, orderline and item. Queries must pass through order to get to customer: */
DYNAMIC-FUNCTION("setAttribute",hFlatBrw,"prescanqueryBuf1_SalesRep","EACH Order OF buf1_SalesRep NO-LOCK,FIRST Customer OF Order NO-LOCK"). 
DYNAMIC-FUNCTION("setAttribute",hFlatBrw,"prescanqueryOrderLine","FIRST Order OF OrderLine NO-LOCK,FIRST Customer OF Order NO-LOCK"). 
DYNAMIC-FUNCTION("setAttribute",hFlatBrw,"prescanqueryItem","EACH OrderLine OF Item NO-LOCK,FIRST Order OF OrderLine NO-LOCK,FIRST Customer OF Order NO-LOCK"). /* Possible: wmqy (week,month,quarter,year) */

/* Lookup attributes for the filter: */
{incl/dynfilterlookups.i hFlatBrw}


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
DEF VAR cTabList    AS CHAR   NO-UNDO.
DEF VAR iy          AS INT    NO-UNDO.

RUN enable_UI.

DO WITH FRAME {&FRAME-NAME}:
  DYNAMIC-FUNCTION("NewObject",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE,"container").
  DYNAMIC-FUNCTION("CreateStatusBar",THIS-PROCEDURE:CURRENT-WINDOW,"",0,YES,?).

  hBrowse = DYNAMIC-FUNCTION("NewBrowse"
            ,brwBrowse:HANDLE
            ,100
            ,""
            ,"temp-table"
             + ";field1|character|x(30)||Field 1"
            ,"WHERE false"
            ,"").

  DYNAMIC-FUNCTION("setSortString",hBrowse,"Field1").

  hSearchField = DYNAMIC-FUNCTION("NewBrowseSearchField",SearchField:HANDLE,hBrowse,1).
  DYNAMIC-FUNCTION("CreateObjectLink",hSearchField,hBrowse).

  DYNAMIC-FUNCTION("createObjectLink",hBrowse,THIS-PROCEDURE).

  /* optionally extend filter capabilities and add query tuning: 
  {incl/dynfilterlookups.i hBrowse}
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"filterlookupfields_Salesrep","Salesrep;Salesrep;RepName;Region").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"filterlookupquery_Salesrep","where true").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"filterlookupreturnfield_Salesrep","Salesrep").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"getRecordCount","yes").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"allowCan-DoFilterOperator","Salesrep").

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"extraFilterFields",
                   "Item.itemname|Item on order,Order.OrderDate|Order date,OrderLine.Qty|Order qty").

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"prescanqueryItem","EACH OrderLine OF Item NO-LOCK,FIRST Order OF OrderLine NO-LOCK,FIRST Customer OF Order NO-LOCK"). 
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"prescanqueryOrderline","EACH Order OF OrderLine NO-LOCK,FIRST Customer OF Order NO-LOCK"). 
  */
    
  hToolbar = DYNAMIC-FUNCTION("NewToolBar",
                    NavToolbar:HANDLE,             
                    "File",                          
                    "filter,flatview,excel"
                   ,"maxborder").
  DYNAMIC-FUNCTION("createObjectLink",hBrowse,hToolbar).

  hTabFolder = DYNAMIC-FUNCTION("NewTabFolder",rectTabFolder:HANDLE).
  
  /* X and Y limits for move of widget are not yet set for the window. 
     Since we want the tabs to resize according to the window size these values must be set here and
     they must be exact the same as in setOrwWinSize (see InitResize function) 
     Here the values are set to not give any automatic move of widgets */
     
  DYNAMIC-FUNCTION("setMinXYmove",2000,1200). 

  DYNAMIC-FUNCTION("setImageList" IN hTabFolder,"newbmp\user_green.bmp;application_add.bmp;bullet.bmp").
  DYNAMIC-FUNCTION("InitPages" IN hTabFolder,"Page 1|template/JBoxSuppressedWindow.w;Page 2|template/JBoxSuppressedWindow.w",hBrowse).  
  DYNAMIC-FUNCTION("buildFolder" IN hTabFolder).

  DYNAMIC-FUNCTION("setAttribute",DYNAMIC-FUNCTION("getPageQuery" IN hTabFolder,2),"prevTabItem",STRING(hBrowse)).

END.
 
DYNAMIC-FUNCTION("initTranslation",THIS-PROCEDURE:CURRENT-WINDOW).

DYNAMIC-FUNCTION("TabFolderChanged" IN hTabFolder,1).

InitializeResize().

RUN InvokeMethod(hBrowse,"OpenQuery").

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
/* {&WINDOW-NAME}:BGCOLOR = 16.  */
IF DYNAMIC-FUNCTION("getAttribute",THIS-PROCEDURE,"advGuiWin") = "YES" THEN
  RUN ShowForm ("") NO-ERROR.

THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = NO.
{&WINDOW-NAME}:WINDOW-STATE = 3.
{&WINDOW-NAME}:MOVE-TO-TOP().
DYNAMIC-FUNCTION("DoLockWindow",?).

RUN InvokeMethod(hBrowse,"EnterBrowseSearch").

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
IF CAN-DO(hCurrTabProc:INTERNAL-ENTRIES,"SaveRecord") THEN
  RUN SaveRecord IN hCurrTabProc.
ELSE RUN SUPER.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

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
  DYNAMIC-FUNCTION("setSplitBarX",THIS-PROCEDURE:CURRENT-WINDOW,btnSplitBarX:HANDLE IN FRAME frSplitBarX,NO).

  DYNAMIC-FUNCTION("setFollowSplitBarX",THIS-PROCEDURE:CURRENT-WINDOW,btnSplitBarX:HANDLE IN FRAME frSplitBarX, 
                    DYNAMIC-FUNCTION("getWidgetsByLasso",brwBrowse:HANDLE IN FRAME {&FRAME-NAME},0,"frame,control-frame,rectangle,browse") + "," +
                    DYNAMIC-FUNCTION("getWidgetsByLasso",rectTabFolder:HANDLE IN FRAME {&FRAME-NAME},0,"frame,browse,control-frame,rectangle,editor,button,toggle-box,combo-box,fill-in")
                    ).

  DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "brwBrowse," + hBrowse:NAME).
  DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "NavToolbar").

  DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,2000,1200,0,550).
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
IF iCurrTab NE 0 THEN
  DYNAMIC-FUNCTION("DeleteObjectLink",hBrowse,DYNAMIC-FUNCTION("getPageQuery" IN hTabFolder,iCurrTab)).

hCurrTabQuery = DYNAMIC-FUNCTION("getPageQuery" IN hTabFolder,iiTab).

/* When details record on first page and child records on subsequent pages 
   linking shold be done like this: 

IF iiTab = 1 THEN
  DYNAMIC-FUNCTION("CreateOneToOneLink",hCurrTabQuery,hBrowse,"CustNum").
ELSE IF VALID-HANDLE(hCurrTabQuery) THEN
  DYNAMIC-FUNCTION("CreateParentLink",hCurrTabQuery,hBrowse,"CustNum").
*/
                                    
ASSIGN hCurrTabProc  = DYNAMIC-FUNCTION("getPageHandle" IN hTabFolder,iiTab)
       hCurrTabFrame = DYNAMIC-FUNCTION("getPageFrame" IN hTabFolder,iiTab)
       iCurrTab      = iiTab.

RUN InvokeMethod(hBrowse,"DisplayRecord").

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

