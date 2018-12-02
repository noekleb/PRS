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

DEF VAR bOk               AS LOG  NO-UNDO.
DEF VAR ix                AS INT  NO-UNDO.
                          
DEF VAR hToolbar          AS HANDLE NO-UNDO.
DEF VAR hBrowse           AS HANDLE NO-UNDO.
DEF VAR hTabFolder        AS HANDLE NO-UNDO.
 
DEF VAR hCurrTabProc      AS HANDLE NO-UNDO.
DEF VAR hCurrTabFrame     AS HANDLE NO-UNDO.
DEF VAR hCurrTabQuery     AS HANDLE NO-UNDO.
DEF VAR iCurrTab          AS INT NO-UNDO.
DEF VAR hParent           AS HANDLE NO-UNDO.
DEF VAR hParentQuery      AS HANDLE NO-UNDO.
DEF VAR hPrintPreview     AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME frmOrderBrw

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnSplitBarY OrderToolbar OrderNavBrowse ~
OrderFolder 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCustNum C-Win 
FUNCTION getCustNum RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFrameHandle C-Win 
FUNCTION getFrameHandle RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getOrderNum C-Win 
FUNCTION getOrderNum RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InitializeResize C-Win 
FUNCTION InitializeResize RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD TabChanged C-Win 
FUNCTION TabChanged RETURNS LOGICAL
  ( INPUT iiTab AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnSplitBarY 
     IMAGE-UP FILE "bmp/tabup.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Button 1" 
     SIZE 112 BY .43.

DEFINE RECTANGLE OrderFolder
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 112 BY 9.29
     BGCOLOR 12 .

DEFINE RECTANGLE OrderNavBrowse
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 111.4 BY 6.19.

DEFINE RECTANGLE OrderToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9.8 BY .91.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME frmOrderBrw
     btnSplitBarY AT ROW 8.71 COL 1.6
     OrderToolbar AT ROW 1.19 COL 1.8
     OrderNavBrowse AT ROW 2.43 COL 1.6
     OrderFolder AT ROW 9.33 COL 1.8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 113 BY 17.71.


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
         TITLE              = "Orders"
         HEIGHT             = 17.67
         WIDTH              = 113.4
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

{incl/devmode.i}
{incl/custdevmode.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME frmOrderBrw
   FRAME-NAME                                                           */
ASSIGN 
       btnSplitBarY:MOVABLE IN FRAME frmOrderBrw          = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME frmOrderBrw
/* Query rebuild information for FRAME frmOrderBrw
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME frmOrderBrw */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Orders */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Orders */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSplitBarY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSplitBarY C-Win
ON END-MOVE OF btnSplitBarY IN FRAME frmOrderBrw /* Button 1 */
DO:
  DYNAMIC-FUNCTION("setSplitBarY",CURRENT-WINDOW,btnSplitBarY:HANDLE IN FRAME {&FRAME-NAME},NO).
  APPLY "window-resized" TO {&WINDOW-NAME}.
/*   RUN MoveToTop. */
/*   DYNAMIC-FUNCTION("MoveTabToTop" IN hTabFolder,OrderFolder:HANDLE). */
/*   RUN MoveToTop IN hCurrTabProc NO-ERROR.                            */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME OrderToolbar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL OrderToolbar C-Win
ON MOUSE-SELECT-CLICK OF OrderToolbar IN FRAME frmOrderBrw

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
  IF VALID-HANDLE(hPrintPreview) THEN APPLY "close" TO hPrintPreview.
  PUBLISH "InvalidateHandle".
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  IF THIS-PROCEDURE:CURRENT-WINDOW = ? THEN THIS-PROCEDURE:CURRENT-WINDOW = CURRENT-WINDOW.
  RUN enable_UI.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

{incl/supptrigg.i hBrowse}

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
  /* Hide all frames. */
  HIDE FRAME frmOrderBrw.
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
  ENABLE btnSplitBarY OrderToolbar OrderNavBrowse OrderFolder 
      WITH FRAME frmOrderBrw.
  {&OPEN-BROWSERS-IN-QUERY-frmOrderBrw}
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
DO WITH FRAME {&FRAME-NAME}:

  DYNAMIC-FUNCTION("NewObject",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE,"procedure").
  hBrowse = DYNAMIC-FUNCTION("NewBrowse",
                    OrderNavBrowse:HANDLE,   
                    100,                 
                    "",                  
                    "Order"
                    + ";OrderNum"
                    + ";OrderDate"
                    + ";PromiseDate"
                    + ";ShipDate"
                    + ";OrderStatus"
                    + ";Terms"
                    + ";Carrier"
                    + ";SalesRep"
                    + ";!BillToId;!CustNum"
                  + ",buf1_SalesRep"
                    + ";RepName"
                    ,"WHERE false"
                    + ",FIRST buf1_SalesRep NO-LOCK OF Order"
                    ,"sort|OrderNum desc").
  DYNAMIC-FUNCTION("createObjectLink",hBrowse,THIS-PROCEDURE).

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"getrecordcount","yes").
  
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"filterlookupfields_Salesrep","Salesrep;Salesrep;RepName").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"filterlookupquery_Salesrep","where false").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"filterlookupreturnfield_Salesrep","Salesrep").

  hTabFolder = DYNAMIC-FUNCTION("NewTabFolder",OrderFolder:HANDLE).
  DYNAMIC-FUNCTION("setTabMoveY" IN hTabFolder).
  
  DYNAMIC-FUNCTION("InitPages" IN hTabFolder,"Details|OrderViewDotNet.w" 
                                           + "|Items|OrderLineBrw.w"
                                            ,hBrowse).
/*   DYNAMIC-FUNCTION("InitPages" IN hTabFolder,"Details|OrderView.w|Items|OrderLineBrw.w",hBrowse). */

  DYNAMIC-FUNCTION("buildFolder" IN hTabFolder).

  hToolbar = DYNAMIC-FUNCTION("NewToolBar",
                    OrderToolbar:HANDLE,
                    "",
                    "New,Edit,Undo,Delete,Save,Filter,excel,Print,BrowseConfig",
                    "maxborder").

  DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:HANDLE,"OrderToolbar,OrderNavBrowse," + hBrowse:NAME).
  DYNAMIC-FUNCTION("setSplitBarY",THIS-PROCEDURE:CURRENT-WINDOW,btnSplitBarY:HANDLE,NO).

  DYNAMIC-FUNCTION("setFollowSplitBarY",THIS-PROCEDURE:CURRENT-WINDOW,btnSplitBarY:HANDLE IN FRAME {&FRAME-NAME}, 
                    DYNAMIC-FUNCTION("getWidgetsByLasso",OrderFolder:HANDLE IN FRAME {&FRAME-NAME},0,"frame,control-frame,rectangle,browse") + "," +
                    STRING(OrderNavBrowse:HANDLE) + "," +
                    STRING(hBrowse)
                    ).
  DYNAMIC-FUNCTION("setSplitBarYlimits",btnSplitBarY:HANDLE,110,220).

  DYNAMIC-FUNCTION("TabFolderChanged" IN hTabFolder,1).

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
FRAME {&FRAME-NAME}:MOVE-TO-TOP().
DYNAMIC-FUNCTION("MoveTabToTop" IN hTabFolder,OrderFolder:HANDLE).
RUN MoveToTop IN hCurrTabProc NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrintRecord C-Win 
PROCEDURE PrintRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF NOT VALID-HANDLE(hPrintPreview) THEN DO:
  RUN JBoxTextFilePreview.w PERSIST SET hPrintPreview.
  /* To remove Excel button (Excel only applies for column reports with regular column headers): */
/*   DYNAMIC-FUNCTION("setEnableExcel" IN hPrintPreview,NO). */
  RUN InitializeObject IN hPrintPreview.
END.

DYNAMIC-FUNCTION("setWindowTitle" IN hPrintPreview,"Orderprint from " + PROGRAM-NAME(1) + " Server-routine: samples/order_print.p").

/* If the header looks nasty (like here) in Excel it can be skipped from the export: */
DYNAMIC-FUNCTION("setViewHeadersInExcel" IN hPrintPreview,NO).

/* If (like here) the column headers don't occur on the first page, give the start of the column header line so
   column headers may be viewed the first time they occur: */
DYNAMIC-FUNCTION("setStartColumnHeader" IN hPrintPreview,"Line Num ").

/* For pages after the first, instuct the preview to hide header lines, including the column headers */
DYNAMIC-FUNCTION("setEndColumnHeader" IN hPrintPreview,"-----"). /* <- the beginning of the end */

/* To hide the footer from the preview: */
DYNAMIC-FUNCTION("setFooterLineNum" IN hPrintPreview,39). 

DYNAMIC-FUNCTION("LoadPreviewFromTT" IN hPrintPreview,
                 DYNAMIC-FUNCTION("getTempTable","order_print.p"
                                   ,STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("Ordernum"):BUFFER-VALUE),?)
                ,"",NO).

RUN MoveToTop IN hPrintPreview.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RefreshBrowseRecord C-Win 
PROCEDURE RefreshBrowseRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN SUPER.
PUBLISH "NewOrderCount" (hBrowse).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCustNum C-Win 
FUNCTION getCustNum RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF hBrowse:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN
  RETURN hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("CustNum"):STRING-VALUE.
ELSE RETURN DYNAMIC-FUNCTION("getCustNum" IN hParent).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getOrderNum C-Win 
FUNCTION getOrderNum RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("OrderNum"):STRING-VALUE.

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
DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:HANDLE,"OrderToolbar,OrderNavBrowse").
/* DYNAMIC-FUNCTION("setAddMoveY",THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:HANDLE,"OrderFolder").  */

RETURN TRUE.   /* Function return value. */

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
hParentQuery = ihQuery.

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

/* DYNAMIC-FUNCTION("DeleteObjectLink",hToolbar,DYNAMIC-FUNCTION("getPageQuery"    IN hTabFolder,iCurrTab)). */
/* DYNAMIC-FUNCTION("DeleteObjectLink",hToolbar,DYNAMIC-FUNCTION("getPageFieldMap" IN hTabFolder,iCurrTab)). */
/* DYNAMIC-FUNCTION("DeleteObjectLink",hToolbar,hBrowse).                                                    */
/* These lines can be replaced by this command: */
DYNAMIC-FUNCTION("DeleteLinksFrom",hToolbar).

ASSIGN hCurrTabProc  = DYNAMIC-FUNCTION("getPageHandle" IN hTabFolder,iiTab)
       hCurrTabQuery = DYNAMIC-FUNCTION("getPageQuery" IN hTabFolder,iiTab)
       hCurrTabFrame = DYNAMIC-FUNCTION("getPageFrame" IN hTabFolder,iiTab)
       iCurrTab      = iiTab.

IF iiTab = 1 THEN DO:
  DYNAMIC-FUNCTION("CreateOneToOneLink",hCurrTabQuery,hBrowse,"OrderNum").
  DYNAMIC-FUNCTION("CreateObjectLink",hToolbar,DYNAMIC-FUNCTION("getPageFieldMap" IN hTabFolder,iiTab)).
  DYNAMIC-FUNCTION("createObjectLink",hBrowse,hToolbar).
  DYNAMIC-FUNCTION("setToolbarHandle" IN hCurrTabProc,hToolbar).
END.
ELSE
  DYNAMIC-FUNCTION("CreateParentLink",hCurrTabQuery,hBrowse,"OrderNum").


DYNAMIC-FUNCTION("CreateObjectLink",hToolbar,hCurrTabQuery).

DYNAMIC-FUNCTION("setCurrentObject",hBrowse).

RUN DisplayRecord.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

