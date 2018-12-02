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
&IF "{1}" = "Developer_Studio_is_Running" &THEN
  &SCOPED-DEFINE UIB_is_Running 1 
&ENDIF   

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEF VAR bOk               AS LOG  NO-UNDO.
DEF VAR ix                AS INT  NO-UNDO.
DEF VAR iReturn           AS INT  NO-UNDO.
                          
DEF VAR hParentBrowse     AS HANDLE NO-UNDO.
DEF VAR hBrowse           AS HANDLE NO-UNDO.
DEF VAR hBrowseLines      AS HANDLE NO-UNDO.
DEF VAR hToolBar          AS HANDLE NO-UNDO.
DEF VAR hWinToolBar       AS HANDLE NO-UNDO.
DEF VAR hFilter           AS HANDLE NO-UNDO.
DEF VAR hSearchField      AS HANDLE NO-UNDO.
DEF VAR hParent           AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnSplitBarY Orders rectToolbar ~
rectWinToolbar RectBrowseSearch OrderLines CustNum btnCustomer Name ~
SalesRep OrderStatus Carrier Terms 
&Scoped-Define DISPLAYED-OBJECTS CustNum Name SalesRep OrderStatus Carrier ~
Terms 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCustomer 
     LABEL "..." 
     SIZE 4 BY 1.

DEFINE BUTTON btnSplitBarY 
     IMAGE-UP FILE "bmp/tabdown.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Button 1" 
     SIZE 80 BY .48.

DEFINE VARIABLE OrderStatus AS CHARACTER FORMAT "x(20)" 
     LABEL "Status" 
     VIEW-AS COMBO-BOX 
     LIST-ITEMS "","Ordered","Back Ordered","Partially Shipped","Shipped" 
     DROP-DOWN-LIST
     SIZE 22 BY 1.

DEFINE VARIABLE SalesRep AS CHARACTER FORMAT "X(256)":U 
     LABEL "Salesrep" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE Carrier AS CHARACTER FORMAT "x(25)" 
     LABEL "Carrier" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1.

DEFINE VARIABLE CustNum AS INTEGER FORMAT ">>>>9" INITIAL 0 
     LABEL "Cust Num" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.

DEFINE VARIABLE Name AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE Terms AS CHARACTER FORMAT "x(20)" 
     LABEL "Terms" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE RECTANGLE OrderLines
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 80 BY 6.62.

DEFINE RECTANGLE Orders
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 80 BY 5.24.

DEFINE RECTANGLE RectBrowseSearch
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 22 BY .95.

DEFINE RECTANGLE rectToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 13.6 BY .95.

DEFINE RECTANGLE rectWinToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 8 BY .95.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnSplitBarY AT ROW 12.33 COL 1
     CustNum AT ROW 2.43 COL 17 COLON-ALIGNED HELP
          "Please enter a customer number."
     btnCustomer AT ROW 2.43 COL 28.2 NO-TAB-STOP 
     Name AT ROW 2.43 COL 30.6 COLON-ALIGNED HELP
          "Please enter a name." NO-LABEL
     SalesRep AT ROW 3.48 COL 17 COLON-ALIGNED
     OrderStatus AT ROW 3.48 COL 53 COLON-ALIGNED
     Carrier AT ROW 4.52 COL 17 COLON-ALIGNED HELP
          "Please enter the carrier."
     Terms AT ROW 4.52 COL 53 COLON-ALIGNED HELP
          "Please enter the terms."
     Orders AT ROW 6.95 COL 1
     rectToolbar AT ROW 1.33 COL 2.4
     rectWinToolbar AT ROW 1.33 COL 71.6
     RectBrowseSearch AT ROW 5.76 COL 1
     OrderLines AT ROW 12.91 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 18.52.


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
         TITLE              = "<insert window title>"
         HEIGHT             = 18.52
         WIDTH              = 80
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
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
ASSIGN 
       btnSplitBarY:MOVABLE IN FRAME DEFAULT-FRAME          = TRUE.

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
  DEF VAR hColumn AS HANDLE NO-UNDO.
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").
  hColumn = hBrowse:GET-BROWSE-COLUMN(1).
  APPLY "end-resize" TO hColumn.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCustomer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCustomer C-Win
ON CHOOSE OF btnCustomer IN FRAME DEFAULT-FRAME /* ... */
DO:

  DEF VAR cReturnValues   AS CHAR NO-UNDO.
  DEF VAR bOk             AS LOG  NO-UNDO.

  /* See also procedure setLookupAttributes */

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxLookup.w (THIS-PROCEDURE,50,
                    "Customer"
                    + ";Custnum"
                    + ";Name"
                    + ";Address"
                   ,"WHERE false"
                    ,""                                                  
                    ,"CustNum", 
                      OUTPUT cReturnValues,
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk AND cReturnValues NE "" THEN DO:
    CustNum:SCREEN-VALUE = cReturnValues.
    DYNAMIC-FUNCTION("setCurrentObject",hFilter).
    APPLY "return" TO CustNum.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSplitBarY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSplitBarY C-Win
ON END-MOVE OF btnSplitBarY IN FRAME DEFAULT-FRAME /* Button 1 */
DO:
  DYNAMIC-FUNCTION("setSplitBarY" , THIS-PROCEDURE:CURRENT-WINDOW, btnSplitBarY:HANDLE IN FRAME {&FRAME-NAME},NO).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CustNum
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CustNum C-Win
ON F3 OF CustNum IN FRAME DEFAULT-FRAME /* Cust Num */
DO:
  APPLY "choose" TO btnCustomer.
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
  PUBLISH "InvalidateHandle" (THIS-PROCEDURE).
  DYNAMIC-FUNCTION("SetCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
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
  RUN enable_UI.

  IF SOURCE-PROCEDURE:FILE-NAME MATCHES "*Customer*" THEN 
    hParent = SOURCE-PROCEDURE.
  RUN InitWindow.
  THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.

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
  DISPLAY CustNum Name SalesRep OrderStatus Carrier Terms 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btnSplitBarY Orders rectToolbar rectWinToolbar RectBrowseSearch 
         OrderLines CustNum btnCustomer Name SalesRep OrderStatus Carrier Terms 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExtraFlatViewRecord C-Win 
PROCEDURE ExtraFlatViewRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihFlatView   AS HANDLE NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO INIT TRUE.

DEF VAR hFlatBrw    AS HANDLE NO-UNDO.

hFlatBrw  = DYNAMIC-FUNCTION("getBrowseHandle" IN ihFlatView).

/* Enable accumulation pr week for orderdata: */
DYNAMIC-FUNCTION("setAttribute",hFlatBrw,"addperiodOrderdate","mq"). /* Possible: wmqy (week,month,quarter,year) */

/* Change the filter prescan query definition for salesrep joined from order, orderline and item. Queries must pass through order to get to customer: */
/* DYNAMIC-FUNCTION("setAttribute",hFlatBrw,"prescanqueryBuf1_SalesRep","EACH Order OF buf1_SalesRep NO-LOCK,FIRST Customer OF Order NO-LOCK").  */
/* DYNAMIC-FUNCTION("setAttribute",hFlatBrw,"prescanqueryOrderLine","FIRST Order OF OrderLine NO-LOCK,FIRST Customer OF Order NO-LOCK").         */
DYNAMIC-FUNCTION("setAttribute",hFlatBrw,"prescanqueryItem","EACH OrderLine OF Item NO-LOCK,FIRST Order OF OrderLine NO-LOCK"). 
/* Lookup attributes for the filter: */
{incl/dynfilterlookups.i hFlatBrw}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FlatViewRecord C-Win 
PROCEDURE FlatViewRecord :
/*------------------------------------------------------------------------------
  Purpose:    Enable lookup on filter columns 
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hFlatView AS HANDLE NO-UNDO.
DEF VAR hFlatBrw  AS HANDLE NO-UNDO.

RUN SUPER.

/* hFlatView = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hToolBar,"flatviewhandle")) NO-ERROR. */
/* IF NOT VALID-HANDLE(hFlatView) THEN RETURN.                                                     */
/*                                                                                                 */
/* hFlatBrw  = DYNAMIC-FUNCTION("getBrowseHandle" IN hFlatView).                                   */
/*                                                                                                 */
/*                                                                                                 */
/* {incl/dynfilterlookups.i}                                                                       */
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
DEF VAR bLinked AS LOG NO-UNDO.

/* DYNAMIC-FUNCTION("newObject",SESSION,SESSION,"session").               */
/* DYNAMIC-FUNCTION("setAttribute",SESSION,"copytoolbartobrowse","yes").  */

DO WITH FRAME {&FRAME-NAME}:
  IF VALID-HANDLE(hParent) THEN DO:
    hParentBrowse = DYNAMIC-FUNCTION("getParentBrowse" IN hParent).
    ASSIGN bLinked = TRUE
           CustNum:HIDDEN = TRUE
           NAME:HIDDEN = TRUE
           btnCustomer:HIDDEN = TRUE.
  END.

  /* Create the browse: */
  hBrowse = DYNAMIC-FUNCTION("NewBrowse",
                             Orders:HANDLE,
                             100,
                             "",
                             "Order"
                              + ";Ordernum"
                              + ";OrderStatus"
                              + (IF NOT bLinked THEN 
                                 ";CustNum"
                                 ELSE ";!CustNum")
                              + ";PO"
                              + ";OrderDate"
                              + ";ShipDate"
                              + ";Carrier"
                              + ";Terms"
                              + ";WarehouseNum"
                              + ";SalesRep"                              
                             + ",buf1_SalesRep"
                              + ";RepName"
                              + ";Region"
                             + (IF NOT bLinked THEN
                                 ",Customer;Name"
                                ELSE ""),
                             "WHERE false,FIRST buf1_SalesRep OF Order NO-LOCK"
                             + (IF NOT bLinked THEN
                                 ",FIRST Customer OF Order NO-LOCK"
                                ELSE ""),
                             "SORT|OrderNum").
  IF NOT bLinked THEN
    hBrowse:MOVE-COLUMN(12,4).
  ELSE DO:
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"querywhere","").
    DYNAMIC-FUNCTION("CreateParentLink",hBrowse,hParentBrowse,"CustNum").    
  END.

  hSearchField = DYNAMIC-FUNCTION("NewBrowseSearchField",RectBrowseSearch:HANDLE,hBrowse,1).
  DYNAMIC-FUNCTION("createObjectLink",hBrowse,hSearchField).

  hToolbar = DYNAMIC-FUNCTION("NewToolbar",
                             rectToolbar:HANDLE,
                             "File",
                             "first|Navigate,prev|Navigate,next|Navigate,last|Navigate" 
                             + (IF NOT bLinked THEN ",rule,Excel,Flatview" ELSE ""),
                             "maxborder").
  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hToolbar).

  hWinToolbar = DYNAMIC-FUNCTION("NewToolbar",
                             rectWinToolbar:HANDLE,
                             "File",
                             "close;E&xit",
                             "right|enable").

  ASSIGN SalesRep:DELIMITER = "|"
         SalesRep:LIST-ITEM-PAIRS = "||" + DYNAMIC-FUNCTION("getFieldList","SalesRep;SalesRep|RepName;SalesRep","WHERE true").
  hFilter = DYNAMIC-FUNCTION("NewFilter",
                             hBrowse:QUERY,
                             FRAME {&FRAME-NAME}:HANDLE,
                             (IF NOT bLinked THEN "CustNum,name," ELSE "") + "SalesRep,OrderStatus,Carrier,Terms",
                             "", /* Corr. buffer field list */
                             "", /* Operator list */
                             "", /* Operator field list */
                             YES,
                             ?,
                             "").
  DYNAMIC-FUNCTION("CreateObjectLink",hFilter,hBrowse).

  hBrowseLines = DYNAMIC-FUNCTION("NewBrowse",
                             OrderLines:HANDLE,
                             100,
                             "MULTIPLE",
                             "OrderLine"
                              + ";!OrderNum"
                              + ";Linenum"
                              + ";Itemnum"
                              + ";Price"
                              + ";Qty"
                              + ";Discount"
                              + ";OrderLineStatus"
                            + ",Item"
                              + ";ItemName",
                             "WHERE false,FIRST Item OF OrderLine NO-LOCK",
                             "SORT|LineNum").
  hBrowseLines:MOVE-COLUMN(7,3).

  DYNAMIC-FUNCTION("setAttribute",hBrowseLines,"querywhere","").
  DYNAMIC-FUNCTION("setAttribute",hBrowseLines,"use-index","orderline").
  DYNAMIC-FUNCTION("createParentLink",hBrowseLines,hBrowse,"OrderNum").

  DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "Orders,BrwOrders").

  DYNAMIC-FUNCTION("setSplitBarY" , THIS-PROCEDURE:CURRENT-WINDOW, btnSplitBarY:HANDLE IN FRAME {&FRAME-NAME},NO).
  DYNAMIC-FUNCTION("setFollowSplitBarY",THIS-PROCEDURE:CURRENT-WINDOW, btnSplitBarY:HANDLE IN FRAME {&FRAME-NAME},
                    STRING(Orders:HANDLE)
                  + "," + STRING(hBrowse)
                  + "," + STRING(OrderLines:HANDLE)
                  + "," + STRING(hBrowseLines:HANDLE)).
  DYNAMIC-FUNCTION("setSplitBarYLimits",btnSplitBarY:HANDLE IN FRAME {&FRAME-NAME},200,100).

  DYNAMIC-FUNCTION("setOrgWinSize", THIS-PROCEDURE:CURRENT-WINDOW,300,150,0,0).
          
  DYNAMIC-FUNCTION("InitTranslation",THIS-PROCEDURE:CURRENT-WINDOW).

  APPLY "value-changed" TO hBrowse.
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
THIS-PROCEDURE:CURRENT-WINDOW:WINDOW-STATE = 3.
THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartFilterRecord C-Win 
PROCEDURE StartFilterRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  IF DYNAMIC-FUNCTION("getCurrentWidget") = CustNum:HANDLE AND CustNum:MODIFIED THEN 
    NAME:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","Customer","WHERE CustNum = " + CustNum:SCREEN-VALUE,"Name").
END.
DYNAMIC-FUNCTION("setCurrentObject",hFilter).
RUN SUPER.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

