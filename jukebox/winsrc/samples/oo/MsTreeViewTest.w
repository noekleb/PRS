&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          sports117        PROGRESS
*/
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
&IF "{1}" = "Developer_Studio_is_Running" &THEN
  &SCOPED-DEFINE UIB_is_Running 1 
&ENDIF   
/* Uncomment to enable use of .Net components: */
&SCOPED-DEFINE AdvGuiWin 

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEF VAR bOk         AS LOG    NO-UNDO.
DEF VAR ix          AS INT    NO-UNDO.
DEF VAR hBrowse     AS HANDLE NO-UNDO.
DEF VAR hQuery      AS HANDLE NO-UNDO.
DEF VAR hToolbar    AS HANDLE NO-UNDO.
DEF VAR hFieldMap   AS HANDLE NO-UNDO.
DEF VAR oContainer  AS JBoxContainer NO-UNDO.
DEF VAR cRegionList AS CHAR   NO-UNDO.


DEF VAR oJBoxMsTreeView AS uc.JBoxMsTreeView NO-UNDO.


/* Code for Definitions: */

/*** Start instance property definitions for JBoxQuery object oQrySalesrep ***/
DEF VAR oQrySalesrep AS JBoxQuery NO-UNDO.

DEF TEMP-TABLE Salesrep
    FIELD MonthQuota_1 AS integer
    FIELD jbextent_1_MonthQuota AS integer /* placeholder for calculation */
    FIELD MonthQuota_2 AS integer
    FIELD jbextent_2_MonthQuota AS integer /* placeholder for calculation */
    FIELD MonthQuota_3 AS integer
    FIELD jbextent_3_MonthQuota AS integer /* placeholder for calculation */
    FIELD MonthQuota_4 AS integer
    FIELD jbextent_4_MonthQuota AS integer /* placeholder for calculation */
    FIELD MonthQuota_5 AS integer
    FIELD jbextent_5_MonthQuota AS integer /* placeholder for calculation */
    FIELD MonthQuota_6 AS integer
    FIELD jbextent_6_MonthQuota AS integer /* placeholder for calculation */
    FIELD MonthQuota_7 AS integer
    FIELD jbextent_7_MonthQuota AS integer /* placeholder for calculation */
    FIELD MonthQuota_8 AS integer
    FIELD jbextent_8_MonthQuota AS integer /* placeholder for calculation */
    FIELD MonthQuota_9 AS integer
    FIELD jbextent_9_MonthQuota AS integer /* placeholder for calculation */
    FIELD MonthQuota_10 AS integer
    FIELD jbextent_10_MonthQuota AS integer /* placeholder for calculation */
    FIELD MonthQuota_11 AS integer
    FIELD jbextent_11_MonthQuota AS integer /* placeholder for calculation */
    FIELD MonthQuota_12 AS integer
    FIELD jbextent_12_MonthQuota AS integer /* placeholder for calculation */
    FIELD Region AS character
    FIELD RepName AS character
    FIELD SalesRep AS character
    FIELD RowIdent1 AS CHARACTER 
    FIELD RowCount AS INTEGER
    INDEX idxRowids  RowIdent1
    .
DEFINE BUFFER v_Salesrep FOR Salesrep.

FUNCTION getTableHandleQrySalesrep RETURNS HANDLE().
  RETURN BUFFER Salesrep:HANDLE:TABLE-HANDLE.
END FUNCTION.
FUNCTION getBuffersAndFieldsQrySalesrep RETURNS CHARACTER().
  RETURN 
      'Salesrep'.
END FUNCTION.
FUNCTION getQueryJoinQrySalesrep RETURNS CHARACTER().
  RETURN ''.
END FUNCTION.
/*** End instance property settings for JBoxQuery object oQrySalesrep ***/


/*** Start instance property definitions for JBoxBrowse object oBrwCustomer ***/
DEF VAR oBrwCustomer AS JBoxBrowse NO-UNDO.
DEF TEMP-TABLE Customer
    FIELD CustNum AS integer
    FIELD Name AS character
    FIELD Phone AS character
    FIELD PostalCode AS character
    FIELD Address AS character
    FIELD City AS character
    FIELD Country AS character
    FIELD RowIdent1 AS CHARACTER 
    FIELD RowCount AS INTEGER
    FIELD jbCountDistinct AS INTEGER FORMAT '>>>,>>>,>>9' INIT 1
    FIELD jbAverage AS DECIMAL FORMAT '->>>,>>>,>>9.99'
    INDEX idxRowids  RowIdent1
    .
DEF BUFFER v_Customer FOR TEMP-TABLE Customer.


FUNCTION getBuffersAndFieldsBrwCustomer RETURNS CHARACTER():
  RETURN
    'Customer'
     + ';CustNum'
     + ';Name'
     + ';Phone'
     + ';PostalCode'
     + ';Address'
     + ';City'
     + ';Country'
     .
END FUNCTION.
FUNCTION getQueryJoinBrwCustomer RETURNS CHARACTER():
  RETURN ''.
END FUNCTION.


DEF VAR otbSalesrep AS JBoxToolbar NO-UNDO.


/*** Start instance property definitions for JBoxBrowse object oBrwOrder ***/
DEF VAR oBrwOrder AS JBoxBrowse NO-UNDO.
DEF TEMP-TABLE Order
    FIELD Ordernum AS integer
    FIELD OrderDate AS date
    FIELD OrderStatus AS character
    FIELD PromiseDate AS date
    FIELD ShipDate AS date
    FIELD SalesRep AS character
    FIELD RepName AS character
    FIELD CustNum AS integer
    FIELD RowIdent1 AS CHARACTER 
    FIELD RowIdent2 AS CHARACTER 
    FIELD RowCount AS INTEGER
    FIELD jbCountDistinct AS INTEGER FORMAT '>>>,>>>,>>9' INIT 1
    FIELD jbAverage AS DECIMAL FORMAT '->>>,>>>,>>9.99'
    INDEX idxRowids  RowIdent1 RowIdent2
    .
DEF BUFFER v_Order FOR TEMP-TABLE Order.


FUNCTION getBuffersAndFieldsBrwOrder RETURNS CHARACTER():
  RETURN
    'Order'
     + ';Ordernum'
     + ';OrderDate'
     + ';OrderStatus'
     + ';PromiseDate'
     + ';ShipDate'
     + ';SalesRep'
     + ';CustNum'
  + ',Salesrep'
     + ';RepName'
     .
END FUNCTION.
FUNCTION getQueryJoinBrwOrder RETURNS CHARACTER():
  RETURN 'EACH Salesrep OF Order NO-LOCK'.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BrwCustomer
&Scoped-define QUERY-NAME QUERY-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Customer Order Salesrep

/* Definitions for BROWSE BrwCustomer                                   */
&Scoped-define FIELDS-IN-QUERY-BrwCustomer Customer.CustNum Customer.Name ~
Customer.Phone Customer.PostalCode Customer.Address Customer.City ~
Customer.Country 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BrwCustomer Customer.CustNum 
&Scoped-define QUERY-STRING-BrwCustomer FOR EACH Customer NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BrwCustomer OPEN QUERY BrwCustomer FOR EACH Customer NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BrwCustomer Customer
&Scoped-define FIRST-TABLE-IN-QUERY-BrwCustomer Customer


/* Definitions for BROWSE BrwOrder                                      */
&Scoped-define FIELDS-IN-QUERY-BrwOrder Order.Ordernum Order.OrderDate ~
Order.OrderStatus Order.PromiseDate Order.ShipDate Order.SalesRep ~
Order.RepName Order.CustNum 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BrwOrder Order.Ordernum 
&Scoped-define QUERY-STRING-BrwOrder FOR EACH Order NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BrwOrder OPEN QUERY BrwOrder FOR EACH Order NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BrwOrder Order
&Scoped-define FIRST-TABLE-IN-QUERY-BrwOrder Order


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Definitions for QUERY QUERY-2                                        */
&Scoped-define SELF-NAME QUERY-2
&Scoped-define QUERY-STRING-QUERY-2 FOR EACH Salesrep NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-QUERY-2 OPEN QUERY {&SELF-NAME} FOR EACH Salesrep NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-QUERY-2 Salesrep
&Scoped-define FIRST-TABLE-IN-QUERY-QUERY-2 Salesrep


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tbSalesrep filter_tbSalesrep ~
excel_tbSalesrep ClearNodes JBoxMsTreeView BrwCustomer BrwOrder 
&Scoped-Define DISPLAYED-OBJECTS JBoxMsTreeView 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FillTreeView C-Win 
FUNCTION FillTreeView RETURNS LOGICAL
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setMySplitbarY C-Win 
FUNCTION setMySplitbarY RETURNS LOGICAL FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON ClearNodes 
     LABEL "Clear nodes" 
     SIZE 15 BY 1.14.

DEFINE BUTTON excel_tbSalesrep 
     IMAGE-UP FILE "gif/afexcel.gif":U
     LABEL "Eksporter til E&xcel" 
     SIZE 6 BY 1.52 TOOLTIP "Eksporter til E&xcel (ALT-X)".

DEFINE BUTTON filter_tbSalesrep 
     IMAGE-UP FILE "gif/filter.gif":U
     LABEL "Filter" 
     SIZE 6 BY 1.52 TOOLTIP "Filter (CTRL-F)".

DEFINE VARIABLE JBoxMsTreeView AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 61 BY 11.43 NO-UNDO.

DEFINE RECTANGLE tbSalesrep
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 77.6 BY 1.71.

DEFINE BUTTON btnSplitBarX 
     IMAGE-UP FILE "bmp/tabright.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 1.4 BY 11.43
     BGCOLOR 12 FGCOLOR 12 .

DEFINE BUTTON btnSplitBarY 
     IMAGE-UP FILE "bmp/tabdown.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 78 BY .48
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BrwCustomer FOR 
      Customer SCROLLING.

DEFINE QUERY BrwOrder FOR 
      Order SCROLLING.

DEFINE QUERY QUERY-2 FOR 
      Salesrep SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BrwCustomer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BrwCustomer C-Win _STRUCTURED
  QUERY BrwCustomer NO-LOCK DISPLAY
      Customer.CustNum COLUMN-LABEL "Cust Num" FORMAT ">>>>9":U
      Customer.Name COLUMN-LABEL "Name" FORMAT "x(30)":U
      Customer.Phone COLUMN-LABEL "Phone" FORMAT "x(20)":U
      Customer.PostalCode COLUMN-LABEL "Postal Code" FORMAT "x(10)":U
      Customer.Address COLUMN-LABEL "Address" FORMAT "x(35)":U
      Customer.City COLUMN-LABEL "City" FORMAT "x(25)":U
      Customer.Country COLUMN-LABEL "Country" FORMAT "x(20)":U
  ENABLE
      Customer.CustNum HELP "Please enter a customer number."
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 78 BY 6.19 FIT-LAST-COLUMN.

DEFINE BROWSE BrwOrder
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BrwOrder C-Win _STRUCTURED
  QUERY BrwOrder NO-LOCK DISPLAY
      Order.Ordernum COLUMN-LABEL "Order Num" FORMAT "zzzzzzzzz9":U
            WIDTH 10.2
      Order.OrderDate COLUMN-LABEL "Ordered" FORMAT "99/99/99":U
      Order.OrderStatus COLUMN-LABEL "Order Status" FORMAT "x(20)":U
      Order.PromiseDate COLUMN-LABEL "Promised" FORMAT "99/99/99":U
      Order.ShipDate COLUMN-LABEL "Shipped" FORMAT "99/99/9999":U
      Order.SalesRep COLUMN-LABEL "Sales Rep" FORMAT "x(4)":U
      Order.RepName COLUMN-LABEL "Rep Name" FORMAT "x(30)":U
      Order.CustNum COLUMN-LABEL "Cust Num" FORMAT ">>>>9":U
  ENABLE
      Order.Ordernum HELP "Please enter an order number."
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 78 BY 4.52 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     filter_tbSalesrep AT ROW 1.33 COL 66.2 WIDGET-ID 8
     excel_tbSalesrep AT ROW 1.33 COL 72.2 WIDGET-ID 10
     ClearNodes AT ROW 1.48 COL 47 WIDGET-ID 4
     JBoxMsTreeView AT ROW 3.14 COL 2 NO-LABEL WIDGET-ID 2
     BrwCustomer AT ROW 3.14 COL 66 WIDGET-ID 200
     BrwOrder AT ROW 10.05 COL 66 WIDGET-ID 400
     tbSalesrep AT ROW 1.24 COL 66 WIDGET-ID 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 143.6 BY 13.81 WIDGET-ID 100.

DEFINE FRAME frSplitBarY
     btnSplitBarY AT ROW 3.85 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         THREE-D 
         AT COL 66 ROW 6.48
         SIZE 78 BY 5.24 WIDGET-ID 500.

DEFINE FRAME frSplitBarX
     btnSplitBarX AT ROW 1.1 COL 20
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 44.2 ROW 3.14
         SIZE 27.2 BY 11.67 WIDGET-ID 300.


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
         HEIGHT             = 13.81
         WIDTH              = 143.6
         MAX-HEIGHT         = 23.52
         MAX-WIDTH          = 143.6
         VIRTUAL-HEIGHT     = 23.52
         VIRTUAL-WIDTH      = 143.6
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
ASSIGN FRAME frSplitBarX:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME frSplitBarY:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB BrwCustomer frSplitBarX DEFAULT-FRAME */
/* BROWSE-TAB BrwOrder frSplitBarY DEFAULT-FRAME */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 13.81
       FRAME DEFAULT-FRAME:WIDTH            = 143.6.

ASSIGN 
       Order.CustNum:VISIBLE IN BROWSE BrwOrder = FALSE.

ASSIGN 
       tbSalesrep:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "filter;Filter,excel;Eksporter til E&xcelmaxborder".

/* SETTINGS FOR FRAME frSplitBarX
                                                                        */
ASSIGN 
       btnSplitBarX:MOVABLE IN FRAME frSplitBarX          = TRUE.

/* SETTINGS FOR FRAME frSplitBarY
   NOT-VISIBLE UNDERLINE                                                */
ASSIGN 
       btnSplitBarY:MOVABLE IN FRAME frSplitBarY          = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BrwCustomer
/* Query rebuild information for BROWSE BrwCustomer
     _TblList          = "sports117.Customer"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > "_<CALC>"
"Customer.CustNum" "Cust Num" ">>>>9" "integer" ? ? ? ? ? ? yes "Please enter a customer number." no no "9.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > "_<CALC>"
"Customer.Name" "Name" "x(30)" "character" ? ? ? ? ? ? no "Please enter a name." no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > "_<CALC>"
"Customer.Phone" "Phone" "x(20)" "character" ? ? ? ? ? ? no "Please enter a phone number" no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > "_<CALC>"
"Customer.PostalCode" "Postal Code" "x(10)" "character" ? ? ? ? ? ? no "Please enter the appropriate Postal Code." no no "11.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > "_<CALC>"
"Customer.Address" "Address" "x(35)" "character" ? ? ? ? ? ? no "Please enter an address." no no "35" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > "_<CALC>"
"Customer.City" "City" "x(25)" "character" ? ? ? ? ? ? no "Please enter a city." no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > "_<CALC>"
"Customer.Country" "Country" "x(20)" "character" ? ? ? ? ? ? no "Please enter a country." no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BrwCustomer */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BrwOrder
/* Query rebuild information for BROWSE BrwOrder
     _TblList          = "sports117.Order"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > "_<CALC>"
"Order.Ordernum" "Order Num" "zzzzzzzzz9" "integer" ? ? ? ? ? ? yes "Please enter an order number." no no "10.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > "_<CALC>"
"Order.OrderDate" "Ordered" "99/99/99" "date" ? ? ? ? ? ? no "Please enter the date of order." no no "9.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > "_<CALC>"
"Order.OrderStatus" "Order Status" "x(20)" "character" ? ? ? ? ? ? no "Please enter the Order Status." no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > "_<CALC>"
"Order.PromiseDate" "Promised" "99/99/99" "date" ? ? ? ? ? ? no "Please enter the Promise Date." no no "9.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > "_<CALC>"
"Order.ShipDate" "Shipped" "99/99/9999" "date" ? ? ? ? ? ? no "Please enter the ship date." no no "11.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > "_<CALC>"
"Order.SalesRep" "Sales Rep" "x(4)" "character" ? ? ? ? ? ? no "Please enter the Sales Rep." no no "9.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > "_<CALC>"
"Order.RepName" "Rep Name" "x(30)" "character" ? ? ? ? ? ? no "Please enter the Name of the Salesperson." no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > "_<CALC>"
"Order.CustNum" "Cust Num" ">>>>9" "integer" ? ? ? ? ? ? no "Please enter an existing customer number." no no "9.2" no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BrwOrder */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME frSplitBarX
/* Query rebuild information for FRAME frSplitBarX
     _Query            is NOT OPENED
*/  /* FRAME frSplitBarX */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME frSplitBarY
/* Query rebuild information for FRAME frSplitBarY
     _Query            is NOT OPENED
*/  /* FRAME frSplitBarY */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK QUERY QUERY-2
/* Query rebuild information for QUERY QUERY-2
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Salesrep NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Design-Parent    is FRAME DEFAULT-FRAME @ ( 1.48 , 38 )
*/  /* QUERY QUERY-2 */
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


&Scoped-define FRAME-NAME frSplitBarX
&Scoped-define SELF-NAME btnSplitBarX
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSplitBarX C-Win
ON END-MOVE OF btnSplitBarX IN FRAME frSplitBarX
DO:
  FRAME frSplitBarY:MOVE-TO-TOP().
  btnSplitBarY:MOVE-TO-TOP().
  DYNAMIC-FUNCTION("setSplitBarX" , THIS-PROCEDURE:CURRENT-WINDOW, btnSplitBarX:HANDLE IN FRAME frSplitBarX,NO).
/*  RUN MoveToTop IN hCurrTabProc NO-ERROR.*/

  setMySplitbarY().
  
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


&Scoped-define FRAME-NAME frSplitBarY
&Scoped-define SELF-NAME btnSplitBarY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSplitBarY C-Win
ON END-MOVE OF btnSplitBarY IN FRAME frSplitBarY
DO:
  DYNAMIC-FUNCTION("setSplitBarY",THIS-PROCEDURE:CURRENT-WINDOW, btnSplitBarY:HANDLE IN FRAME frSplitBarY,NO).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME ClearNodes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ClearNodes C-Win
ON CHOOSE OF ClearNodes IN FRAME DEFAULT-FRAME /* Clear nodes */
DO:
  oJBoxMsTreeView:clearNodes().  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME QUERY-2
&Scoped-define BROWSE-NAME BrwCustomer
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
{incl/conttrigg.i oBrwCustomer:BROWSE-HANDLE}

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
    IF DYNAMIC-FUNCTION("getAttribute",THIS-PROCEDURE,"advGuiWin") = "YES" THEN
      RUN WaitForForm NO-ERROR.
  &ENDIF

  IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
    &IF DEFINED(UIB_is_Running) = 0 &THEN
    RUN InitializeObject.
    RUN MoveToTop.
    APPLY "WINDOW-RESIZED" TO {&WINDOW-NAME}.
    SESSION:SET-WAIT-STATE("").
    IF DYNAMIC-FUNCTION("getAttribute",THIS-PROCEDURE,"advGuiWin") = "YES" THEN
      RUN WaitForForm NO-ERROR.
    ELSE 
    &ENDIF
      WAIT-FOR CLOSE OF THIS-PROCEDURE.
  END.  
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EditRegion C-Win 
PROCEDURE EditRegion :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF INPUT PARAM icNode AS CHAR NO-UNDO.

JBoxSession:Instance:PromptForValue("Edit region").
IF JBoxSession:Instance:PromptValueOk THEN DO:
  FOR EACH Salesrep 
      WHERE Salesrep.Region = icNode
      :
    JBoxServerAPI:Instance:Update("Salesrep","Salesrep",Salesrep.SalesRep,"Region",JBoxSession:Instance:PromptValue,
                                  NO, /* no validation */
                                  NO). /* don't commit */
  END.
  IF JBoxServerAPI:Instance:Commit() THEN DO:  
    cRegionList = "".
    FillTreeView().
  END.  
END.  

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
  DISPLAY JBoxMsTreeView 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE tbSalesrep filter_tbSalesrep excel_tbSalesrep ClearNodes 
         JBoxMsTreeView BrwCustomer BrwOrder 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  ENABLE btnSplitBarX 
      WITH FRAME frSplitBarX IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frSplitBarX}
  ENABLE btnSplitBarY 
      WITH FRAME frSplitBarY IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frSplitBarY}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeComponents C-Win 
PROCEDURE InitializeComponents :
&IF DEFINED(AdvGuiWin) &THEN
DO WITH FRAME {&FRAME-NAME}:

  oJBoxMsTreeView = NEW uc.JBoxMsTreeView(THIS-PROCEDURE,JBoxMsTreeView:HANDLE,"tvpics\Fold.bmp,tvpics\FoldOpen.bmp,gif\flag_no.gif").
  oJBoxMsTreeView:RegisterWithJukeBox(YES).
  oJBoxMsTreeView:setAllowDrop(YES).
  
  FillTreeView().
    
END.
&ENDIF
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

oContainer = NEW JBoxContainer().
oContainer:addStatusBar().

DO WITH FRAME {&FRAME-NAME}:

  oQrySalesrep = NEW JBoxQuery('Salesrep').
  RUN InitializeComponents.

  oBrwCustomer = NEW JBoxBrowse(brwCustomer:HANDLE).
  oBrwCustomer:enabledColumns = "Country".

  oJBoxMsTreeView:SelectNode(ENTRY(1,cRegionList)).
  otbSalesrep = NEW JBoxToolbar(tbSalesrep:HANDLE).

  oBrwCustomer:TOOLBAR-OBJECT = otbSalesrep.
  oBrwOrder = NEW JBoxBrowse(brwOrder:HANDLE).

oContainer:setSplitBarY(btnSplitBarY:HANDLE IN FRAME frSplitBarY).
oContainer:setFollowSplitBarY(STRING(BrwCustomer:HANDLE) + "," + STRING(BrwOrder:HANDLE)).
oContainer:setNoResizeY("BrwCustomer").
END.
oBrwOrder:OpenQuery().

oContainer:setSplitBarX(btnSplitBarX:HANDLE IN FRAME frSplitBarX).
oContainer:setFollowSplitBarX(STRING(ClearNodes:HANDLE) + "," + STRING(JBoxMsTreeView:HANDLE) + "," + STRING(BrwCustomer:HANDLE)
                          + "," + STRING(tbSalesrep:HANDLE) + "," + STRING(excel_tbSalesrep:HANDLE) + "," + STRING(filter_tbSalesrep:HANDLE)
                          + "," + STRING(BrwOrder:HANDLE)).
oContainer:setNoResizeX("JBoxMsTreeView").
oContainer:initResize().

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
IF DYNAMIC-FUNCTION("getAttribute",THIS-PROCEDURE,"advGuiWin") = "YES" THEN
  RUN ShowForm("").
THIS-PROCEDURE:CURRENT-WINDOW:WINDOW-STATE = 3.
THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
DYNAMIC-FUNCTION("DoLockWindow",?).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TreeDropNode C-Win 
PROCEDURE TreeDropNode :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF INPUT PARAM icDragNode  AS CHAR NO-UNDO.
DEF INPUT PARAM icDropNode  AS CHAR NO-UNDO.

IF NOT CAN-DO(cRegionList,icDropNode) OR CAN-DO(cRegionList,icDragNode) THEN RETURN.

IF JBoxServerAPI:Instance:Update("Salesrep","Salesrep",icDragNode,"Region",icDropNode,NO) THEN DO:
  FillTreeView().
  oJBoxMsTreeView:ExpandNodes(icDropNode).
END.
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TreeMenuItemRecord C-Win 
PROCEDURE TreeMenuItemRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF INPUT PARAM icMenuItem AS CHAR NO-UNDO.
DEF INPUT PARAM icTreeNode AS CHAR NO-UNDO.

MESSAGE icMenuItem " <- optionally add as procedure (like EditRegion)" 
        skip icTreeNode
VIEW-AS ALERT-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TreeNodeSelectedRecord C-Win 
PROCEDURE TreeNodeSelectedRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF INPUT PARAM icNode AS CHAR NO-UNDO.

IF CAN-DO(cRegionList,icNode) THEN
  ASSIGN oBrwCustomer:preScanBaseQuery = "Salesrep WHERE region = '" + icNode + "',EACH Customer NO-LOCK OF Salesrep" 
         oBrwCustomer:baseQuery = "".
ELSE
  ASSIGN oBrwCustomer:preScanBaseQuery = "" 
         oBrwCustomer:baseQuery = "WHERE Salesrep = '" + icNode + "'".

oBrwCustomer:OpenQuery().

/*MESSAGE icNode    */
/*VIEW-AS ALERT-BOX.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FillTreeView C-Win 
FUNCTION FillTreeView RETURNS LOGICAL
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF VAR cRegion AS CHAR NO-UNDO.
DEF VAR oSubMenu AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.

EMPTY TEMP-TABLE Salesrep.
oJBoxMsTreeView:clearNodes(). 
oQrySalesrep:openQuery().

IF cRegionList = "" THEN
  FOR EACH Salesrep 
      BREAK BY Salesrep.Region
            BY Salesrep.SalesRep
      :
    IF FIRST-OF(Salesrep.Region) THEN DO:
      cRegion = Salesrep.Region.
      cRegionList = cRegionList + (IF cRegionList NE "" THEN "," ELSE "") + cRegion.
      oJBoxMsTreeView:AddNode("",cRegion,cRegion,"Region",
                              IF cRegion = "Norway" THEN 2 ELSE -1,
                              -1).
    END.
       
    oJBoxMsTreeView:AddNode(cRegion,Salesrep.SalesRep,Salesrep.SalesRep + ", " + Salesrep.RepName,"Salesrep",-1,-1).
  END.
ELSE DO:
  FOR EACH Salesrep:
    IF NOT CAN-DO(cRegionList,Salesrep.Region) THEN
      cRegionList = cRegionList + "," + Salesrep.Region.
  END.     
  DO ix = 1 TO NUM-ENTRIES(cRegionList):
    cRegion = ENTRY(ix,cRegionList).
    oJBoxMsTreeView:AddNode("",         /* Parent nodename */
                            cRegion,    /* Nodename  */
                            cRegion,    /* Nodetext */
                            "Region",   /* Nodetype - needed if you want to assign a specific context menu to a subset of nodes */ 
                            -1,         /* Image index (<0 default) */
                            -1).        /* Selected image index (<0 default) */
                            
    FOR EACH Salesrep 
        WHERE Salesrep.Region = cRegion
           BY Salesrep.SalesRep:
      oJBoxMsTreeView:AddNode(cRegion,Salesrep.SalesRep,Salesrep.SalesRep + ", " + Salesrep.RepName,"Salesrep",-1,-1).
    END.
  END.  
END.
  
oJBoxMsTreeView:AddContextMenuStrip("",      /* use to add the menu to a specific node(name) */
                                    "Region" /* The menu is added to nodes of type "region" - blank for all nodes */
                                    ).

oJBoxMsTreeView:AddContextMenuItem("Region",    /* Add to the "Region" context menu strip */
                                   "",
                                   "EditRegion", /* Procedure to call when menu item is selected */  
                                   "Edit region", /* Text */
                                   "").     /* Image */

oJBoxMsTreeView:AddContextMenuStrip("",
                                    "Salesrep" /* The menu is added to nodes of type "salesrep" */
                                    ).

oJBoxMsTreeView:AddContextSubMenu("Salesrep", /* Add submenu to the salesrep menustrip */
                                  "",         /* If the submenu comes under a submenu ad the submenu name here */
                                  "Salesrep", /* Name of the submenu */
                                  "Edit salesrep", 
                                  "ico\bullet_triangle_green.ico"
                                  ). 

oJBoxMsTreeView:AddContextMenuItem("",
                                   "Salesrep",
                                   "EditRepName", /* Procedure to call when menu item is selected */  
                                   "Edit rep name", /* Text */
                                   "ico\bullet_ball_blue.ico"). 

/*                                                              */
/*oJBoxMsTreeView:AddContextMenuItem(?,oSubMenu,"Menuitem 2").  */

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setSplitbarY C-Win 
FUNCTION setMySplitbarY RETURNS LOGICAL:
/*------------------------------------------------------------------------------
  Purpose:
------------------------------------------------------------------------------*/

ASSIGN btnSplitBarY:WIDTH-CHARS IN FRAME frSplitBarY =  1
       btnSplitBarY:X = 1
       FRAME frSplitBarY:WIDTH-CHARS = oBrwCustomer:BROWSE-HANDLE:WIDTH-CHARS
       FRAME frSplitBarY:X = oBrwCustomer:BROWSE-HANDLE:X 
       btnSplitBarY:WIDTH-CHARS = oBrwCustomer:BROWSE-HANDLE:WIDTH-CHARS - 1
       NO-ERROR.
IF ERROR-STATUS:ERROR THEN DO:
  ASSIGN btnSplitBarY:X = 1
         btnSplitBarY:WIDTH-CHARS = oBrwCustomer:BROWSE-HANDLE:WIDTH-CHARS - 1
         FRAME frSplitBarY:WIDTH-CHARS = oBrwCustomer:BROWSE-HANDLE:WIDTH-CHARS
         NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO:
    ASSIGN FRAME frSplitBarY:WIDTH-CHARS = oBrwCustomer:BROWSE-HANDLE:WIDTH-CHARS
           btnSplitBarY:WIDTH-CHARS = oBrwCustomer:BROWSE-HANDLE:WIDTH-CHARS - 1
           NO-ERROR.
    IF ERROR-STATUS:ERROR THEN       
      ASSIGN btnSplitBarY:WIDTH-CHARS = oBrwCustomer:BROWSE-HANDLE:WIDTH-CHARS - 1
             FRAME frSplitBarY:WIDTH-CHARS = oBrwCustomer:BROWSE-HANDLE:WIDTH-CHARS
             NO-ERROR.
  END.         
END.

FRAME frSplitBarY:SCROLLABLE = NO.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

