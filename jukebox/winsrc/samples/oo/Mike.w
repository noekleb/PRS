&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          sports116        PROGRESS
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


/*** Start instance property definitions for JBoxBrowse object oBrwOrder ***/
DEF VAR oBrwOrder AS JBoxBrowse NO-UNDO.
DEF TEMP-TABLE Order
    FIELD SalesRep AS character
    FIELD ShipDate AS date
    FIELD ShipToID AS integer
    FIELD Terms AS character
    FIELD WarehouseNum AS integer
    FIELD Address AS character
    FIELD Address2 AS character
    FIELD Balance AS decimal
    FIELD City AS character
    FIELD Comments AS character
    FIELD Contact AS character
    FIELD Country AS character
    FIELD Instructions AS character
    FIELD OrderDate AS date
    FIELD Ordernum AS integer
    FIELD OrderStatus AS character
    FIELD PO AS character
    FIELD PromiseDate AS date
    FIELD BillToID_time AS CHARACTER
    FIELD RowIdent1 AS CHARACTER 
    FIELD RowIdent2 AS CHARACTER 
    FIELD RowCount AS INTEGER
    FIELD jbCountDistinct AS INTEGER FORMAT '>>>,>>>,>>9' INIT 1
    FIELD jbAverage AS DECIMAL FORMAT '->>>,>>>,>>9.99'
    INDEX idxRowids  RowIdent1 RowIdent2
    .
DEF BUFFER v_Order FOR Order.


FUNCTION getBuffersAndFieldsBrwOrder RETURNS CHARACTER():
  RETURN
    'Order'
     + ';SalesRep'
     + ';ShipDate'
     + ';ShipToID'
     + ';Terms'
     + ';WarehouseNum'
     + ';Instructions'
     + ';OrderDate'
     + ';Ordernum'
     + ';OrderStatus'
     + ';PO'
     + ';PromiseDate'
     + ';+BillToID_time|CHARACTER||jb_hhmm(BillToID)|Bill To ID'
  + ',Customer'
     + ';Address'
     + ';Address2'
     + ';Balance'
     + ';City'
     + ';Comments'
     + ';Contact'
     + ';Country'
     .
END FUNCTION.
FUNCTION getQueryJoinBrwOrder RETURNS CHARACTER():
  RETURN 'EACH Customer OF Order NO-LOCK'.
END FUNCTION.
DEF VAR oFmOrder AS JBoxFieldMap NO-UNDO.


DEF VAR otbOrder AS JBoxToolbar NO-UNDO.


DEF VAR oOrderDate_JBoxDevExEdit AS JBoxDevExDateEdit NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BrwOrder

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Order

/* Definitions for BROWSE BrwOrder                                      */
&Scoped-define FIELDS-IN-QUERY-BrwOrder Order.SalesRep Order.ShipDate ~
Order.ShipToID Order.Terms Order.WarehouseNum Order.Address Order.Address2 ~
Order.Balance Order.City Order.Comments Order.Contact Order.Country ~
Order.Instructions Order.OrderDate Order.Ordernum Order.OrderStatus ~
Order.PO Order.PromiseDate Order.BillToID_time 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BrwOrder Order.SalesRep 
&Scoped-define ENABLED-TABLES-IN-QUERY-BrwOrder Order
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BrwOrder Order
&Scoped-define QUERY-STRING-BrwOrder FOR EACH Order NO-LOCK, ~
    EACH Customer OF Order NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BrwOrder OPEN QUERY BrwOrder FOR EACH Order NO-LOCK, ~
    EACH Customer OF Order NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BrwOrder Order
&Scoped-define FIRST-TABLE-IN-QUERY-BrwOrder Order


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tbOrder new_tbOrder copy_tbOrder ~
undo_tbOrder delete_tbOrder save_tbOrder excel_tbOrder insert_tbOrder ~
BrwOrder OrderDate WarehouseNum CustNum Ordernum Address OrderStatus ~
Address2 PO Balance PromiseDate City SalesRep Comments ShipDate Contact ~
ShipToID Country Terms CreditLimit 
&Scoped-Define DISPLAYED-OBJECTS OrderDate WarehouseNum CustNum Ordernum ~
Address OrderStatus Address2 PO Balance PromiseDate City SalesRep Comments ~
ShipDate Contact ShipToID Country Terms CreditLimit 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON copy_tbOrder 
     IMAGE-UP FILE "bmp/copy.bmp":U
     LABEL "Kopier" 
     SIZE 6 BY 1.52 TOOLTIP "Kopier (ALT-K)".

DEFINE BUTTON delete_tbOrder 
     IMAGE-UP FILE "bmp/del16e.bmp":U
     LABEL "Slett" 
     SIZE 6 BY 1.52 TOOLTIP "Slett (CTRL-D)".

DEFINE BUTTON excel_tbOrder 
     IMAGE-UP FILE "gif/afexcel.gif":U
     LABEL "Eksporter til E&xcel" 
     SIZE 6 BY 1.52 TOOLTIP "Eksporter til E&xcel (ALT-X)".

DEFINE BUTTON insert_tbOrder 
     IMAGE-UP FILE "bmp/add.bmp":U
     LABEL "Sett inn" 
     SIZE 6 BY 1.52 TOOLTIP "Sett inn (ALT-S)".

DEFINE BUTTON new_tbOrder 
     IMAGE-UP FILE "bmp/new16e.bmp":U
     LABEL "Ny" 
     SIZE 6 BY 1.52 TOOLTIP "Ny (CTRL-N)".

DEFINE BUTTON save_tbOrder 
     IMAGE-UP FILE "bmp/save.bmp":U
     LABEL "Lagre" 
     SIZE 6 BY 1.52 TOOLTIP "Lagre (CTRL-S)".

DEFINE BUTTON undo_tbOrder 
     IMAGE-UP FILE "bmp/undo16e.bmp":U
     LABEL "Angre" 
     SIZE 6 BY 1.52 TOOLTIP "Angre (CTRL-Z)".

DEFINE VARIABLE Address AS CHARACTER FORMAT "x(35)" 
     LABEL "Address" 
     VIEW-AS FILL-IN 
     SIZE 37 BY 1 TOOLTIP "Please enter an address.".

DEFINE VARIABLE Address2 AS CHARACTER FORMAT "x(35)" 
     LABEL "Address2" 
     VIEW-AS FILL-IN 
     SIZE 37 BY 1 TOOLTIP "Please enter an address.".

DEFINE VARIABLE Balance AS DECIMAL FORMAT "->,>>>,>>9.99" INITIAL 0 
     LABEL "Balance" 
     VIEW-AS FILL-IN 
     SIZE 20.2 BY 1 TOOLTIP "Please enter a balance.".

DEFINE VARIABLE City AS CHARACTER FORMAT "x(25)" 
     LABEL "City" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1 TOOLTIP "Please enter a city.".

DEFINE VARIABLE Comments AS CHARACTER FORMAT "x(80)" 
     LABEL "Comments" 
     VIEW-AS FILL-IN 
     SIZE 50.4 BY 1 TOOLTIP "Please enter comments.".

DEFINE VARIABLE Contact AS CHARACTER FORMAT "x(30)" 
     LABEL "Contact" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1 TOOLTIP "Please enter a contact.".

DEFINE VARIABLE Country AS CHARACTER FORMAT "x(20)" INITIAL "USA" 
     LABEL "Country" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 TOOLTIP "Please enter a country.".

DEFINE VARIABLE CreditLimit AS DECIMAL FORMAT "->,>>>,>>9" INITIAL 1500 
     LABEL "Credit Limit" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 TOOLTIP "Please enter a Credit Limit.".

DEFINE VARIABLE CustNum AS INTEGER FORMAT ">>>>9" INITIAL 0 
     LABEL "Cust Num" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 TOOLTIP "Please enter a customer number.".

DEFINE VARIABLE OrderDate AS DATE FORMAT "99/99/99" 
     LABEL "Ordered" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1 TOOLTIP "Please enter the date of order.".

DEFINE VARIABLE Ordernum AS INTEGER FORMAT "zzzzzzzzz9" INITIAL 0 
     LABEL "Order Num" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 TOOLTIP "Please enter an order number.".

DEFINE VARIABLE OrderStatus AS CHARACTER FORMAT "x(20)" INITIAL "Ordered" 
     LABEL "Order Status" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 TOOLTIP "Please enter the Order Status.".

DEFINE VARIABLE PO AS CHARACTER FORMAT "x(20)" 
     LABEL "PO" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 TOOLTIP "Please enter the PO.".

DEFINE VARIABLE PromiseDate AS DATE FORMAT "99/99/99" 
     LABEL "Promised" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1 TOOLTIP "Please enter the Promise Date.".

DEFINE VARIABLE SalesRep AS CHARACTER FORMAT "x(4)" 
     LABEL "Sales Rep" 
     VIEW-AS FILL-IN 
     SIZE 9.6 BY 1 TOOLTIP "Please enter the Sales Rep.".

DEFINE VARIABLE ShipDate AS DATE FORMAT "99/99/9999" 
     LABEL "Shipped" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 TOOLTIP "Please enter the ship date.".

DEFINE VARIABLE ShipToID AS INTEGER FORMAT "zzzzzzzzz9" INITIAL 0 
     LABEL "Ship To ID" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 TOOLTIP "Please enter the ShipToID.".

DEFINE VARIABLE Terms AS CHARACTER FORMAT "x(20)" INITIAL "Net30" 
     LABEL "Terms" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 TOOLTIP "Please enter the terms.".

DEFINE VARIABLE WarehouseNum AS INTEGER FORMAT "zzzzzzzzz9" INITIAL 0 
     LABEL "Warehouse Num" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 TOOLTIP "Please enter the Warehouse Number.".

DEFINE RECTANGLE tbOrder
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 122 BY 1.71.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BrwOrder FOR 
      Order SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BrwOrder
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BrwOrder C-Win _STRUCTURED
  QUERY BrwOrder NO-LOCK DISPLAY
      Order.SalesRep COLUMN-LABEL "Sales Rep" FORMAT "x(4)":U
      Order.ShipDate COLUMN-LABEL "Shipped" FORMAT "99/99/9999":U
      Order.ShipToID COLUMN-LABEL "Ship To ID" FORMAT "zzzzzzzzz9":U
            WIDTH 10.2
      Order.Terms COLUMN-LABEL "Terms" FORMAT "x(20)":U
      Order.WarehouseNum COLUMN-LABEL "Warehouse Num" FORMAT "zzzzzzzzz9":U
      Order.Address COLUMN-LABEL "Address" FORMAT "x(35)":U WIDTH 27.6
      Order.Address2 COLUMN-LABEL "Address2" FORMAT "x(35)":U
      Order.Balance COLUMN-LABEL "Balance" FORMAT "->,>>>,>>9.99":U
      Order.City COLUMN-LABEL "City" FORMAT "x(25)":U
      Order.Comments COLUMN-LABEL "Comments" FORMAT "x(80)":U
      Order.Contact COLUMN-LABEL "Contact" FORMAT "x(30)":U
      Order.Country COLUMN-LABEL "Country" FORMAT "x(20)":U
      Order.Instructions COLUMN-LABEL "Instructions" FORMAT "x(50)":U
      Order.OrderDate COLUMN-LABEL "Ordered" FORMAT "99/99/99":U
      Order.Ordernum COLUMN-LABEL "Order Num" FORMAT "zzzzzzzzz9":U
            WIDTH 10.2
      Order.OrderStatus COLUMN-LABEL "Order Status" FORMAT "x(20)":U
      Order.PO COLUMN-LABEL "PO" FORMAT "x(20)":U
      Order.PromiseDate COLUMN-LABEL "Promised" FORMAT "99/99/99":U
      Order.BillToID_time COLUMN-LABEL "Bill To ID" FORMAT "x(6)":U
  ENABLE
      Order.SalesRep HELP "Please enter the Sales Rep."
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 123 BY 10 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     new_tbOrder AT ROW 1.33 COL 3.2 WIDGET-ID 4
     copy_tbOrder AT ROW 1.33 COL 9.2 WIDGET-ID 6
     undo_tbOrder AT ROW 1.33 COL 15.4 WIDGET-ID 8
     delete_tbOrder AT ROW 1.33 COL 21.4 WIDGET-ID 10
     save_tbOrder AT ROW 1.33 COL 27.6 WIDGET-ID 12
     excel_tbOrder AT ROW 1.33 COL 33.6 WIDGET-ID 14
     insert_tbOrder AT ROW 1.33 COL 39.8 WIDGET-ID 16
     BrwOrder AT ROW 3.62 COL 2 WIDGET-ID 200
     OrderDate AT ROW 14 COL 12 COLON-ALIGNED
     WarehouseNum AT ROW 14 COL 45 COLON-ALIGNED
     CustNum AT ROW 14 COL 78 COLON-ALIGNED
     Ordernum AT ROW 15 COL 12 COLON-ALIGNED
     Address AT ROW 15 COL 45 COLON-ALIGNED
     OrderStatus AT ROW 16 COL 12 COLON-ALIGNED
     Address2 AT ROW 16 COL 45 COLON-ALIGNED
     PO AT ROW 17 COL 12 COLON-ALIGNED
     Balance AT ROW 17 COL 45 COLON-ALIGNED
     PromiseDate AT ROW 18 COL 12 COLON-ALIGNED
     City AT ROW 18 COL 45 COLON-ALIGNED
     SalesRep AT ROW 19 COL 12 COLON-ALIGNED
     Comments AT ROW 19 COL 45 COLON-ALIGNED
     ShipDate AT ROW 20 COL 12 COLON-ALIGNED
     Contact AT ROW 20 COL 45 COLON-ALIGNED
     ShipToID AT ROW 21 COL 12 COLON-ALIGNED
     Country AT ROW 21 COL 45 COLON-ALIGNED
     Terms AT ROW 22 COL 12 COLON-ALIGNED
     CreditLimit AT ROW 22 COL 45 COLON-ALIGNED
     tbOrder AT ROW 1.24 COL 3 WIDGET-ID 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 125.2 BY 24.57 WIDGET-ID 100.


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
         HEIGHT             = 24.57
         WIDTH              = 125.2
         MAX-HEIGHT         = 24.57
         MAX-WIDTH          = 125.2
         VIRTUAL-HEIGHT     = 24.57
         VIRTUAL-WIDTH      = 125.2
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
/* BROWSE-TAB BrwOrder insert_tbOrder DEFAULT-FRAME */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 24.57
       FRAME DEFAULT-FRAME:WIDTH            = 125.2.

ASSIGN 
       tbOrder:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "new;Ny,copy;Kopier,undo;Angre,delete;Slett,save;Lagre,excel;Eksporter til E&xcel,insert;Sett innmaxborder".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BrwOrder
/* Query rebuild information for BROWSE BrwOrder
     _TblList          = "sports116.Order"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Order.SalesRep
"Order.SalesRep" "Sales Rep" "x(4)" "character" ? ? ? ? ? ? yes "Please enter the Sales Rep." no no "9.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Order.ShipDate
"Order.ShipDate" "Shipped" "99/99/9999" "date" ? ? ? ? ? ? no "Please enter the ship date." no no "11.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Order.ShipToID
"Order.ShipToID" "Ship To ID" "zzzzzzzzz9" "integer" ? ? ? ? ? ? no "Please enter the ShipToID." no no "10.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Order.Terms
"Order.Terms" "Terms" "x(20)" "character" ? ? ? ? ? ? no "Please enter the terms." no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Order.WarehouseNum
"Order.WarehouseNum" "Warehouse Num" "zzzzzzzzz9" "integer" ? ? ? ? ? ? no "Please enter the Warehouse Number." no no "16" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Order.Address
"Order.Address" "Address" "x(35)" "character" ? ? ? ? ? ? no "Please enter an address." no no "27.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Order.Address2
"Order.Address2" "Address2" "x(35)" "character" ? ? ? ? ? ? no "Please enter an address." no no "35" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > Order.Balance
"Order.Balance" "Balance" "->,>>>,>>9.99" "decimal" ? ? ? ? ? ? no "Please enter a balance." no no "13.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > Order.City
"Order.City" "City" "x(25)" "character" ? ? ? ? ? ? no "Please enter a city." no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > Order.Comments
"Order.Comments" "Comments" "x(80)" "character" ? ? ? ? ? ? no "Please enter comments." no no "80" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > Order.Contact
"Order.Contact" "Contact" "x(30)" "character" ? ? ? ? ? ? no "Please enter a contact." no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > Order.Country
"Order.Country" "Country" "x(20)" "character" ? ? ? ? ? ? no "Please enter a country." no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > Order.Instructions
"Order.Instructions" "Instructions" "x(50)" "character" ? ? ? ? ? ? no "Please enter Instructions" no no "50" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > Order.OrderDate
"Order.OrderDate" "Ordered" "99/99/99" "date" ? ? ? ? ? ? no "Please enter the date of order." no no "9.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > Order.Ordernum
"Order.Ordernum" "Order Num" "zzzzzzzzz9" "integer" ? ? ? ? ? ? no "Please enter an order number." no no "10.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > Order.OrderStatus
"Order.OrderStatus" "Order Status" "x(20)" "character" ? ? ? ? ? ? no "Please enter the Order Status." no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > Order.PO
"Order.PO" "PO" "x(20)" "character" ? ? ? ? ? ? no "Please enter the PO." no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > Order.PromiseDate
"Order.PromiseDate" "Promised" "99/99/99" "date" ? ? ? ? ? ? no "Please enter the Promise Date." no no "9.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > "_<CALC>"
"Order.BillToID_time" "Bill To ID" "x(6)" "CHARACTER" ? ? ? ? ? ? no "" no no "8.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BrwOrder */
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


&Scoped-define BROWSE-NAME BrwOrder
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
/*{incl/conttrigg.i oBrw<>:BROWSE-HANDLE} */

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
  DISPLAY OrderDate WarehouseNum CustNum Ordernum Address OrderStatus Address2 
          PO Balance PromiseDate City SalesRep Comments ShipDate Contact 
          ShipToID Country Terms CreditLimit 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE tbOrder new_tbOrder copy_tbOrder undo_tbOrder delete_tbOrder 
         save_tbOrder excel_tbOrder insert_tbOrder BrwOrder OrderDate 
         WarehouseNum CustNum Ordernum Address OrderStatus Address2 PO Balance 
         PromiseDate City SalesRep Comments ShipDate Contact ShipToID Country 
         Terms CreditLimit 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeComponents C-Win 
PROCEDURE InitializeComponents :
&IF DEFINED(AdvGuiWin) &THEN
DO WITH FRAME {&FRAME-NAME}:


  oOrderDate_JBoxDevExEdit = NEW JBoxDevExDateEdit(THIS-PROCEDURE,OrderDate:HANDLE).
  oOrderDate_JBoxDevExEdit:RegisterWithJukeBox(YES). /* YES: Visible */
  oOrderDate_JBoxDevExEdit:CreateDisplayLink(oFmOrder:BUFFER-HANDLE,'OrderDate').
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

  oBrwOrder = NEW JBoxBrowse(brwOrder:HANDLE).

  oFmOrder = NEW JBoxFieldMap(FRAME {&FRAME-NAME}:HANDLE).
  oFmOrder:updateFields = 'OrderDate,Ordernum,OrderStatus,PO,PromiseDate,SalesRep,ShipDate,ShipToID,Terms,WarehouseNum'.
  oFmOrder:displayFields = 'Address,Address2,Balance,City,Comments,Contact,Country,CreditLimit,CustNum'.
  oFmOrder:primaryKeyFields = 'Ordernum'.

  oFmOrder:BROWSE-OBJECT = oBrwOrder.
  otbOrder = NEW JBoxToolbar(tbOrder:HANDLE).
 
  oBrwOrder:TOOLBAR-OBJECT = otbOrder.
  oFmOrder:TOOLBAR-OBJECT = otbOrder.
  RUN InitializeComponents.
END.
oBrwOrder:OpenQuery().


oContainer:initResize().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE insertRecord C-Win 
PROCEDURE insertRecord :
RUN SUPER.


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

