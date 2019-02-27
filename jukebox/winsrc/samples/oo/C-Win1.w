&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          sports113        PROGRESS
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
/* &SCOPED-DEFINE AdvGuiWin */ 

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEF VAR bOk          AS LOG    NO-UNDO.
DEF VAR ix           AS INT    NO-UNDO.
DEF VAR hBrowse      AS HANDLE NO-UNDO.
DEF VAR hQuery       AS HANDLE NO-UNDO.
DEF VAR hToolbar     AS HANDLE NO-UNDO.
DEF VAR hFieldMap    AS HANDLE NO-UNDO.


DEF VAR oFmCustomer AS JBoxFieldMap NO-UNDO.
DEF VAR oTbCustomer AS JBoxToolbar NO-UNDO.


/*** Start instance property definitions for JBoxBrowse object oBrwCustomer ***/
DEF VAR oBrwCustomer AS JBoxBrowse NO-UNDO.

DEF TEMP-TABLE Customer
    FIELD CustNum AS integer
    FIELD Name AS character
    FIELD City AS character
    FIELD Discount AS integer
    FIELD Country AS character
    FIELD SalesRep AS character
    FIELD RepName AS character
    FIELD Region AS character
    FIELD OrderDate AS date
    FIELD OrderStatus AS character
    FIELD RowIdent1 AS CHARACTER 
    FIELD RowIdent2 AS CHARACTER 
    FIELD RowIdent3 AS CHARACTER 
    FIELD RowCount AS INTEGER
    FIELD jbCountDistinct AS INTEGER FORMAT '>>>,>>>,>>9' INIT 1
    FIELD jbAverage AS DECIMAL FORMAT '->>>,>>>,>>9.99'
    INDEX idxRowids  RowIdent1 RowIdent2 RowIdent3
    .
DEFINE BUFFER v_Customer FOR Customer.

FUNCTION getBuffersAndFieldsBrwCustomer RETURNS CHARACTER():
  RETURN
    'Customer'
     + ';CustNum'
     + ';Name'
     + ';City'
     + ';Discount'
     + ';Country'
     + ';SalesRep'
  + ',Salesrep'
     + ';RepName'
     + ';Region'
  + ',Order'
     + ';OrderDate'
     + ';OrderStatus'.
END FUNCTION.
FUNCTION getQueryJoinBrwCustomer RETURNS CHARACTER():
  RETURN 'EACH Salesrep OF Customer NO-LOCK,FIRST Order OF Customer NO-LOCK'.
END FUNCTION.
/*** End instance property settings for JBoxBrowse object oBrwCustomer ***/


/*** Start instance property definitions for JBoxBrowse object oBrwInvoice ***/
DEF VAR oBrwInvoice AS JBoxBrowse NO-UNDO.

DEF TEMP-TABLE Invoice
    FIELD Invoicenum AS integer
    FIELD InvoiceDate AS date
    FIELD OrderNum AS integer
    FIELD ShipCharge AS decimal
    FIELD TotalPaid AS decimal
    FIELD Instructions AS character
    FIELD OrderDate AS date
    FIELD RowIdent1 AS CHARACTER 
    FIELD RowIdent2 AS CHARACTER 
    FIELD RowCount AS INTEGER
    FIELD jbCountDistinct AS INTEGER FORMAT '>>>,>>>,>>9' INIT 1
    FIELD jbAverage AS DECIMAL FORMAT '->>>,>>>,>>9.99'
    INDEX idxRowids  RowIdent1 RowIdent2
    .
DEFINE BUFFER v_Invoice FOR Invoice.

FUNCTION getBuffersAndFieldsBrwInvoice RETURNS CHARACTER().
  RETURN 
      'Invoice'
      + ';Invoicenum'
      + ';InvoiceDate'
      + ';OrderNum'
      + ';ShipCharge'
      + ';TotalPaid'
    + ',Order'
      + ';Instructions'
      + ';OrderDate'.
END FUNCTION.
FUNCTION getQueryJoinBrwInvoice RETURNS CHARACTER().
  RETURN 'EACH Order WHERE Order.Ordernum = Invoice.OrderNum NO-LOCK'.
END FUNCTION.
/*** End instance property settings for JBoxBrowse object oBrwInvoice ***/


DEF VAR oTbInvoice AS JBoxToolbar NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME brwCustomer

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Customer Invoice

/* Definitions for BROWSE brwCustomer                                   */
&Scoped-define FIELDS-IN-QUERY-brwCustomer Customer.CustNum Customer.Name Customer.City Customer.Discount Customer.Country Customer.SalesRep Customer.RepName Customer.Region Customer.OrderDate Customer.OrderStatus   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brwCustomer Customer.CustNum   
&Scoped-define ENABLED-TABLES-IN-QUERY-brwCustomer Customer
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-brwCustomer Customer
&Scoped-define SELF-NAME brwCustomer
&Scoped-define QUERY-STRING-brwCustomer FOR EACH Customer NO-LOCK
&Scoped-define OPEN-QUERY-brwCustomer OPEN QUERY {&SELF-NAME} FOR EACH Customer NO-LOCK.
&Scoped-define TABLES-IN-QUERY-brwCustomer Customer
&Scoped-define FIRST-TABLE-IN-QUERY-brwCustomer Customer


/* Definitions for BROWSE brwInvoice                                    */
&Scoped-define FIELDS-IN-QUERY-brwInvoice Invoice.Invoicenum Invoice.InvoiceDate Invoice.OrderNum Invoice.ShipCharge Invoice.TotalPaid Invoice.Instructions Invoice.OrderDate   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brwInvoice Invoice.Invoicenum   
&Scoped-define ENABLED-TABLES-IN-QUERY-brwInvoice Invoice
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-brwInvoice Invoice
&Scoped-define SELF-NAME brwInvoice
&Scoped-define QUERY-STRING-brwInvoice FOR EACH Invoice NO-LOCK
&Scoped-define OPEN-QUERY-brwInvoice OPEN QUERY {&SELF-NAME} FOR EACH Invoice NO-LOCK.
&Scoped-define TABLES-IN-QUERY-brwInvoice Invoice
&Scoped-define FIRST-TABLE-IN-QUERY-brwInvoice Invoice


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-brwCustomer}~
    ~{&OPEN-QUERY-brwInvoice}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tbCustomer tbInvoice brwCustomer CustNum ~
Name SalesRep RepName brwInvoice 
&Scoped-Define DISPLAYED-OBJECTS CustNum Name SalesRep RepName 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE CustNum AS INTEGER FORMAT ">>>>9" INITIAL 0 
     LABEL "Cust Num" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 TOOLTIP "Please enter a customer number.".

DEFINE VARIABLE Name AS CHARACTER FORMAT "x(30)" 
     LABEL "Name" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1 TOOLTIP "Please enter a name.".

DEFINE VARIABLE RepName AS CHARACTER FORMAT "x(30)" 
     LABEL "Rep Name" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1 TOOLTIP "Please enter the Name of the Salesperson.".

DEFINE VARIABLE SalesRep AS CHARACTER FORMAT "x(4)" 
     LABEL "Sales Rep" 
     VIEW-AS FILL-IN 
     SIZE 9.6 BY 1 TOOLTIP "Please Enter a Sales Rep.".

DEFINE RECTANGLE tbCustomer
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9.8 BY .91.

DEFINE RECTANGLE tbInvoice
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 56 BY .91.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brwCustomer FOR 
      Customer SCROLLING.

DEFINE QUERY brwInvoice FOR 
      Invoice SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brwCustomer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brwCustomer C-Win _FREEFORM
  QUERY brwCustomer NO-LOCK DISPLAY
      Customer.CustNum FORMAT ">>>>9" LABEL "Cust.num" WIDTH 8
Customer.Name FORMAT "x(30)" LABEL "Name" WIDTH 30
Customer.City FORMAT "x(25)" LABEL "City" WIDTH 25
Customer.Discount FORMAT ">>9%" LABEL "Discount" WIDTH 8
Customer.Country FORMAT "x(20)" LABEL "Country" WIDTH 20
Customer.SalesRep FORMAT "x(4)" LABEL "Sales Rep" WIDTH 9.8
Customer.RepName FORMAT "x(30)" LABEL "Rep Name" WIDTH 30
Customer.Region FORMAT "x(8)" LABEL "Region" WIDTH 8
Customer.OrderDate FORMAT "99/99/99" LABEL "Ordered" WIDTH 9.2
Customer.OrderStatus FORMAT "x(20)" LABEL "Order Status" WIDTH 20
ENABLE Customer.CustNum
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 89 BY 8.57 ROW-HEIGHT-CHARS .57 FIT-LAST-COLUMN.

DEFINE BROWSE brwInvoice
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brwInvoice C-Win _FREEFORM
  QUERY brwInvoice NO-LOCK DISPLAY
      Invoice.Invoicenum FORMAT "zzzzzzzzz9":U LABEL 'Invoice Num'
Invoice.InvoiceDate FORMAT "99/99/9999":U LABEL 'Invoice Date'
Invoice.OrderNum FORMAT "zzzzzzzzz9":U LABEL 'Order Num'
Invoice.ShipCharge FORMAT "->>,>>9.99":U LABEL 'Ship Charge'
Invoice.TotalPaid FORMAT "->>,>>9.99":U LABEL 'Total Paid'
Invoice.Instructions FORMAT "x(50)":U LABEL 'Instructions'
Invoice.OrderDate FORMAT "99/99/99":U LABEL 'Ordered'
ENABLE Invoice.Invoicenum
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 86 BY 9.29 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     brwCustomer AT ROW 2.91 COL 4 WIDGET-ID 200
     CustNum AT ROW 12 COL 13 COLON-ALIGNED
     Name AT ROW 13 COL 13 COLON-ALIGNED
     SalesRep AT ROW 14 COL 13 COLON-ALIGNED
     RepName AT ROW 15 COL 13 COLON-ALIGNED
     brwInvoice AT ROW 18.38 COL 6 WIDGET-ID 300
     tbCustomer AT ROW 1.24 COL 2 WIDGET-ID 10
     tbInvoice AT ROW 16.71 COL 3 WIDGET-ID 12
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 138 BY 27.52 WIDGET-ID 100.


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
         HEIGHT             = 27.52
         WIDTH              = 138
         MAX-HEIGHT         = 27.52
         MAX-WIDTH          = 138
         VIRTUAL-HEIGHT     = 27.52
         VIRTUAL-WIDTH      = 138
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
/* BROWSE-TAB brwCustomer tbInvoice DEFAULT-FRAME */
/* BROWSE-TAB brwInvoice RepName DEFAULT-FRAME */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 27.52
       FRAME DEFAULT-FRAME:WIDTH            = 138.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brwCustomer
/* Query rebuild information for BROWSE brwCustomer
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Customer NO-LOCK
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _TblOptList       = ",, FIRST"
     _Query            is OPENED
*/  /* BROWSE brwCustomer */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brwInvoice
/* Query rebuild information for BROWSE brwInvoice
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Invoice NO-LOCK
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE brwInvoice */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <Insert window title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brwCustomer
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
    IF DYNAMIC-FUNCTION("getAttribute",THIS-PROCEDURE,"advGuiWin") = "YES" THEN
      RUN WaitForForm NO-ERROR.
  &ENDIF

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
  DISPLAY CustNum Name SalesRep RepName 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE tbCustomer tbInvoice brwCustomer CustNum Name SalesRep RepName 
         brwInvoice 
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
 
  oBrwCustomer = NEW JBoxBrowse(brwCustomer:HANDLE).

  oFmCustomer = NEW JBoxFieldMap(FRAME {&FRAME-NAME}:HANDLE).
  oFmCustomer:updateFields = 'CustNum,Name,SalesRep'.
  oFmCustomer:displayFields = 'RepName'.
  oFmCustomer:primaryKeyFields = 'CustNum'.

  oFmCustomer:BROWSE-OBJECT = oBrwCustomer.
  oTbCustomer = NEW JBoxToolbar(tbCustomer:HANDLE,"File").
  oTbCustomer:AddToolGroup('New,Edit,Undo,Save,Filter,Excel').

  oBrwCustomer:TOOLBAR-OBJECT = oTbCustomer.
  oFmCustomer:TOOLBAR-OBJECT = oTbCustomer.
 
  oBrwCustomer:OpenQuery().
  oBrwInvoice = NEW JBoxBrowse(brwInvoice:HANDLE).

  oTbInvoice = NEW JBoxToolbar(tbInvoice:HANDLE).
  oTbInvoice:AddToolGroup('New,Edit,Undo,Save,Filter,Excel').

  oBrwInvoice:TOOLBAR-OBJECT = oTbInvoice.
END.

DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,200,200,0,0).

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
DYNAMIC-FUNCTION("DoLockWindow",?).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

