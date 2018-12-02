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
/* &SCOPED-DEFINE AdvGuiWin */ 

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEF VAR bOk         AS LOG    NO-UNDO.
DEF VAR ix          AS INT    NO-UNDO.
DEF VAR hBrowse     AS HANDLE NO-UNDO.
DEF VAR hQuery      AS HANDLE NO-UNDO.
DEF VAR hToolbar    AS HANDLE NO-UNDO.
DEF VAR hFieldMap   AS HANDLE NO-UNDO.
DEF VAR oContainer  AS JBoxContainer NO-UNDO.


/*** Start instance property definitions for JBoxBrowse object oBrwCustomer ***/
DEF VAR oBrwCustomer AS JBoxBrowse NO-UNDO.
DEF TEMP-TABLE Customer
    FIELD Comments AS character
    FIELD Contact AS character
    FIELD Country AS character
    FIELD CreditLimit AS decimal
    FIELD CustNum AS integer
    FIELD Discount AS integer
    FIELD EmailAddress AS character
    FIELD Fax AS character
    FIELD Name AS character
    FIELD Phone AS character
    FIELD PostalCode AS character
    FIELD SalesRep AS character
    FIELD State AS character
    FIELD Terms AS character
    FIELD BillToID AS integer
    FIELD Carrier AS character
    FIELD Creditcard AS character
    FIELD CustNum2 AS integer
    FIELD Instructions AS character
    FIELD OrderDate AS date
    FIELD Ordernum AS integer
    FIELD OrderStatus AS character
    FIELD PO AS character
    FIELD PromiseDate AS date
    FIELD SalesRep2 AS character
    FIELD ShipDate AS date
    FIELD RowIdent1 AS CHARACTER 
    FIELD RowIdent2 AS CHARACTER 
    FIELD RowCount AS INTEGER
    FIELD jbCountDistinct AS INTEGER FORMAT '>>>,>>>,>>9' INIT 1
    FIELD jbAverage AS DECIMAL FORMAT '->>>,>>>,>>9.99'
    INDEX idxRowids  RowIdent1 RowIdent2
    .
DEF BUFFER v_Customer FOR Customer.


FUNCTION getBuffersAndFieldsBrwCustomer RETURNS CHARACTER():
  RETURN
    'Customer'
     + ';Comments'
     + ';Contact'
     + ';Country'
     + ';CreditLimit'
     + ';CustNum'
     + ';Discount'
     + ';EmailAddress'
     + ';Fax'
     + ';Name'
     + ';Phone'
     + ';PostalCode'
     + ';SalesRep'
     + ';State'
     + ';Terms'
  + ',Order'
     + ';BillToID'
     + ';Carrier'
     + ';Creditcard'
     + ';CustNum'
     + ';Instructions'
     + ';OrderDate'
     + ';Ordernum'
     + ';OrderStatus'
     + ';PO'
     + ';PromiseDate'
     + ';SalesRep'
     + ';ShipDate'
     .
END FUNCTION.
FUNCTION getQueryJoinBrwCustomer RETURNS CHARACTER():
  RETURN 'EACH Order OF Customer NO-LOCK'.
END FUNCTION.
DEF VAR opopupCustomer AS JBoxPopupMenu NO-UNDO.


DEF VAR oFmCustomer AS JBoxFieldMap NO-UNDO.


DEF VAR otbCustomer AS JBoxToolbar NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BrwCustomer

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Customer

/* Definitions for BROWSE BrwCustomer                                   */
&Scoped-define FIELDS-IN-QUERY-BrwCustomer Customer.Comments ~
Customer.Contact Customer.Country Customer.CreditLimit Customer.CustNum ~
Customer.Discount Customer.EmailAddress Customer.Fax Customer.Name ~
Customer.Phone Customer.PostalCode Customer.SalesRep Customer.State ~
Customer.Terms Customer.BillToID Customer.Carrier Customer.Creditcard ~
Customer.CustNum2 Customer.Instructions Customer.OrderDate ~
Customer.Ordernum Customer.OrderStatus Customer.PO Customer.PromiseDate ~
Customer.SalesRep2 Customer.ShipDate 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BrwCustomer Customer.Comments 
&Scoped-define QUERY-STRING-BrwCustomer FOR EACH Customer NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BrwCustomer OPEN QUERY BrwCustomer FOR EACH Customer NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BrwCustomer Customer
&Scoped-define FIRST-TABLE-IN-QUERY-BrwCustomer Customer


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tbCustomer new_tbCustomer copy_tbCustomer ~
undo_tbCustomer delete_tbCustomer save_tbCustomer excel_tbCustomer ~
BrwCustomer EmailAddress Fax btnFax Carrier Name Creditcard Phone CustNum ~
PostalCode Instructions SalesRep State Terms OrderDate BillToID Ordernum 
&Scoped-Define DISPLAYED-OBJECTS EmailAddress Fax Carrier Name Creditcard ~
Phone CustNum PostalCode Instructions SalesRep State Terms OrderDate ~
BillToID Ordernum 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnFax 
     LABEL "..." 
     SIZE 4 BY 1.

DEFINE BUTTON copy_tbCustomer 
     IMAGE-UP FILE "bmp/copy.bmp":U
     LABEL "Kopier" 
     SIZE 6 BY 1.52 TOOLTIP "Kopier (ALT-K)".

DEFINE BUTTON delete_tbCustomer 
     IMAGE-UP FILE "bmp/del16e.bmp":U
     LABEL "Slett" 
     SIZE 6 BY 1.52 TOOLTIP "Slett (CTRL-D)".

DEFINE BUTTON excel_tbCustomer 
     IMAGE-UP FILE "gif/afexcel.gif":U
     LABEL "Eksporter til E&xcel" 
     SIZE 6 BY 1.52 TOOLTIP "Eksporter til E&xcel (ALT-X)".

DEFINE BUTTON new_tbCustomer 
     IMAGE-UP FILE "bmp/new16e.bmp":U
     LABEL "Ny" 
     SIZE 6 BY 1.52 TOOLTIP "Ny (CTRL-N)".

DEFINE BUTTON save_tbCustomer 
     IMAGE-UP FILE "bmp/save.bmp":U
     LABEL "Lagre" 
     SIZE 6 BY 1.52 TOOLTIP "Lagre (CTRL-S)".

DEFINE BUTTON undo_tbCustomer 
     IMAGE-UP FILE "bmp/undo16e.bmp":U
     LABEL "Angre" 
     SIZE 6 BY 1.52 TOOLTIP "Angre (CTRL-Z)".

DEFINE VARIABLE Instructions AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 52 BY 2.71 TOOLTIP "Please enter Instructions".

DEFINE VARIABLE BillToID AS INTEGER FORMAT "zzzzzzzzz9" INITIAL 0 
     LABEL "Bill To ID" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 TOOLTIP "Please enter the BillTo ID.".

DEFINE VARIABLE Carrier AS CHARACTER FORMAT "x(25)" 
     LABEL "Carrier" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1 TOOLTIP "Please enter the carrier.".

DEFINE VARIABLE Creditcard AS CHARACTER FORMAT "x(20)" INITIAL "Visa" 
     LABEL "Credit Card" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 TOOLTIP "Please enter the credit card.".

DEFINE VARIABLE CustNum AS INTEGER FORMAT ">>>>9" INITIAL 0 
     LABEL "Cust Num" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 TOOLTIP "Please enter an existing customer number.".

DEFINE VARIABLE EmailAddress AS CHARACTER FORMAT "x(50)" 
     LABEL "Email" 
     VIEW-AS FILL-IN 
     SIZE 52 BY 1 TOOLTIP "Please enter an full Internet Email Address.".

DEFINE VARIABLE Fax AS CHARACTER FORMAT "x(20)" 
     LABEL "Fax" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 TOOLTIP "Please enter a fax number.".

DEFINE VARIABLE Name AS CHARACTER FORMAT "x(30)" 
     LABEL "Name" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1 TOOLTIP "Please enter a name.".

DEFINE VARIABLE OrderDate AS DATE FORMAT "99/99/99" 
     LABEL "Ordered" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1 TOOLTIP "Please enter the date of order.".

DEFINE VARIABLE Ordernum AS INTEGER FORMAT "zzzzzzzzz9" INITIAL 0 
     LABEL "Order Num" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 TOOLTIP "Please enter an order number.".

DEFINE VARIABLE Phone AS CHARACTER FORMAT "x(20)" 
     LABEL "Phone" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 TOOLTIP "Please enter a phone number".

DEFINE VARIABLE PostalCode AS CHARACTER FORMAT "x(10)" 
     LABEL "Postal Code" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 TOOLTIP "Please enter the appropriate Postal Code.".

DEFINE VARIABLE SalesRep AS CHARACTER FORMAT "x(4)" 
     LABEL "Sales Rep" 
     VIEW-AS FILL-IN 
     SIZE 9.6 BY 1 TOOLTIP "Please Enter a Sales Rep.".

DEFINE VARIABLE State AS CHARACTER FORMAT "x(20)" 
     LABEL "State" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 TOOLTIP "Please enter standard state abbreviation.".

DEFINE VARIABLE Terms AS CHARACTER FORMAT "x(20)" INITIAL "Net30" 
     LABEL "Terms" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 TOOLTIP "Please enter terms".

DEFINE RECTANGLE tbCustomer
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 158 BY 1.71.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BrwCustomer FOR 
      Customer SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BrwCustomer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BrwCustomer C-Win _STRUCTURED
  QUERY BrwCustomer NO-LOCK DISPLAY
      Customer.Comments COLUMN-LABEL "Comments" FORMAT "x(80)":U
            WIDTH 41.2
      Customer.Contact COLUMN-LABEL "Contact" FORMAT "x(30)":U
      Customer.Country COLUMN-LABEL "Country" FORMAT "x(20)":U
      Customer.CreditLimit COLUMN-LABEL "Credit Limit" FORMAT "->,>>>,>>9":U
      Customer.CustNum COLUMN-LABEL "Cust Num" FORMAT ">>>>9":U
      Customer.Discount COLUMN-LABEL "Discount" FORMAT ">>9%":U
      Customer.EmailAddress COLUMN-LABEL "Email" FORMAT "x(50)":U
      Customer.Fax COLUMN-LABEL "Fax" FORMAT "x(20)":U
      Customer.Name COLUMN-LABEL "Name" FORMAT "x(30)":U
      Customer.Phone COLUMN-LABEL "Phone" FORMAT "x(20)":U
      Customer.PostalCode COLUMN-LABEL "Postal Code" FORMAT "x(10)":U
      Customer.SalesRep COLUMN-LABEL "Sales Rep" FORMAT "x(4)":U
      Customer.State COLUMN-LABEL "State" FORMAT "x(20)":U
      Customer.Terms COLUMN-LABEL "Terms" FORMAT "x(20)":U
      Customer.BillToID COLUMN-LABEL "Bill To ID" FORMAT "zzzzzzzzz9":U
            WIDTH 10.2
      Customer.Carrier COLUMN-LABEL "Carrier" FORMAT "x(25)":U
      Customer.Creditcard COLUMN-LABEL "Credit Card" FORMAT "x(20)":U
      Customer.CustNum2 COLUMN-LABEL "Cust Num" FORMAT ">>>>9":U
      Customer.Instructions COLUMN-LABEL "Instructions" FORMAT "x(50)":U
      Customer.OrderDate COLUMN-LABEL "Ordered" FORMAT "99/99/99":U
      Customer.Ordernum COLUMN-LABEL "Order Num" FORMAT "zzzzzzzzz9":U
            WIDTH 10.2
      Customer.OrderStatus COLUMN-LABEL "Order Status" FORMAT "x(20)":U
      Customer.PO COLUMN-LABEL "PO" FORMAT "x(20)":U
      Customer.PromiseDate COLUMN-LABEL "Promised" FORMAT "99/99/99":U
      Customer.SalesRep2 COLUMN-LABEL "Sales Rep" FORMAT "x(4)":U
      Customer.ShipDate COLUMN-LABEL "Shipped" FORMAT "99/99/9999":U
  ENABLE
      Customer.Comments HELP "Please enter comments."
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 156.8 BY 12.86 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     new_tbCustomer AT ROW 1.33 COL 2.2 WIDGET-ID 4
     copy_tbCustomer AT ROW 1.33 COL 8.4 WIDGET-ID 6
     undo_tbCustomer AT ROW 1.33 COL 14.4 WIDGET-ID 8
     delete_tbCustomer AT ROW 1.33 COL 20.4 WIDGET-ID 10
     save_tbCustomer AT ROW 1.33 COL 26.4 WIDGET-ID 12
     excel_tbCustomer AT ROW 1.33 COL 32.4 WIDGET-ID 14
     BrwCustomer AT ROW 3.05 COL 2.2 WIDGET-ID 200
     EmailAddress AT ROW 16.48 COL 14 COLON-ALIGNED
     Fax AT ROW 17.48 COL 14 COLON-ALIGNED
     btnFax AT ROW 17.48 COL 38 WIDGET-ID 18 NO-TAB-STOP 
     Carrier AT ROW 18.38 COL 63 COLON-ALIGNED
     Name AT ROW 18.48 COL 14 COLON-ALIGNED
     Creditcard AT ROW 19.38 COL 63 COLON-ALIGNED
     Phone AT ROW 19.48 COL 14 COLON-ALIGNED
     CustNum AT ROW 20.38 COL 63 COLON-ALIGNED
     PostalCode AT ROW 20.48 COL 14 COLON-ALIGNED
     Instructions AT ROW 21.38 COL 65 NO-LABEL WIDGET-ID 16
     SalesRep AT ROW 21.48 COL 14 COLON-ALIGNED
     State AT ROW 22.48 COL 14 COLON-ALIGNED
     Terms AT ROW 23.48 COL 14 COLON-ALIGNED
     OrderDate AT ROW 24.33 COL 63 COLON-ALIGNED
     BillToID AT ROW 24.48 COL 14 COLON-ALIGNED
     Ordernum AT ROW 25.33 COL 63 COLON-ALIGNED
     tbCustomer AT ROW 1.24 COL 2 WIDGET-ID 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 159.2 BY 25.71 WIDGET-ID 100.


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
         HEIGHT             = 25.62
         WIDTH              = 159.4
         MAX-HEIGHT         = 29.95
         MAX-WIDTH          = 159.4
         VIRTUAL-HEIGHT     = 29.95
         VIRTUAL-WIDTH      = 159.4
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
/* BROWSE-TAB BrwCustomer excel_tbCustomer DEFAULT-FRAME */
ASSIGN 
       BrwCustomer:PRIVATE-DATA IN FRAME DEFAULT-FRAME           = 
                "multiSortBrowse;sort on multiple columns".

ASSIGN 
       tbCustomer:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "new;Ny,copy;Kopier,undo;Angre,delete;Slett,save;Lagre,excel;Eksporter til E&xcelmaxborder".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BrwCustomer
/* Query rebuild information for BROWSE BrwCustomer
     _TblList          = "sports116.Customer"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > "_<CALC>"
"Customer.Comments" "Comments" "x(80)" "character" ? ? ? ? ? ? yes "Please enter comments." no no "41.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > "_<CALC>"
"Customer.Contact" "Contact" "x(30)" "character" ? ? ? ? ? ? no "Please enter a contact." no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > "_<CALC>"
"Customer.Country" "Country" "x(20)" "character" ? ? ? ? ? ? no "Please enter a country." no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > "_<CALC>"
"Customer.CreditLimit" "Credit Limit" "->,>>>,>>9" "decimal" ? ? ? ? ? ? no "Please enter a Credit Limit." no no "10.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > "_<CALC>"
"Customer.CustNum" "Cust Num" ">>>>9" "integer" ? ? ? ? ? ? no "Please enter a customer number." no no "9.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > "_<CALC>"
"Customer.Discount" "Discount" ">>9%" "integer" ? ? ? ? ? ? no "Please enter a percentage from 0 to 100." no no "8.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > "_<CALC>"
"Customer.EmailAddress" "Email" "x(50)" "character" ? ? ? ? ? ? no "Please enter an full Internet Email Address." no no "50" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > "_<CALC>"
"Customer.Fax" "Fax" "x(20)" "character" ? ? ? ? ? ? no "Please enter a fax number." no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > "_<CALC>"
"Customer.Name" "Name" "x(30)" "character" ? ? ? ? ? ? no "Please enter a name." no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > "_<CALC>"
"Customer.Phone" "Phone" "x(20)" "character" ? ? ? ? ? ? no "Please enter a phone number" no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > "_<CALC>"
"Customer.PostalCode" "Postal Code" "x(10)" "character" ? ? ? ? ? ? no "Please enter the appropriate Postal Code." no no "11.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > "_<CALC>"
"Customer.SalesRep" "Sales Rep" "x(4)" "character" ? ? ? ? ? ? no "Please Enter a Sales Rep." no no "9.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > "_<CALC>"
"Customer.State" "State" "x(20)" "character" ? ? ? ? ? ? no "Please enter standard state abbreviation." no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > "_<CALC>"
"Customer.Terms" "Terms" "x(20)" "character" ? ? ? ? ? ? no "Please enter terms" no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > "_<CALC>"
"Customer.BillToID" "Bill To ID" "zzzzzzzzz9" "integer" ? ? ? ? ? ? no "Please enter the BillTo ID." no no "10.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > "_<CALC>"
"Customer.Carrier" "Carrier" "x(25)" "character" ? ? ? ? ? ? no "Please enter the carrier." no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > "_<CALC>"
"Customer.Creditcard" "Credit Card" "x(20)" "character" ? ? ? ? ? ? no "Please enter the credit card." no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > "_<CALC>"
"Customer.CustNum2" "Cust Num" ">>>>9" "integer" ? ? ? ? ? ? no "Please enter an existing customer number." no no "9.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > "_<CALC>"
"Customer.Instructions" "Instructions" "x(50)" "character" ? ? ? ? ? ? no "Please enter Instructions" no no "50" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[20]   > "_<CALC>"
"Customer.OrderDate" "Ordered" "99/99/99" "date" ? ? ? ? ? ? no "Please enter the date of order." no no "9.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[21]   > "_<CALC>"
"Customer.Ordernum" "Order Num" "zzzzzzzzz9" "integer" ? ? ? ? ? ? no "Please enter an order number." no no "10.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[22]   > "_<CALC>"
"Customer.OrderStatus" "Order Status" "x(20)" "character" ? ? ? ? ? ? no "Please enter the Order Status." no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[23]   > "_<CALC>"
"Customer.PO" "PO" "x(20)" "character" ? ? ? ? ? ? no "Please enter the PO." no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[24]   > "_<CALC>"
"Customer.PromiseDate" "Promised" "99/99/99" "date" ? ? ? ? ? ? no "Please enter the Promise Date." no no "9.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[25]   > "_<CALC>"
"Customer.SalesRep2" "Sales Rep" "x(4)" "character" ? ? ? ? ? ? no "Please enter the Sales Rep." no no "9.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[26]   > "_<CALC>"
"Customer.ShipDate" "Shipped" "99/99/9999" "date" ? ? ? ? ? ? no "Please enter the ship date." no no "11.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BrwCustomer */
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


&Scoped-define SELF-NAME btnFax
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFax C-Win
ON CHOOSE OF btnFax IN FRAME DEFAULT-FRAME /* ... */
DO:
  DEF VAR cRowIdList  AS CHAR NO-UNDO.
  DEF VAR cIdList     AS CHAR NO-UNDO.  
  DEF VAR bOk         AS LOG  NO-UNDO.

  /* Uncomment and modify to fetch pre-selected rows from database:
  cRowIdList = DYNAMIC-FUNCTION("getRowIdList",
                                "<table>",       /* Buffer(list) for query */
                                "",              /* Name of buffer to fetch rowid from. Blank: Last buffer in bufferlist) */
                                "where Fax begins 'b'").
  */

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "<table>"      
                      + ";Fax"  
                      + ";<field2>"
                      ,"where true",
                      INPUT-OUTPUT cRowIdList,
                      "Fax", /* Primary key */
                      INPUT-OUTPUT cIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk THEN
    MESSAGE PROGRAM-NAME(1) SKIP
            cIdList SKIP
            cRowIdList
            VIEW-AS ALERT-BOX.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE documentsRecord C-Win 
PROCEDURE documentsRecord :
RUN SUPER.


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
  DISPLAY EmailAddress Fax Carrier Name Creditcard Phone CustNum PostalCode 
          Instructions SalesRep State Terms OrderDate BillToID Ordernum 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE tbCustomer new_tbCustomer copy_tbCustomer undo_tbCustomer 
         delete_tbCustomer save_tbCustomer excel_tbCustomer BrwCustomer 
         EmailAddress Fax btnFax Carrier Name Creditcard Phone CustNum 
         PostalCode Instructions SalesRep State Terms OrderDate BillToID 
         Ordernum 
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

oContainer = NEW JBoxContainer().
oContainer:addStatusBar().

DO WITH FRAME {&FRAME-NAME}:

  oBrwCustomer = NEW JBoxBrowse(brwCustomer:HANDLE).

  opopupCustomer = NEW JBoxPopupMenu().
  opopupCustomer:AddToolGroup('multiSortBrowse;sort on multiple columns').

  oBrwCustomer:POPUP-MENU-OBJECT = opopupCustomer.
  oFmCustomer = NEW JBoxFieldMap(FRAME {&FRAME-NAME}:HANDLE).
  oFmCustomer:updateFields = 'EmailAddress,Fax,Name,Phone,PostalCode,SalesRep,State,Terms'.
  oFmCustomer:displayFields = 'BillToID,Carrier,Creditcard,CustNum,Instructions,OrderDate,Ordernum'.

  oFmCustomer:BROWSE-OBJECT = oBrwCustomer.
  otbCustomer = NEW JBoxToolbar(tbCustomer:HANDLE).

  oBrwCustomer:TOOLBAR-OBJECT = otbCustomer.
  oFmCustomer:TOOLBAR-OBJECT = otbCustomer.
END.
oBrwCustomer:OpenQuery().


oContainer:initResize(1000,200).

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

