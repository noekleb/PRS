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

DEF VAR bOk         AS LOG    NO-UNDO.
DEF VAR ix          AS INT    NO-UNDO.
DEF VAR hBrowse     AS HANDLE NO-UNDO.
DEF VAR hQuery      AS HANDLE NO-UNDO.
DEF VAR hToolbar    AS HANDLE NO-UNDO.
DEF VAR hFieldMap   AS HANDLE NO-UNDO.

/*** Start instance property definitions for JBoxBrowse object oBrwCustomer ***/
DEF VAR oBrwCustomer AS JBoxBrowse NO-UNDO.

DEF TEMP-TABLE Customer
    FIELD CustOrderTotal AS DECIMAL
    FIELD Name AS character
    FIELD address AS character
    FIELD City AS character
    FIELD Fax AS character
    FIELD Country AS character
    FIELD Balance AS decimal
    FIELD SalesRep AS character
    FIELD CustNum AS integer
    FIELD CustNum2 AS integer
    FIELD Ordernum AS integer
    FIELD OrderStatus AS character
    FIELD OrderDate AS date
    FIELD MonthQuota_1 AS integer
    FIELD jbextent_1_MonthQuota AS integer /* placeholder for calculation */
    FIELD MonthQuota_2 AS integer
    FIELD jbextent_2_MonthQuota AS integer /* placeholder for calculation */
    FIELD RepName AS character
    FIELD Region AS character
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
     + ';Name'
     + ';address'
     + ';City'
     + ';Fax'
     + ';Country'
     + ';Balance'
     + ';SalesRep'
     + ';CustNum'
     + ';+CustOrderTotal|DECIMAL||CustOrderTotal(CustNum)'
  + ',Order'
     + ';CustNum'
     + ';Ordernum'
     + ';OrderStatus'
     + ';OrderDate'
  + ',Salesrep'
     + ';MonthQuota[1]'
     + ';MonthQuota[2]'
     + ';RepName'
     + ';Region'
     .
END FUNCTION.
FUNCTION getQueryJoinBrwCustomer RETURNS CHARACTER():
  RETURN 'FIRST Order OF Customer OUTER-JOIN NO-LOCK,FIRST Salesrep OF Customer NO-LOCK'.
END FUNCTION.
FUNCTION getCalcFieldProcBrwCustomer RETURNS CHARACTER():
  RETURN 
     'customer_browsecalc.p' /* CustOrderTotal(CustNum) */
     .
END FUNCTION.

/*** End instance property settings for JBoxBrowse object oBrwCustomer ***/

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
&Scoped-define INTERNAL-TABLES Customer

/* Definitions for BROWSE brwCustomer                                   */
&Scoped-define FIELDS-IN-QUERY-brwCustomer Customer.CustOrderTotal ~
Customer.Name Customer.address Customer.City Customer.Fax Customer.Country ~
Customer.Balance Customer.SalesRep Customer.CustNum Customer.CustNum2 ~
Customer.Ordernum Customer.OrderStatus Customer.OrderDate ~
Customer.MonthQuota_1 Customer.MonthQuota_2 Customer.RepName ~
Customer.Region 
&Scoped-define ENABLED-FIELDS-IN-QUERY-brwCustomer Customer.CustOrderTotal ~
Customer.CustNum2 
&Scoped-define ENABLED-TABLES-IN-QUERY-brwCustomer Customer
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-brwCustomer Customer
&Scoped-define QUERY-STRING-brwCustomer FOR EACH Customer NO-LOCK, ~
    FIRST Order OF Customer OUTER-JOIN NO-LOCK, ~
    FIRST Salesrep OF Customer NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-brwCustomer OPEN QUERY brwCustomer FOR EACH Customer NO-LOCK, ~
    FIRST Order OF Customer OUTER-JOIN NO-LOCK, ~
    FIRST Salesrep OF Customer NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-brwCustomer Customer
&Scoped-define FIRST-TABLE-IN-QUERY-brwCustomer Customer


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS brwCustomer 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brwCustomer FOR 
      Customer SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brwCustomer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brwCustomer C-Win _STRUCTURED
  QUERY brwCustomer NO-LOCK DISPLAY
      Customer.CustOrderTotal COLUMN-LABEL "Order tot" FORMAT "->>,>>9.99":U
            WIDTH 35.6
      Customer.Name COLUMN-LABEL "Name" FORMAT "x(30)":U WIDTH 31.2
      Customer.address COLUMN-LABEL "Address" FORMAT "x(30)":U
      Customer.City COLUMN-LABEL "City" FORMAT "x(25)":U
      Customer.Fax COLUMN-LABEL "Fax" FORMAT "x(20)":U
      Customer.Country COLUMN-LABEL "Country" FORMAT "x(20)":U
      Customer.Balance COLUMN-LABEL "Balance" FORMAT "->,>>>,>>9.99":U
      Customer.SalesRep COLUMN-LABEL "Sales Rep" FORMAT "x(4)":U
      Customer.CustNum FORMAT ">>>>9":U
      Customer.CustNum2 COLUMN-LABEL "Custnum" FORMAT ">>>>9":U
      Customer.Ordernum COLUMN-LABEL "Order Num" FORMAT "zzzzzzzzz9":U
            WIDTH 10.2
      Customer.OrderStatus COLUMN-LABEL "Order Status" FORMAT "x(20)":U
      Customer.OrderDate COLUMN-LABEL "Ordered" FORMAT "99/99/99":U
      Customer.MonthQuota_1 COLUMN-LABEL "Month Quota" FORMAT "->,>>>,>>9":U
      Customer.MonthQuota_2 COLUMN-LABEL "Month Quota" FORMAT "->,>>>,>>9":U
      Customer.RepName COLUMN-LABEL "Rep Name" FORMAT "x(30)":U
      Customer.Region COLUMN-LABEL "Region" FORMAT "x(8)":U
  ENABLE
      Customer.CustOrderTotal
      Customer.CustNum2
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 112 BY 9.05 ROW-HEIGHT-CHARS .52 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     brwCustomer AT ROW 2.91 COL 3 WIDGET-ID 200
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 114.4 BY 11.38 WIDGET-ID 100.


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
         HEIGHT             = 11.33
         WIDTH              = 115
         MAX-HEIGHT         = 23.52
         MAX-WIDTH          = 115
         VIRTUAL-HEIGHT     = 23.52
         VIRTUAL-WIDTH      = 115
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
/* BROWSE-TAB brwCustomer 1 DEFAULT-FRAME */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 11.38
       FRAME DEFAULT-FRAME:WIDTH            = 114.4.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brwCustomer
/* Query rebuild information for BROWSE brwCustomer
     _TblList          = "sports113.Customer"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _TblOptList       = ", FIRST OUTER, FIRST"
     _FldNameList[1]   > "_<CALC>"
"Customer.CustOrderTotal" "Order tot" "->>,>>9.99" "DECIMAL" ? ? ? ? ? ? yes "" no no "35.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Customer.Name
"Customer.Name" "Name" "x(30)" "character" ? ? ? ? ? ? no "" no no "31.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Customer.address
"Customer.address" "Address" "x(30)" "character" ? ? ? ? ? ? no "" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Customer.City
"Customer.City" "City" "x(25)" "character" ? ? ? ? ? ? no "Please enter a city." no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Customer.Fax
"Customer.Fax" "Fax" "x(20)" "character" ? ? ? ? ? ? no "Please enter a fax number." no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Customer.Country
"Customer.Country" "Country" "x(20)" "character" ? ? ? ? ? ? no "Please enter a country." no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Customer.Balance
"Customer.Balance" "Balance" "->,>>>,>>9.99" "decimal" ? ? ? ? ? ? no "Please enter a balance." no no "13.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > Customer.SalesRep
"Customer.SalesRep" "Sales Rep" "x(4)" "character" ? ? ? ? ? ? no "Please Enter a Sales Rep." no no "9.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > Customer.CustNum
"Customer.CustNum" "Cust Num" ">>>>9" "integer" ? ? ? ? ? ? no "Please enter a customer number." no no "9.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > Customer.CustNum2
"Customer.CustNum2" "Custnum" ">>>>9" "integer" ? ? ? ? ? ? yes "" no no "8.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > Customer.Ordernum
"Customer.Ordernum" "Order Num" "zzzzzzzzz9" "integer" ? ? ? ? ? ? no "Please enter an order number." no no "10.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > Customer.OrderStatus
"Customer.OrderStatus" "Order Status" "x(20)" "character" ? ? ? ? ? ? no "Please enter the Order Status." no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > Customer.OrderDate
"Customer.OrderDate" "Ordered" "99/99/99" "date" ? ? ? ? ? ? no "Please enter the date of order." no no "9.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > Customer.MonthQuota_1
"Customer.MonthQuota_1" "Month Quota" "->,>>>,>>9" "integer" ? ? ? ? ? ? no "Please enter the Month Quota." no no "12.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > Customer.MonthQuota_2
"Customer.MonthQuota_2" "Month Quota" "->,>>>,>>9" "integer" ? ? ? ? ? ? no "Please enter the Month Quota." no no "12.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > Customer.RepName
"Customer.RepName" "Rep Name" "x(30)" "character" ? ? ? ? ? ? no "Please enter the Name of the Salesperson." no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > Customer.Region
"Customer.Region" "Region" "x(8)" "character" ? ? ? ? ? ? no "Please enter the Sales Region covered by this salesman." no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE brwCustomer */
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
  ENABLE brwCustomer 
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

  oBrwCustomer:OpenQuery().

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

