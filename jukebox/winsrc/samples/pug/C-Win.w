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
&SCOPED-DEFINE AdvGuiWin 

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEF VAR bOk         AS LOG    NO-UNDO.
DEF VAR ix          AS INT    NO-UNDO.
DEF VAR hBrowse     AS HANDLE NO-UNDO.
DEF VAR hQuery      AS HANDLE NO-UNDO.
DEF VAR hToolbar    AS HANDLE NO-UNDO.
DEF VAR hFieldMap   AS HANDLE NO-UNDO.


/*** Start instance property definitions for JBoxBrowse object oBrwOrder ***/
DEF VAR oBrwOrder AS JBoxBrowse NO-UNDO.
DEF TEMP-TABLE Order
    FIELD Ordernum AS integer
    FIELD OrderDate AS date
    FIELD PromiseDate AS date
    FIELD OrderStatus AS character
    FIELD OrderTotal AS DECIMAL
    FIELD Balance AS decimal
    FIELD CustNum AS integer
    FIELD SalesRep AS character
    FIELD Instructions AS character
    FIELD RepName AS character
    FIELD Name AS character
    FIELD RowIdent1 AS CHARACTER 
    FIELD RowIdent2 AS CHARACTER 
    FIELD RowIdent3 AS CHARACTER 
    FIELD RowCount AS INTEGER
    FIELD jbCountDistinct AS INTEGER FORMAT '>>>,>>>,>>9' INIT 1
    FIELD jbAverage AS DECIMAL FORMAT '->>>,>>>,>>9.99'
    INDEX idxRowids  RowIdent1 RowIdent2 RowIdent3
    .
DEF BUFFER v_Order FOR Order.


FUNCTION getBuffersAndFieldsBrwOrder RETURNS CHARACTER():
  RETURN
    'Order'
     + ';Ordernum'
     + ';OrderDate'
     + ';PromiseDate'
     + ';OrderStatus'
     + ';CustNum'
     + ';SalesRep'
     + ';Instructions'
     + ';+OrderTotal|DECIMAL||jb_total(EACH OrderLine OF Order¤Qty * Price)'
  + ',Salesrep'
     + ';RepName'
  + ',Customer'
     + ';Balance'
     + ';Name'
     .
END FUNCTION.
FUNCTION getQueryJoinBrwOrder RETURNS CHARACTER():
  RETURN 'EACH Salesrep OF Order NO-LOCK,EACH Customer OF Order NO-LOCK'.
END FUNCTION.


DEF VAR oFmOrder AS JBoxFieldMap NO-UNDO.


DEF VAR oOrderDate_JBoxDevExEdit AS JBoxDevExDateEdit NO-UNDO.

DEF VAR oPromiseDate_JBoxDevExEdit AS JBoxDevExDateEdit NO-UNDO.

DEF VAR oInstructions_JBoxDevExEdit AS JBoxDevExEdit NO-UNDO.


DEF VAR oTbOrder AS JBoxToolbar NO-UNDO.

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
&Scoped-define FIELDS-IN-QUERY-BrwOrder Order.Ordernum Order.OrderDate ~
Order.PromiseDate Order.OrderStatus Order.OrderTotal Order.Balance ~
Order.CustNum Order.SalesRep Order.Instructions Order.RepName Order.Name 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BrwOrder Order.Ordernum ~
Order.OrderTotal 
&Scoped-define QUERY-STRING-BrwOrder FOR EACH Order NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BrwOrder OPEN QUERY BrwOrder FOR EACH Order NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BrwOrder Order
&Scoped-define FIRST-TABLE-IN-QUERY-BrwOrder Order


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tbOrder new_tbOrder edit_tbOrder ~
copy_tbOrder undo_tbOrder save_tbOrder delete_tbOrder refresh_tbOrder ~
filter_tbOrder browseconfig_tbOrder excel_tbOrder BrwOrder Ordernum ~
OrderStatus OrderDate PromiseDate CustNum btnCustNum Name SalesRep ~
btnSalesRep Balance RepName Instructions 
&Scoped-Define DISPLAYED-OBJECTS Ordernum OrderStatus OrderDate PromiseDate ~
CustNum Name SalesRep Balance RepName Instructions 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON browseconfig_tbOrder 
     IMAGE-UP FILE "bmp/table.bmp":U
     LABEL "Button 9" 
     SIZE 4.6 BY 1.1.

DEFINE BUTTON btnCustNum 
     LABEL "..." 
     SIZE 4 BY 1.

DEFINE BUTTON btnSalesRep 
     LABEL "..." 
     SIZE 4 BY 1.

DEFINE BUTTON copy_tbOrder 
     IMAGE-UP FILE "bmp/copy.bmp":U
     LABEL "Button 3" 
     SIZE 4.6 BY 1.1.

DEFINE BUTTON delete_tbOrder 
     IMAGE-UP FILE "bmp/del16e.bmp":U
     LABEL "Button 6" 
     SIZE 4.6 BY 1.1.

DEFINE BUTTON edit_tbOrder 
     IMAGE-UP FILE "bmp/edit16e.bmp":U
     LABEL "Button 2" 
     SIZE 4.6 BY 1.1.

DEFINE BUTTON excel_tbOrder 
     IMAGE-UP FILE "gif/afexcel.gif":U
     LABEL "Button 10" 
     SIZE 4.6 BY 1.1.

DEFINE BUTTON filter_tbOrder 
     IMAGE-UP FILE "gif/filter.gif":U
     LABEL "Button 8" 
     SIZE 4.6 BY 1.1.

DEFINE BUTTON new_tbOrder 
     IMAGE-UP FILE "bmp/new16e.bmp":U
     LABEL "Button 1" 
     SIZE 4.6 BY 1.1.

DEFINE BUTTON refresh_tbOrder 
     IMAGE-UP FILE "gif/refresh.gif":U
     LABEL "Button 7" 
     SIZE 4.6 BY 1.1.

DEFINE BUTTON save_tbOrder 
     IMAGE-UP FILE "bmp/save.bmp":U
     LABEL "Button 5" 
     SIZE 4.6 BY 1.1.

DEFINE BUTTON undo_tbOrder 
     IMAGE-UP FILE "bmp/undo16e.bmp":U
     LABEL "Button 4" 
     SIZE 4.6 BY 1.1.

DEFINE VARIABLE OrderStatus AS CHARACTER FORMAT "x(20)" INITIAL "Ordered" 
     LABEL "Order Status" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Ordered","Ordered",
                     "Shipped","Shipped"
     DROP-DOWN-LIST
     SIZE 22 BY 1 TOOLTIP "Please enter the Order Status.".

DEFINE VARIABLE Instructions AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 77 BY 3.19 TOOLTIP "Please enter Instructions".

DEFINE VARIABLE Balance AS DECIMAL FORMAT "->,>>>,>>9.99" INITIAL 0 
     LABEL "Balance" 
     VIEW-AS FILL-IN 
     SIZE 20.2 BY 1 TOOLTIP "Please enter a balance.".

DEFINE VARIABLE CustNum AS INTEGER FORMAT ">>>>9" INITIAL 0 
     LABEL "Cust Num" 
     VIEW-AS FILL-IN 
     SIZE 9.6 BY 1 TOOLTIP "Please enter an existing customer number.".

DEFINE VARIABLE Name AS CHARACTER FORMAT "x(30)" 
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

DEFINE VARIABLE PromiseDate AS DATE FORMAT "99/99/99" 
     LABEL "Promised" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1 TOOLTIP "Please enter the Promise Date.".

DEFINE VARIABLE RepName AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1 TOOLTIP "Please enter the Name of the Salesperson.".

DEFINE VARIABLE SalesRep AS CHARACTER FORMAT "x(4)" 
     LABEL "Sales Rep" 
     VIEW-AS FILL-IN 
     SIZE 9.6 BY 1 TOOLTIP "Please enter the Sales Rep.".

DEFINE RECTANGLE tbOrder
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 109 BY 1.29.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BrwOrder FOR 
      Order SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BrwOrder
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BrwOrder C-Win _STRUCTURED
  QUERY BrwOrder NO-LOCK DISPLAY
      Order.Ordernum COLUMN-LABEL "Order Num" FORMAT "zzzzzzzzz9":U
            WIDTH 10.2
      Order.OrderDate COLUMN-LABEL "Ordered" FORMAT "99/99/99":U
      Order.PromiseDate COLUMN-LABEL "Promised" FORMAT "99/99/99":U
      Order.OrderStatus COLUMN-LABEL "Order Status" FORMAT "x(20)":U
      Order.OrderTotal COLUMN-LABEL "Total" FORMAT "->>,>>>,>>9.99":U
      Order.Balance COLUMN-LABEL "Balance" FORMAT "->,>>>,>>9.99":U
      Order.CustNum COLUMN-LABEL "Cust Num" FORMAT ">>>>9":U
      Order.SalesRep COLUMN-LABEL "Sales Rep" FORMAT "x(4)":U
      Order.Instructions COLUMN-LABEL "Instructions" FORMAT "x(50)":U
      Order.RepName COLUMN-LABEL "Rep Name" FORMAT "x(30)":U
      Order.Name COLUMN-LABEL "Name" FORMAT "x(30)":U
  ENABLE
      Order.Ordernum HELP "Please enter an order number."
      Order.OrderTotal
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 107 BY 7.86 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     new_tbOrder AT ROW 1.33 COL 2.2 WIDGET-ID 32
     edit_tbOrder AT ROW 1.33 COL 6.8 WIDGET-ID 34
     copy_tbOrder AT ROW 1.33 COL 11.4 WIDGET-ID 36
     undo_tbOrder AT ROW 1.33 COL 16 WIDGET-ID 38
     save_tbOrder AT ROW 1.33 COL 20.6 WIDGET-ID 40
     delete_tbOrder AT ROW 1.33 COL 25.2 WIDGET-ID 42
     refresh_tbOrder AT ROW 1.33 COL 29.8 WIDGET-ID 44
     filter_tbOrder AT ROW 1.33 COL 34.4 WIDGET-ID 46
     browseconfig_tbOrder AT ROW 1.33 COL 39 WIDGET-ID 48
     excel_tbOrder AT ROW 1.33 COL 43.6 WIDGET-ID 50
     BrwOrder AT ROW 2.67 COL 2 WIDGET-ID 200
     Ordernum AT ROW 11 COL 17 COLON-ALIGNED HELP
          "Please enter an order number."
     OrderStatus AT ROW 11 COL 81 COLON-ALIGNED HELP
          "Please enter the Order Status." WIDGET-ID 26
     OrderDate AT ROW 12.1 COL 17 COLON-ALIGNED HELP
          "Please enter the date of order."
     PromiseDate AT ROW 12.1 COL 45 COLON-ALIGNED HELP
          "Please enter the Promise Date."
     CustNum AT ROW 13.19 COL 17 COLON-ALIGNED HELP
          "Please enter an existing customer number."
     btnCustNum AT ROW 13.19 COL 28.6 WIDGET-ID 22 NO-TAB-STOP 
     Name AT ROW 13.24 COL 30.8 COLON-ALIGNED HELP
          "Please enter a name." NO-LABEL
     SalesRep AT ROW 14.19 COL 17 COLON-ALIGNED HELP
          "Please enter the Sales Rep."
     btnSalesRep AT ROW 14.19 COL 28.6 WIDGET-ID 24 NO-TAB-STOP 
     Balance AT ROW 14.19 COL 73.8 COLON-ALIGNED HELP
          "Please enter a balance."
     RepName AT ROW 14.24 COL 30.8 COLON-ALIGNED HELP
          "Please enter the Name of the Salesperson." NO-LABEL
     Instructions AT ROW 15.43 COL 19 HELP
          "Please enter Instructions" NO-LABEL WIDGET-ID 28
     tbOrder AT ROW 1.24 COL 2 WIDGET-ID 30
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 111 BY 18.67 WIDGET-ID 100.


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
         HEIGHT             = 18.67
         WIDTH              = 111
         MAX-HEIGHT         = 23.52
         MAX-WIDTH          = 114.2
         VIRTUAL-HEIGHT     = 23.52
         VIRTUAL-WIDTH      = 114.2
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
/* BROWSE-TAB BrwOrder excel_tbOrder DEFAULT-FRAME */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 18.67
       FRAME DEFAULT-FRAME:WIDTH            = 111.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BrwOrder
/* Query rebuild information for BROWSE BrwOrder
     _TblList          = "sports113.Order"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > "_<CALC>"
"Order.Ordernum" "Order Num" "zzzzzzzzz9" "integer" ? ? ? ? ? ? yes "Please enter an order number." no no "10.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > "_<CALC>"
"Order.OrderDate" "Ordered" "99/99/99" "date" ? ? ? ? ? ? no "Please enter the date of order." no no "9.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > "_<CALC>"
"Order.PromiseDate" "Promised" "99/99/99" "date" ? ? ? ? ? ? no "Please enter the Promise Date." no no "9.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > "_<CALC>"
"Order.OrderStatus" "Order Status" "x(20)" "character" ? ? ? ? ? ? no "Please enter the Order Status." no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > "_<CALC>"
"Order.OrderTotal" "Total" "->>,>>>,>>9.99" "DECIMAL" ? ? ? ? ? ? yes "" no no "14.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > "_<CALC>"
"Order.Balance" "Balance" "->,>>>,>>9.99" "decimal" ? ? ? ? ? ? no "Please enter a balance." no no "13.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > "_<CALC>"
"Order.CustNum" "Cust Num" ">>>>9" "integer" ? ? ? ? ? ? no "Please enter an existing customer number." no no "9.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > "_<CALC>"
"Order.SalesRep" "Sales Rep" "x(4)" "character" ? ? ? ? ? ? no "Please enter the Sales Rep." no no "9.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > "_<CALC>"
"Order.Instructions" "Instructions" "x(50)" "character" ? ? ? ? ? ? no "Please enter Instructions" no no "50" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > "_<CALC>"
"Order.RepName" "Rep Name" "x(30)" "character" ? ? ? ? ? ? no "Please enter the Name of the Salesperson." no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > "_<CALC>"
"Order.Name" "Name" "x(30)" "character" ? ? ? ? ? ? no "Please enter a name." no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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


&Scoped-define SELF-NAME btnCustNum
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCustNum C-Win
ON CHOOSE OF btnCustNum IN FRAME DEFAULT-FRAME /* ... */
DO:
  DEF VAR cReturnValues   AS CHAR NO-UNDO.
  DEF VAR bOk             AS LOG  NO-UNDO.

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxLookup.w (THIS-PROCEDURE,200,
                    "Customer"
                    + ";CustNum"
                    + ";Name"
                    + ";Balance"
                   ,"WHERE true"
                    ,""                                                  
                    ,"CustNum,Name,Balance",   /* <- return values for these fields */
                    OUTPUT cReturnValues,
                    OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk AND cReturnValues NE "" THEN DO:
    ASSIGN CustNum:SCREEN-VALUE = ENTRY(1,cReturnValues,"|")
           Name:SCREEN-VALUE = ENTRY(2,cReturnValues,"|")
           Balance:SCREEN-VALUE = ENTRY(3,cReturnValues,"|")
           .

    APPLY "any-printable" TO CustNum.
  END.
  THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSalesRep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSalesRep C-Win
ON CHOOSE OF btnSalesRep IN FRAME DEFAULT-FRAME /* ... */
DO:
  DEF VAR cReturnValues   AS CHAR NO-UNDO.
  DEF VAR bOk             AS LOG  NO-UNDO.

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxLookup.w (THIS-PROCEDURE,200,
                    "Salesrep"
                    + ";SalesRep"
                    + ";RepName"
                   ,"WHERE true"
                    ,""                                                  
                    ,"SalesRep,RepName",   /* <- return values for these fields */
                    OUTPUT cReturnValues,
                    OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk AND cReturnValues NE "" THEN DO:
    ASSIGN SalesRep:SCREEN-VALUE = ENTRY(1,cReturnValues,"|")
           RepName:SCREEN-VALUE = ENTRY(2,cReturnValues,"|")
           .

    APPLY "any-printable" TO SalesRep.
  END.
  THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
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
  DISPLAY Ordernum OrderStatus OrderDate PromiseDate CustNum Name SalesRep 
          Balance RepName Instructions 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE tbOrder new_tbOrder edit_tbOrder copy_tbOrder undo_tbOrder 
         save_tbOrder delete_tbOrder refresh_tbOrder filter_tbOrder 
         browseconfig_tbOrder excel_tbOrder BrwOrder Ordernum OrderStatus 
         OrderDate PromiseDate CustNum btnCustNum Name SalesRep btnSalesRep 
         Balance RepName Instructions 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExtraDeleteRecord C-Win 
PROCEDURE ExtraDeleteRecord :
DEF OUTPUT PARAM obOk     AS LOG  NO-UNDO.
MESSAGE "are you really sure??" 
VIEW-AS ALERT-BOX buttons ok-cancel update obOk.
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

  oPromiseDate_JBoxDevExEdit = NEW JBoxDevExDateEdit(THIS-PROCEDURE,PromiseDate:HANDLE).
  oPromiseDate_JBoxDevExEdit:RegisterWithJukeBox(YES). /* YES: Visible */
  oPromiseDate_JBoxDevExEdit:CreateDisplayLink(oFmOrder:BUFFER-HANDLE,'PromiseDate').

  oInstructions_JBoxDevExEdit = NEW JBoxDevExEdit(THIS-PROCEDURE,Instructions:HANDLE).
  oInstructions_JBoxDevExEdit:RegisterWithJukeBox(YES). /* YES: Visible */
  oInstructions_JBoxDevExEdit:CreateDisplayLink(oFmOrder:BUFFER-HANDLE,'Instructions').
  /* Blob context and format: */
  oInstructions_JBoxDevExEdit:cDocLoadContext = 'Order.Instructions'.
  oInstructions_JBoxDevExEdit:cDocLoadIdFields = 'OrderNum'. /* <-NB */
  oInstructions_JBoxDevExEdit:cDocLoadFormat = 'html'.
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

DYNAMIC-FUNCTION("CreateStatusBar",THIS-PROCEDURE:CURRENT-WINDOW,"",0,YES,?).

DO WITH FRAME {&FRAME-NAME}:

  oBrwOrder = NEW JBoxBrowse(brwOrder:HANDLE).

  oFmOrder = NEW JBoxFieldMap(FRAME {&FRAME-NAME}:HANDLE).
  oFmOrder:updateFields = 'Ordernum,OrderDate,PromiseDate,OrderStatus,CustNum,SalesRep,Instructions'.
  oFmOrder:displayFields = 'RepName,Balance,Name'.
  oFmOrder:primaryKeyFields = 'Ordernum'.

  oFmOrder:BROWSE-OBJECT = oBrwOrder.
  RUN InitializeComponents.
  oTbOrder = NEW JBoxToolbar(tbOrder:HANDLE,'File').

  oBrwOrder:TOOLBAR-OBJECT = oTbOrder.
  oFmOrder:TOOLBAR-OBJECT = oTbOrder.
END.
oBrwOrder:OpenQuery().

DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,
                                                                                "instructions").
DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,500,200,0,0).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LeaveOfFieldHandle C-Win 
PROCEDURE LeaveOfFieldHandle :
/*------------------------------------------------------------------------------
  Purpose:     Retrieve lookup values when a foreign key field is modified
  Parameters:  Handle to foreign key field
  Notes:       The cReturn variable should be replaced with a fill-in
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihField AS HANDLE NO-UNDO.

DEF VAR cReturn AS CHAR NO-UNDO.

IF ihField:MODIFIED THEN DO WITH FRAME {&FRAME-NAME}:
  CASE ihField:NAME:
    WHEN "CustNum" THEN DO:
     cReturn = DYNAMIC-FUNCTION("getFieldValues","Customer","WHERE CustNum = '" + ihField:SCREEN-VALUE + "'","Name,Balance").
     ASSIGN Name:SCREEN-VALUE = ENTRY(1,cReturn,"|")
            Balance:SCREEN-VALUE = ENTRY(2,cReturn,"|"). 
    END.  
    WHEN "SalesRep" THEN RepName:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","Salesrep","WHERE SalesRep = '" + ihField:SCREEN-VALUE + "'","Repname").
  END CASE.

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
THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = NO.
&IF DEFINED(UIB_is_Running) NE 0 &THEN
  RUN ShowForm("").
&ENDIF
THIS-PROCEDURE:CURRENT-WINDOW:WINDOW-STATE = 3.
THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
DYNAMIC-FUNCTION("DoLockWindow",?).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

