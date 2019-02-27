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
/*&SCOPED-DEFINE AdvGuiWin*/

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEF VAR bOk          AS LOG    NO-UNDO.
DEF VAR ix           AS INT    NO-UNDO.
DEF VAR hBrwSalesRep AS HANDLE NO-UNDO.
DEF VAR hBrowse      AS HANDLE NO-UNDO.
DEF VAR hQuery       AS HANDLE NO-UNDO.
DEF VAR hToolbar     AS HANDLE NO-UNDO.
DEF VAR hFieldMap    AS HANDLE NO-UNDO.

DEF TEMP-TABLE Customer
    FIELD CustNum AS integer
    FIELD Name AS character
    FIELD Phone AS character
    FIELD SalesRep AS character
    FIELD RepName AS character
    FIELD CreditLimit AS decimal
    FIELD Discount AS integer
    FIELD Ordernum AS integer
    FIELD OrderStatus AS character
    FIELD OrderDate AS date
    FIELD SalesRep3 AS character
    FIELD Linenum AS integer
    FIELD Itemnum AS integer
    FIELD Price AS decimal
    FIELD Qty AS integer
    FIELD RowCount AS INTEGER
    FIELD jbCountDistinct AS INTEGER FORMAT ">>>,>>>,>>9" INIT 1
    FIELD jbAverage AS DECIMAL FORMAT "->>>,>>>,>>9.99"
    FIELD RowIdent1 AS CHAR
    FIELD RowIdent2 AS CHAR
    FIELD RowIdent3 AS CHAR
    FIELD RowIdent4 AS CHAR
    INDEX idxRowids RowIdent1 RowIdent2 RowIdent3 RowIdent4
    .
FUNCTION getTableFieldListCustomer RETURNS CHARACTER().
  RETURN 'Customer;CustNum;Name;Phone;SalesRep;CreditLimit;Discount,Salesrep;RepName,Order;Ordernum;OrderStatus;OrderDate;SalesRep,OrderLine;Linenum;Itemnum;Price;Qty'.
END FUNCTION.
FUNCTION getQueryJoinCustomer RETURNS CHARACTER().
  RETURN 'EACH Salesrep OF Customer NO-LOCK,EACH Order OF Customer NO-LOCK,EACH OrderLine OF Order NO-LOCK'.
END FUNCTION.

DEF TEMP-TABLE Salesrep
    FIELD SalesRep AS character
    FIELD RepName AS character
    FIELD MonthQuota_1 AS INTEGER
    FIELD MonthQuota_2 AS INTEGER
  FIELD RowCount AS INTEGER
  FIELD jbCountDistinct AS INTEGER FORMAT ">>>,>>>,>>9" INIT 1
  FIELD jbAverage AS DECIMAL FORMAT "->>>,>>>,>>9.99"
  FIELD jbextent_1_MonthQuota AS INT
  FIELD jbextent_2_MonthQuota AS INT
  FIELD RowIdent1 AS CHAR
  INDEX idxRowids RowIdent1
    .
FUNCTION getTableFieldListSalesrep RETURNS CHARACTER().
  RETURN 'Salesrep;SalesRep;RepName;MonthQuota[1];MonthQuota[2]'.
END FUNCTION.
FUNCTION getQueryJoinSalesrep RETURNS CHARACTER().
  RETURN ''.
END FUNCTION.

/*
Salesrep.MonthQuota[1] FORMAT "->,>>>,>>9":U
Salesrep.MonthQuota[2] FORMAT "->,>>>,>>9":U
*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME brwCustOrder

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Customer Salesrep

/* Definitions for BROWSE brwCustOrder                                  */
&Scoped-define FIELDS-IN-QUERY-brwCustOrder Customer.CustNum Customer.Name Customer.Phone Customer.SalesRep Customer.RepName Customer.CreditLimit Customer.Discount Customer.Ordernum Customer.OrderStatus Customer.OrderDate Customer.SalesRep3 Customer.Linenum Customer.Itemnum Customer.Price Customer.Qty Customer.jbCountDistinct   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brwCustOrder Customer.CustNum   
&Scoped-define ENABLED-TABLES-IN-QUERY-brwCustOrder Customer
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-brwCustOrder Customer
&Scoped-define SELF-NAME brwCustOrder
&Scoped-define QUERY-STRING-brwCustOrder FOR EACH Customer NO-LOCK
&Scoped-define OPEN-QUERY-brwCustOrder OPEN QUERY {&SELF-NAME} FOR EACH Customer NO-LOCK.
&Scoped-define TABLES-IN-QUERY-brwCustOrder Customer
&Scoped-define FIRST-TABLE-IN-QUERY-brwCustOrder Customer


/* Definitions for BROWSE brwSalesRep                                   */
&Scoped-define FIELDS-IN-QUERY-brwSalesRep Salesrep.SalesRep Salesrep.RepName MonthQuota_1 MonthQuota_2   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brwSalesRep Salesrep.SalesRep   
&Scoped-define ENABLED-TABLES-IN-QUERY-brwSalesRep Salesrep
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-brwSalesRep Salesrep
&Scoped-define SELF-NAME brwSalesRep
&Scoped-define QUERY-STRING-brwSalesRep FOR EACH Salesrep NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-brwSalesRep OPEN QUERY {&SELF-NAME} FOR EACH Salesrep NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-brwSalesRep Salesrep
&Scoped-define FIRST-TABLE-IN-QUERY-brwSalesRep Salesrep


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-brwCustOrder}~
    ~{&OPEN-QUERY-brwSalesRep}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tbCustomer brwSalesRep brwCustOrder 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE RECTANGLE tbCustomer
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9.8 BY .91.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brwCustOrder FOR 
      Customer SCROLLING.

DEFINE QUERY brwSalesRep FOR 
      Salesrep SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brwCustOrder
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brwCustOrder C-Win _FREEFORM
  QUERY brwCustOrder NO-LOCK DISPLAY
      Customer.CustNum COLUMN-LABEL "Kundenr" FORMAT ">>>>9":U
Customer.Name FORMAT "x(30)":U
Customer.Phone FORMAT "x(20)":U
Customer.SalesRep FORMAT "x(4)":U
Customer.RepName FORMAT "x(30)":U
Customer.CreditLimit FORMAT "->,>>>,>>9":U
Customer.Discount FORMAT ">>9%":U
Customer.Ordernum FORMAT "zzzzzzzzz9":U
Customer.OrderStatus FORMAT "x(20)":U
Customer.OrderDate FORMAT "99/99/99":U
Customer.SalesRep3 FORMAT "x(4)":U
Customer.Linenum FORMAT ">>9":U
Customer.Itemnum FORMAT "zzzzzzzzz9":U
Customer.Price FORMAT "->,>>>,>>9.99":U
Customer.Qty COLUMN-LABEL "Kvantum" FORMAT "->>>>9":U
Customer.jbCountDistinct
ENABLE Customer.CustNum
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 122 BY 9.52
         TITLE "Customers and orders" FIT-LAST-COLUMN.

DEFINE BROWSE brwSalesRep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brwSalesRep C-Win _FREEFORM
  QUERY brwSalesRep NO-LOCK DISPLAY
      Salesrep.SalesRep FORMAT "x(4)":U
Salesrep.RepName FORMAT "x(30)":U
MonthQuota_1 FORMAT "->,>>>,>>9":U LABEL "MonthQuota[1]"
MonthQuota_2 FORMAT "->,>>>,>>9":U LABEL "MonthQuota[2]"
ENABLE Salesrep.SalesRep
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 122 BY 5.24
         TITLE "Salesreps" FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     brwSalesRep AT ROW 2.19 COL 4 WIDGET-ID 300
     brwCustOrder AT ROW 9.33 COL 4 WIDGET-ID 200
     tbCustomer AT ROW 8.14 COL 4 WIDGET-ID 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 127.4 BY 18.38 WIDGET-ID 100.


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
         HEIGHT             = 18.43
         WIDTH              = 127.4
         MAX-HEIGHT         = 27.14
         MAX-WIDTH          = 127.4
         VIRTUAL-HEIGHT     = 27.14
         VIRTUAL-WIDTH      = 127.4
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
/* BROWSE-TAB brwSalesRep tbCustomer DEFAULT-FRAME */
/* BROWSE-TAB brwCustOrder brwSalesRep DEFAULT-FRAME */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brwCustOrder
/* Query rebuild information for BROWSE brwCustOrder
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Customer NO-LOCK
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE brwCustOrder */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brwSalesRep
/* Query rebuild information for BROWSE brwSalesRep
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Salesrep NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE brwSalesRep */
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


&Scoped-define BROWSE-NAME brwCustOrder
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
  ENABLE tbCustomer brwSalesRep brwCustOrder 
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

ETIME(YES).

DO WITH FRAME {&FRAME-NAME}:
  hBrwSalesRep = DYNAMIC-FUNCTION("NewBrowse"
            ,brwSalesrep:HANDLE
            ,100
            ,"multiple"
            ,getTableFieldListSalesrep()
            ,"WHERE false," + getQueryJoinSalesrep()
            ,"").

/*   DYNAMIC-FUNCTION("setAttribute",hBrwSalesRep,"baseQuery","where salesrep = 'hxm'").  */
  DYNAMIC-FUNCTION("setAttribute",hBrwSalesRep,"sortMap","MonthQuota_1;monthquota[1],MonthQuota_2;monthquota[2]").

  DYNAMIC-FUNCTION("newmenuband",hbrwSalesrep,"multisortbrowse;sort on multiple","").


  hBrowse = DYNAMIC-FUNCTION("NewBrowse"
            ,brwCustOrder:HANDLE
            ,100
            ,"multiple"
            ,getTableFieldListCustomer()
            ,"WHERE false," + getQueryJoinCustomer()
            ,"").

  DYNAMIC-FUNCTION("createParentLink",hBrowse,hBrwSalesRep,"salesRep").
END.

DYNAMIC-FUNCTION("setAttribute",hBrowse,"windowsBrowse","yes").



hToolbar = DYNAMIC-FUNCTION("NewToolBar"
          ,tbCustomer:HANDLE
          ,"File"
          ,"new,copy,undo,delete,save,excel"
         + ",BrowseConfig,Filter,Accum"
          ,"maxborder").

DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hToolbar).

DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:HANDLE,"brwSalesrep").

DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,200,200,0,0).

RUN InvokeMethod(hBrwSalesrep,"OpenQuery").

/* DYNAMIC-FUNCTION("setAttribute",hBrwSalesRep,"useLocalData","yes").  */

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

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

