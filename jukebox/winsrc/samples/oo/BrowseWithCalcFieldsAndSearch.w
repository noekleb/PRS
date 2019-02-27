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

&SCOPED-DEFINE UseAdvGui

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEF VAR bOk         AS LOG    NO-UNDO.
DEF VAR ix          AS INT    NO-UNDO.
DEF VAR hBrowse     AS HANDLE NO-UNDO.
DEF VAR hQuery      AS HANDLE NO-UNDO.
DEF VAR hToolbar    AS HANDLE NO-UNDO.
DEF VAR hFieldMap   AS HANDLE NO-UNDO.

DEF VAR oFmCustomer AS JBoxFieldMap NO-UNDO.
DEF VAR oTbCustomer AS JBoxToolbar NO-UNDO.
/*** Start instance property definitions for JBoxBrowse object oBrwCustomer ***/
DEF VAR oBrwCustomer AS JBoxBrowse NO-UNDO.

DEF TEMP-TABLE Customer
    FIELD CustNum AS integer
    FIELD Name AS character
    FIELD Phone AS character
    FIELD SalesRep AS character
    FIELD RepName AS character
    field OrderTotal as decimal format "->>>,>>9.99" label "Current order total"
    field OrdersBefore1998 as logical label "Orders before 1998"
    FIELD Ordernum AS integer
    FIELD OrderDate AS date
    field LastOrderValue as decimal format "->>,>>9.99" label "Value last order"
    field LastOrderLineCnt as integer label "Linecount last order"
    field LastOrderWeek as char format "x(6)" label "Weeknum last order"
    FIELD MonthQuota_1 AS DECIMAL 
    FIELD MonthQuota_2 AS DECIMAL 
    FIELD RowIdent1 AS CHARACTER 
    FIELD RowIdent2 AS CHARACTER 
    FIELD RowIdent3 AS CHARACTER 
    FIELD RowCount AS INTEGER
    FIELD jbCountDistinct AS INTEGER FORMAT '>>>,>>>,>>9' INIT 1
    FIELD jbAverage AS DECIMAL FORMAT '->>>,>>>,>>9.99'
    INDEX idxRowids  RowIdent1 RowIdent2 RowIdent3
    .

FUNCTION getBuffersAndFieldsBrwCustomer RETURNS CHARACTER().
  RETURN 
      'Customer'
      + ';CustNum'
      + ';Name'
      + ';Phone'
      + ';SalesRep'
    + ',Order'
      + ';Ordernum'
      + ';OrderDate'
    + ',Salesrep'
      + ';RepName'
      + ';MonthQuota[1]'
      + ';MonthQuota[2]'
      .
END FUNCTION.
FUNCTION getQueryJoinBrwCustomer RETURNS CHARACTER().
  RETURN 'LAST Order OF Customer NO-LOCK,EACH Salesrep OF Customer NO-LOCK'.
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
&Scoped-define FIELDS-IN-QUERY-brwCustomer Customer.CustNum Customer.Name Customer.Phone Customer.SalesRep Customer.RepName Customer.MonthQuota_1 Customer.MonthQuota_2 Customer.OrderTotal Customer.OrdersBefore1998 Customer.Ordernum Customer.OrderDate Customer.LastOrderWeek Customer.LastOrderValue   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brwCustomer Customer.CustNum   
&Scoped-define ENABLED-TABLES-IN-QUERY-brwCustomer Customer
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-brwCustomer Customer
&Scoped-define SELF-NAME brwCustomer
&Scoped-define QUERY-STRING-brwCustomer FOR EACH Customer NO-LOCK
&Scoped-define OPEN-QUERY-brwCustomer OPEN QUERY {&SELF-NAME} FOR EACH Customer NO-LOCK.
&Scoped-define TABLES-IN-QUERY-brwCustomer Customer
&Scoped-define FIRST-TABLE-IN-QUERY-brwCustomer Customer


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-brwCustomer}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tbCustomer searchCustomer brwCustomer ~
CustNum Name Phone SalesRep btnSalesRep RepName MonthQuota_1 MonthQuota_2 
&Scoped-Define DISPLAYED-OBJECTS CustNum Name Phone SalesRep RepName ~
MonthQuota_1 MonthQuota_2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnSalesRep 
     LABEL "..." 
     SIZE 4 BY 1.

DEFINE VARIABLE CustNum AS INTEGER FORMAT ">>>>9" INITIAL 0 
     LABEL "Cust Num" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 TOOLTIP "Please enter a customer number.".

DEFINE VARIABLE MonthQuota_1 AS INTEGER FORMAT "->,>>>,>>9" INITIAL 0 
     LABEL "Month Quota" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 TOOLTIP "Please enter the Month Quota.".

DEFINE VARIABLE MonthQuota_2 AS INTEGER FORMAT "->,>>>,>>9" INITIAL 0 
     LABEL "Month Quota" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 TOOLTIP "Please enter the Month Quota.".

DEFINE VARIABLE Name AS CHARACTER FORMAT "x(30)" 
     LABEL "Name" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1 TOOLTIP "Please enter a name.".

DEFINE VARIABLE Phone AS CHARACTER FORMAT "x(20)" 
     LABEL "Phone" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 TOOLTIP "Please enter a phone number".

DEFINE VARIABLE RepName AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1 TOOLTIP "Please enter the Name of the Salesperson.".

DEFINE VARIABLE SalesRep AS CHARACTER FORMAT "x(4)" 
     LABEL "Sales Rep" 
     VIEW-AS FILL-IN 
     SIZE 9.6 BY 1 TOOLTIP "Please Enter a Sales Rep.".

DEFINE RECTANGLE searchCustomer
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 16.4 BY .91.

DEFINE RECTANGLE tbCustomer
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9.8 BY .91.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brwCustomer FOR 
      Customer SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brwCustomer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brwCustomer C-Win _FREEFORM
  QUERY brwCustomer NO-LOCK DISPLAY
      Customer.CustNum FORMAT ">>>>9":U
Customer.Name FORMAT "x(30)":U
Customer.Phone FORMAT "x(20)":U
Customer.SalesRep FORMAT "x(4)":U
Customer.RepName FORMAT "x(30)":U
Customer.MonthQuota_1 FORMAT ">>>,>>>,>>9.99" 
Customer.MonthQuota_2 FORMAT ">>>,>>>,>>9.99"
Customer.OrderTotal
Customer.OrdersBefore1998
Customer.Ordernum FORMAT "zzzzzzzzz9":U
Customer.OrderDate FORMAT "99/99/99":U
Customer.LastOrderWeek
Customer.LastOrderValue
ENABLE Customer.CustNum
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 86 BY 9.05 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     brwCustomer AT ROW 2.67 COL 2 WIDGET-ID 200
     CustNum AT ROW 11.95 COL 15 COLON-ALIGNED
     Name AT ROW 12.95 COL 15 COLON-ALIGNED
     Phone AT ROW 13.95 COL 15 COLON-ALIGNED
     SalesRep AT ROW 14.95 COL 15 COLON-ALIGNED
     btnSalesRep AT ROW 14.95 COL 26.6 WIDGET-ID 4 NO-TAB-STOP 
     RepName AT ROW 14.95 COL 28.8 COLON-ALIGNED NO-LABEL
     MonthQuota_1 AT ROW 16.24 COL 15 COLON-ALIGNED
     MonthQuota_2 AT ROW 17.24 COL 15 COLON-ALIGNED
     tbCustomer AT ROW 1.48 COL 19.4 WIDGET-ID 2
     searchCustomer AT ROW 1.43 COL 2.6 WIDGET-ID 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 87.8 BY 17.43 WIDGET-ID 100.


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
         HEIGHT             = 17.38
         WIDTH              = 87.8
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
/* BROWSE-TAB brwCustomer searchCustomer DEFAULT-FRAME */
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
     _TblOptList       = ", LAST,"
     _Query            is OPENED
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
                    + ";repname"
                   ,"WHERE true"
                    ,""                                                  
                    ,"SalesRep,Repname",   /* <- return values for these fields */
                    OUTPUT cReturnValues,
                    OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk AND cReturnValues NE "" THEN DO:
    ASSIGN SalesRep:SCREEN-VALUE = ENTRY(1,cReturnValues,"|")
           repname:screen-value = ENTRY(2,cReturnValues,"|") 
           .

    APPLY "any-printable" TO SalesRep.
  END.
  THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
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

  IF PROGRAM-NAME(2) BEGINS "adecomm" THEN do:
    RUN InitializeObject.
    RUN MoveToTop.
  END.
  
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BeforeSetDynFilter C-Win 
PROCEDURE BeforeSetDynFilter :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihQuery AS HANDLE NO-UNDO.
MESSAGE 
VIEW-AS ALERT-BOX.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplayRecord C-Win 
PROCEDURE DisplayRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

RUN super.

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
  DISPLAY CustNum Name Phone SalesRep RepName MonthQuota_1 MonthQuota_2 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE tbCustomer searchCustomer brwCustomer CustNum Name Phone SalesRep 
         btnSalesRep RepName MonthQuota_1 MonthQuota_2 
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
  /* Built-in calculations: */
  oBrwCustomer:calcCan-Find(""                      /* Source buffer - assumes 1st buffer when blank. Parmeter can be omitted */
                           ,"OrdersBefore1998"      /* Target field in temp-table */
                           ,"FIRST Order OF Customer where orderdate < 01/01/1998").  /* Expression */

  oBrwCustomer:calcTotal("Order"                    /* Source buffer cannot be ommitted here since Order isn't the primary buffer */
                        ,"LastOrderValue"           /* Target field in temp-table */
                        ,"EACH OrderLine OF Order"  /* Query */
                        ,"Price * Qty").            /* Expression */
  oBrwCustomer:calcTotal("Order"                    /* Source buffer cannot be ommitted here since Order isn't the primary buffer */
                        ,"LastOrderLineCnt"         /* Target field in temp-table */
                        ,"EACH OrderLine WHERE OrderLine.OrderNum = Order.OrderNum"  /* Query */
                        ,"COUNT").                  /* Expression */
                        
  oBrwCustomer:calcWeek("Order"           /* Source buffer cannot be ommitted here since Order isn't the primary buffer */
                       ,"LastOrderWeek"   /* Target field in temp-table */
                       ,"OrderDate").     /* Field in source buffer (parameter) */
  /* Corresponding methods for date conversion are calcMonth, calcQuarter and calcYear */                      
                       
  /* Custom calculations, ie procedures in customer_browsecalc.p: */                     
  oBrwCustomer:calcFieldProc = "customer_browsecalc.p".
  oBrwCustomer:addCalcField("Customer"       /* Source buffer - can be blank or ommitted. If so, assumes 1st buffer in query */
                           ,"OrderTotal"     /* Target field in temp table */
                           ,"CustOrderTotal" /* Name of internal procedure in customer_browsecalc.p */
                           ,"custNum").      /* Parameter - can be field name in source buffer, blank, ommitted or constant (string). 
                                                Constant may be changed at runtime by setting the calcParam property. 
                                                See documentation for signature changes of server procedures */

  oBrwCustomer:setSearchField(searchCustomer:HANDLE). /* When no column num specified 1st column is assumed
                                                         Sorting is also set to this column (if not already specified) */
  
  oFmCustomer = NEW JBoxFieldMap(FRAME {&FRAME-NAME}:HANDLE). /* <- curly brackets and & missing */
  oFmCustomer:updateFields = 'CustNum,Name,Phone,SalesRep'.
  oFmCustomer:displayFields = 'RepName,MonthQuota_1,MonthQuota_2'.
  oFmCustomer:primaryKeyFields = 'CustNum'.
  oFmCustomer:BROWSE-OBJECT = oBrwCustomer.
    
  oTbCustomer = NEW JBoxToolbar(tbCustomer:HANDLE,'File').
  oTbCustomer:AddToolGroup('New,Edit,undo,Delete,Save,Filter,Excel').

  oBrwCustomer:TOOLBAR-OBJECT = oTbCustomer.
  oFmCustomer:TOOLBAR-OBJECT = oTbCustomer.
    
  oBrwCustomer:openQuery().
  
END.


DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,400,200,0,0).

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
    WHEN "SalesRep" THEN repname:SCREEN-VALUE 
                    = DYNAMIC-FUNCTION("getFieldValues","Salesrep","WHERE SalesRep = '" + ihField:SCREEN-VALUE + "'","Repname").
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
THIS-PROCEDURE:CURRENT-WINDOW:WINDOW-STATE = 3.
THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
DYNAMIC-FUNCTION("DoLockWindow",?).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

