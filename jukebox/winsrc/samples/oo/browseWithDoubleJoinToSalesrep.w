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
    FIELD CustNum AS integer
    FIELD Name AS character
    FIELD SalesRep AS character
    FIELD RepName AS character
    FIELD Ordernum AS integer
    FIELD OrderDate AS date
    FIELD SalesRep3 AS character
    FIELD RepName4 AS character
    FIELD RowIdent1 AS CHARACTER 
    FIELD RowIdent2 AS CHARACTER 
    FIELD RowIdent3 AS CHARACTER 
    FIELD RowIdent4 AS CHARACTER 
    FIELD RowCount AS INTEGER
    FIELD jbCountDistinct AS INTEGER FORMAT '>>>,>>>,>>9' INIT 1
    FIELD jbAverage AS DECIMAL FORMAT '->>>,>>>,>>9.99'
    INDEX idxRowids  RowIdent1 RowIdent2 RowIdent3 RowIdent4
    .
DEF BUFFER v_Customer FOR Customer.


FUNCTION getBuffersAndFieldsBrwCustomer RETURNS CHARACTER():
  RETURN
    'Customer'
     + ';CustNum'
     + ';Name'
     + ';SalesRep'
  + ',Salesrep'
     + ';RepName'
  + ',Order'
     + ';Ordernum'
     + ';OrderDate'
     + ';SalesRep'
  + ',buf1_Salesrep'
     + ';RepName'
     .
END FUNCTION.
FUNCTION getQueryJoinBrwCustomer RETURNS CHARACTER():
  RETURN 'EACH Salesrep OF Customer NO-LOCK,EACH Order OF Customer NO-LOCK,EACH buf1_Salesrep  WHERE buf1_Salesrep.SalesRep = Order.SalesRep  AND buf1_Salesrep.RepName BEGINS "harry" NO-LOCK'.
END FUNCTION.


DEF VAR oFmCustomer AS JBoxFieldMap NO-UNDO.

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
&Scoped-define FIELDS-IN-QUERY-BrwCustomer Customer.CustNum Customer.Name ~
Customer.SalesRep Customer.RepName Customer.Ordernum Customer.OrderDate ~
Customer.SalesRep3 Customer.RepName4 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BrwCustomer Customer.CustNum 
&Scoped-define ENABLED-TABLES-IN-QUERY-BrwCustomer Customer
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BrwCustomer Customer
&Scoped-define QUERY-STRING-BrwCustomer FOR EACH Customer NO-LOCK, ~
    EACH Salesrep OF Customer NO-LOCK, ~
    EACH Order OF Customer NO-LOCK, ~
    EACH buf1_Salesrep WHERE buf1_Salesrep.SalesRep = Order.SalesRep AND buf1_Salesrep.RepName BEGINS "harry" NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BrwCustomer OPEN QUERY BrwCustomer FOR EACH Customer NO-LOCK, ~
    EACH Salesrep OF Customer NO-LOCK, ~
    EACH Order OF Customer NO-LOCK, ~
    EACH buf1_Salesrep WHERE buf1_Salesrep.SalesRep = Order.SalesRep AND buf1_Salesrep.RepName BEGINS "harry" NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BrwCustomer Customer
&Scoped-define FIRST-TABLE-IN-QUERY-BrwCustomer Customer


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BrwCustomer CustNum Ordernum Name OrderDate ~
SalesRep btnSalesRep SalesRep3 RepName RepName4 
&Scoped-Define DISPLAYED-OBJECTS CustNum Ordernum Name OrderDate SalesRep ~
SalesRep3 RepName RepName4 

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

DEFINE VARIABLE RepName AS CHARACTER FORMAT "x(30)" 
     LABEL "Rep Name" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1 TOOLTIP "Please enter the Name of the Salesperson.".

DEFINE VARIABLE RepName4 AS CHARACTER FORMAT "x(30)" 
     LABEL "Rep Name" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE SalesRep AS CHARACTER FORMAT "x(4)" 
     LABEL "Sales Rep" 
     VIEW-AS FILL-IN 
     SIZE 9.6 BY 1 TOOLTIP "Please Enter a Sales Rep.".

DEFINE VARIABLE SalesRep3 AS CHARACTER FORMAT "x(4)" 
     LABEL "Sales Rep" 
     VIEW-AS FILL-IN 
     SIZE 9.6 BY 1 TOOLTIP "Please enter the Sales Rep.".

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BrwCustomer FOR 
      Customer SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BrwCustomer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BrwCustomer C-Win _STRUCTURED
  QUERY BrwCustomer NO-LOCK DISPLAY
      Customer.CustNum COLUMN-LABEL "Cust Num" FORMAT ">>>>9":U
      Customer.Name COLUMN-LABEL "Name" FORMAT "x(30)":U
      Customer.SalesRep COLUMN-LABEL "Sales Rep" FORMAT "x(4)":U
      Customer.RepName COLUMN-LABEL "Rep Name" FORMAT "x(30)":U
      Customer.Ordernum COLUMN-LABEL "Order Num" FORMAT "zzzzzzzzz9":U
            WIDTH 10.2
      Customer.OrderDate COLUMN-LABEL "Ordered" FORMAT "99/99/99":U
      Customer.SalesRep3 COLUMN-LABEL "Sales Rep" FORMAT "x(4)":U
      Customer.RepName4 COLUMN-LABEL "Rep Name" FORMAT "x(30)":U
  ENABLE
      Customer.CustNum HELP "Please enter a customer number."
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 95 BY 9.81 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BrwCustomer AT ROW 3.33 COL 2 WIDGET-ID 200
     CustNum AT ROW 13.67 COL 13.2 COLON-ALIGNED HELP
          "Please enter a customer number."
     Ordernum AT ROW 13.67 COL 59.2 COLON-ALIGNED HELP
          "Please enter an order number."
     Name AT ROW 14.67 COL 13.2 COLON-ALIGNED HELP
          "Please enter a name."
     OrderDate AT ROW 14.67 COL 59.2 COLON-ALIGNED HELP
          "Please enter the date of order."
     SalesRep AT ROW 15.67 COL 13.2 COLON-ALIGNED HELP
          "Please Enter a Sales Rep."
     btnSalesRep AT ROW 15.67 COL 24.8 WIDGET-ID 2 NO-TAB-STOP 
     SalesRep3 AT ROW 15.67 COL 59.2 COLON-ALIGNED HELP
          "Please enter the Sales Rep."
     RepName AT ROW 16.67 COL 13.2 COLON-ALIGNED HELP
          "Please enter the Name of the Salesperson."
     RepName4 AT ROW 16.67 COL 59.2 COLON-ALIGNED HELP
          "Please enter the Name of the Salesperson."
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 97.4 BY 17.19 WIDGET-ID 100.


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
         HEIGHT             = 17.19
         WIDTH              = 97.4
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
/* BROWSE-TAB BrwCustomer 1 DEFAULT-FRAME */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 17.19
       FRAME DEFAULT-FRAME:WIDTH            = 97.4.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BrwCustomer
/* Query rebuild information for BROWSE BrwCustomer
     _TblList          = "sports116.Customer"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Customer.CustNum
"Customer.CustNum" "Cust Num" ">>>>9" "integer" ? ? ? ? ? ? yes "Please enter a customer number." no no "9.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Customer.Name
"Customer.Name" "Name" "x(30)" "character" ? ? ? ? ? ? no "Please enter a name." no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Customer.SalesRep
"Customer.SalesRep" "Sales Rep" "x(4)" "character" ? ? ? ? ? ? no "Please Enter a Sales Rep." no no "9.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Customer.RepName
"Customer.RepName" "Rep Name" "x(30)" "character" ? ? ? ? ? ? no "Please enter the Name of the Salesperson." no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Customer.Ordernum
"Customer.Ordernum" "Order Num" "zzzzzzzzz9" "integer" ? ? ? ? ? ? no "Please enter an order number." no no "10.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Customer.OrderDate
"Customer.OrderDate" "Ordered" "99/99/99" "date" ? ? ? ? ? ? no "Please enter the date of order." no no "9.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Customer.SalesRep3
"Customer.SalesRep3" "Sales Rep" "x(4)" "character" ? ? ? ? ? ? no "Please enter the Sales Rep." no no "9.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > Customer.RepName4
"Customer.RepName4" "Rep Name" "x(30)" "character" ? ? ? ? ? ? no "Please enter the Name of the Salesperson." no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
                    + ";Region"
                   ,"WHERE true"
                    ,""                                                  
                    ,"SalesRep,Region",   /* <- return values for these fields */
                    OUTPUT cReturnValues,
                    OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk AND cReturnValues NE "" THEN DO:
    ASSIGN SalesRep:SCREEN-VALUE = ENTRY(1,cReturnValues,"|")
           .

    APPLY "any-printable" TO SalesRep.
  END.
  THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
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
  DISPLAY CustNum Ordernum Name OrderDate SalesRep SalesRep3 RepName RepName4 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE BrwCustomer CustNum Ordernum Name OrderDate SalesRep btnSalesRep 
         SalesRep3 RepName RepName4 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeComponents C-Win 
PROCEDURE InitializeComponents :
DO WITH FRAME {&FRAME-NAME}:


  oBrwCustomer:getRecordCount = YES. /* or NO */
  oBrwCustomer:rowsToBatch = 500. /* or NO */
END.

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

  oFmCustomer = NEW JBoxFieldMap(FRAME {&FRAME-NAME}:HANDLE).
  oFmCustomer:updateFields = 'CustNum,Name,SalesRep'.
  oFmCustomer:displayFields = 'RepName,Ordernum,OrderDate,SalesRep3,RepName4'.
  oFmCustomer:primaryKeyFields = 'CustNum'.

  oFmCustomer:BROWSE-OBJECT = oBrwCustomer.
  RUN InitializeComponents.
END.
oBrwCustomer:OpenQuery().


oContainer:initResize(1000,200).

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
    WHEN "SalesRep" THEN RepName:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","Salesrep","WHERE SalesRep = '" + ihField:SCREEN-VALUE + "'","RepName").
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
IF DYNAMIC-FUNCTION("getAttribute",THIS-PROCEDURE,"advGuiWin") = "YES" THEN
  RUN ShowForm("").
THIS-PROCEDURE:CURRENT-WINDOW:WINDOW-STATE = 3.
THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
DYNAMIC-FUNCTION("DoLockWindow",?).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

