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

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEF VAR bOk         AS LOG    NO-UNDO.
DEF VAR ix          AS INT    NO-UNDO.
DEF VAR hBrowse     AS HANDLE NO-UNDO.
DEF VAR hQuery      AS HANDLE NO-UNDO.
DEF VAR hToolbar    AS HANDLE NO-UNDO.
DEF VAR hFieldMap   AS HANDLE NO-UNDO.

DEF VAR oQryCustomer AS JBoxQuery NO-UNDO.
DEF VAR oTbCustomer  AS JBoxToolbar NO-UNDO.
DEF VAR oFmCustomer  AS JBoxFieldMap NO-UNDO.

DEF TEMP-TABLE Customer
    FIELD Address AS character
    FIELD Address2 AS character
    FIELD Balance AS decimal
    FIELD City AS character
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
    FIELD SalesRep2 AS character
    FIELD RowIdent1 AS CHARACTER 
    FIELD RowIdent2 AS CHARACTER 
    FIELD RowCount AS INTEGER
    INDEX idxRowids  RowIdent1 RowIdent2
    .

FUNCTION getTableHandleQryCustomer RETURNS HANDLE().
  RETURN BUFFER Customer:HANDLE:TABLE-HANDLE.
END FUNCTION.
FUNCTION getBuffersAndFieldsQryCustomer RETURNS CHARACTER().
  RETURN 
      'Customer'
    + ',Salesrep'
      .
END FUNCTION.
/*
FUNCTION getBuffersAndFieldsQryCustomer RETURNS CHARACTER().
  RETURN 
      'Customer'
      + ';Address'
      + ';Address2'
      + ';Balance'
      + ';City'
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
    + ',Salesrep'
      + ';MonthQuota[1]'
      + ';MonthQuota[2]'
      + ';MonthQuota[3]'
      + ';MonthQuota[4]'
      + ';MonthQuota[5]'
      + ';MonthQuota[6]'
      + ';MonthQuota[7]'
      + ';MonthQuota[8]'
      + ';MonthQuota[9]'
      + ';MonthQuota[10]'
      + ';MonthQuota[11]'
      + ';MonthQuota[12]'
      + ';Region'
      + ';RepName'
      + ';SalesRep'.
END FUNCTION.
*/
FUNCTION getQueryJoinQryCustomer RETURNS CHARACTER().
  RETURN 'EACH Salesrep OF Customer NO-LOCK'.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define QUERY-NAME QUERY-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Customer

/* Definitions for QUERY QUERY-2                                        */
&Scoped-define SELF-NAME QUERY-2
&Scoped-define QUERY-STRING-QUERY-2 FOR EACH Customer NO-LOCK
&Scoped-define OPEN-QUERY-QUERY-2 OPEN QUERY {&SELF-NAME} FOR EACH Customer NO-LOCK.
&Scoped-define TABLES-IN-QUERY-QUERY-2 Customer
&Scoped-define FIRST-TABLE-IN-QUERY-QUERY-2 Customer


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tbCustomer CustNum Name Phone SalesRep ~
btnSalesRep RepName MonthQuota_1 MonthQuota_2 MonthQuota_3 
&Scoped-Define DISPLAYED-OBJECTS CustNum Name Phone SalesRep RepName ~
MonthQuota_1 MonthQuota_2 MonthQuota_3 

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
     LABEL "Month Quota(jan)" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 TOOLTIP "Please enter the Month Quota.".

DEFINE VARIABLE MonthQuota_3 AS INTEGER FORMAT "->,>>>,>>9" INITIAL 0 
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

DEFINE RECTANGLE tbCustomer
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9.8 BY .91.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY QUERY-2 FOR 
      Customer SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     CustNum AT ROW 4.33 COL 12 COLON-ALIGNED WIDGET-ID 2
     Name AT ROW 5.33 COL 12 COLON-ALIGNED WIDGET-ID 10
     Phone AT ROW 6.33 COL 12 COLON-ALIGNED WIDGET-ID 12
     SalesRep AT ROW 8.33 COL 12 COLON-ALIGNED WIDGET-ID 18
     btnSalesRep AT ROW 8.33 COL 23.6 WIDGET-ID 22 NO-TAB-STOP 
     RepName AT ROW 8.38 COL 25.8 COLON-ALIGNED NO-LABEL WIDGET-ID 16
     MonthQuota_1 AT ROW 10.05 COL 27 COLON-ALIGNED WIDGET-ID 4
     MonthQuota_2 AT ROW 11.05 COL 27 COLON-ALIGNED WIDGET-ID 6
     MonthQuota_3 AT ROW 12 COL 27 COLON-ALIGNED WIDGET-ID 8
     tbCustomer AT ROW 1.48 COL 3 WIDGET-ID 20
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 62.8 BY 16.1 WIDGET-ID 100.


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
         HEIGHT             = 16.1
         WIDTH              = 63.4
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
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 16.1
       FRAME DEFAULT-FRAME:WIDTH            = 62.8.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK QUERY QUERY-2
/* Query rebuild information for QUERY QUERY-2
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Customer NO-LOCK
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Design-Parent    is FRAME DEFAULT-FRAME @ ( 2.91 , 48 )
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


&Scoped-define SELF-NAME QUERY-2
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplayRecord C-Win 
PROCEDURE DisplayRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN SUPER.
MESSAGE customer.monthQuota_1
  VIEW-AS ALERT-BOX INFO BUTTONS OK.
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
          MonthQuota_3 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE tbCustomer CustNum Name Phone SalesRep btnSalesRep RepName 
         MonthQuota_1 MonthQuota_2 MonthQuota_3 
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
  oQryCustomer = NEW JBoxQuery('Customer').
  oQryCustomer:viewRecordCount = YES.
  oTbCustomer = NEW JBoxToolbar(tbCustomer:HANDLE,"File").
  oTbCustomer:AddToolGroup("New,Copy,Edit,Undo,Save,Delete,Filter,Excel,-,First,Prev,Next,Last").
  oTbCustomer:QUERY-OBJECT = oQryCustomer.
  
  oFmCustomer = NEW JBoxFieldMap(FRAME {&FRAME-NAME}:HANDLE).
  oFmCustomer:updateFields = "custNum,name,phone,salesRep".
  oFmCustomer:displayFields = "repname,MonthQuota_1,MonthQuota_2,MonthQuota_3".
  oFmCustomer:primaryKeyFields = "custNum". /* custNum will be read-only on display/edit but not when "new" */
  oFmCustomer:recordAvailObjects = "btnSalesRep". /* btnSalesRep will be disabled when there is no record available */
  oFmCustomer:QUERY-OBJECT = oQryCustomer.
  oFmCustomer:TOOLBAR-OBJECT = oTbCustomer.

  oQryCustomer:OpenQuery().
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
    WHEN "SalesRep" THEN RepName:SCREEN-VALUE = 
         DYNAMIC-FUNCTION("getFieldValues","Salesrep","WHERE SalesRep = '" + ihField:SCREEN-VALUE + "'","RepName").
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

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

