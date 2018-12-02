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


/*** Start instance property definitions for JBoxBrowse object oBrwCustomer ***/
DEF VAR oBrwCustomer AS JBoxBrowse NO-UNDO.
DEF TEMP-TABLE Customer
    FIELD CustNum AS integer
    FIELD Name AS character
    FIELD Comments AS character
    FIELD SalesRep AS character
    FIELD RepName AS character
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
     + ';CustNum'
     + ';Name'
     + ';Comments'
     + ';SalesRep'
  + ',Salesrep'
     + ';RepName'
     .
END FUNCTION.
FUNCTION getQueryJoinBrwCustomer RETURNS CHARACTER():
  RETURN 'EACH Salesrep OF Customer NO-LOCK'.
END FUNCTION.
DEF VAR oFmCustomer AS JBoxFieldMap NO-UNDO.


DEF VAR oTbCustomer AS JBoxToolbar NO-UNDO.


DEF VAR oComments_JBoxDevExEdit AS JBoxDevExEdit NO-UNDO.

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
Customer.Comments Customer.SalesRep Customer.RepName 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BrwCustomer Customer.CustNum 
&Scoped-define QUERY-STRING-BrwCustomer FOR EACH Customer NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BrwCustomer OPEN QUERY BrwCustomer FOR EACH Customer NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BrwCustomer Customer
&Scoped-define FIRST-TABLE-IN-QUERY-BrwCustomer Customer


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tbCustomer new_tbCustomer edit_tbCustomer ~
copy_tbCustomer undo_tbCustomer save_tbCustomer delete_tbCustomer ~
refresh_tbCustomer filter_tbCustomer browseconfig_tbCustomer ~
excel_tbCustomer BrwCustomer CustNum Name Comments SalesRep btnSalesRep ~
RepName 
&Scoped-Define DISPLAYED-OBJECTS CustNum Name Comments SalesRep RepName 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON browseconfig_tbCustomer 
     IMAGE-UP FILE "bmp/table.bmp":U
     LABEL "Button 9" 
     SIZE 4.6 BY 1.1.

DEFINE BUTTON btnSalesRep 
     LABEL "..." 
     SIZE 4 BY 1.

DEFINE BUTTON copy_tbCustomer 
     IMAGE-UP FILE "bmp/copy.bmp":U
     LABEL "Button 3" 
     SIZE 4.6 BY 1.1.

DEFINE BUTTON delete_tbCustomer 
     IMAGE-UP FILE "bmp/del16e.bmp":U
     LABEL "Button 6" 
     SIZE 4.6 BY 1.1.

DEFINE BUTTON edit_tbCustomer 
     IMAGE-UP FILE "bmp/edit16e.bmp":U
     LABEL "Button 2" 
     SIZE 4.6 BY 1.1.

DEFINE BUTTON excel_tbCustomer 
     IMAGE-UP FILE "gif/afexcel.gif":U
     LABEL "Button 10" 
     SIZE 4.6 BY 1.1.

DEFINE BUTTON filter_tbCustomer 
     IMAGE-UP FILE "gif/filter.gif":U
     LABEL "Button 8" 
     SIZE 4.6 BY 1.1.

DEFINE BUTTON new_tbCustomer 
     IMAGE-UP FILE "bmp/new16e.bmp":U
     LABEL "Button 1" 
     SIZE 4.6 BY 1.1.

DEFINE BUTTON refresh_tbCustomer 
     IMAGE-UP FILE "gif/refresh.gif":U
     LABEL "Button 7" 
     SIZE 4.6 BY 1.1.

DEFINE BUTTON save_tbCustomer 
     IMAGE-UP FILE "bmp/save.bmp":U
     LABEL "Button 5" 
     SIZE 4.6 BY 1.1.

DEFINE BUTTON undo_tbCustomer 
     IMAGE-UP FILE "bmp/undo16e.bmp":U
     LABEL "Button 4" 
     SIZE 4.6 BY 1.1.

DEFINE VARIABLE Comments AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 82 BY 3.57 TOOLTIP "Please enter comments.".

DEFINE VARIABLE CustNum AS INTEGER FORMAT ">>>>9" INITIAL 0 
     LABEL "Cust Num" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.

DEFINE VARIABLE Name AS CHARACTER FORMAT "x(30)" 
     LABEL "Name" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE RepName AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE SalesRep AS CHARACTER FORMAT "x(4)" 
     LABEL "Sales Rep" 
     VIEW-AS FILL-IN 
     SIZE 9.6 BY 1.

DEFINE RECTANGLE tbCustomer
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 107.6 BY 1.29.

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
      Customer.Comments COLUMN-LABEL "Comments" FORMAT "x(80)":U
      Customer.SalesRep COLUMN-LABEL "Sales Rep" FORMAT "x(4)":U
      Customer.RepName COLUMN-LABEL "Rep Name" FORMAT "x(30)":U
  ENABLE
      Customer.CustNum HELP "Please enter a customer number."
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 106 BY 9.76 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     new_tbCustomer AT ROW 1.33 COL 4.2 WIDGET-ID 42
     edit_tbCustomer AT ROW 1.33 COL 8.8 WIDGET-ID 44
     copy_tbCustomer AT ROW 1.33 COL 13.4 WIDGET-ID 46
     undo_tbCustomer AT ROW 1.33 COL 18 WIDGET-ID 48
     save_tbCustomer AT ROW 1.33 COL 22.6 WIDGET-ID 50
     delete_tbCustomer AT ROW 1.33 COL 27.2 WIDGET-ID 52
     refresh_tbCustomer AT ROW 1.33 COL 31.8 WIDGET-ID 54
     filter_tbCustomer AT ROW 1.33 COL 36.4 WIDGET-ID 56
     browseconfig_tbCustomer AT ROW 1.33 COL 41 WIDGET-ID 58
     excel_tbCustomer AT ROW 1.33 COL 45.6 WIDGET-ID 60
     BrwCustomer AT ROW 3.14 COL 3 WIDGET-ID 200
     CustNum AT ROW 13.91 COL 19 COLON-ALIGNED HELP
          "Please enter a customer number."
     Name AT ROW 15 COL 19 COLON-ALIGNED HELP
          "Please enter a name."
     Comments AT ROW 16.1 COL 21 HELP
          "Please enter comments." NO-LABEL WIDGET-ID 12
     SalesRep AT ROW 19.76 COL 19 COLON-ALIGNED HELP
          "Please Enter a Sales Rep."
     btnSalesRep AT ROW 19.76 COL 30.6 WIDGET-ID 14 NO-TAB-STOP 
     RepName AT ROW 19.76 COL 32.6 COLON-ALIGNED HELP
          "Please enter the Name of the Salesperson." NO-LABEL
     tbCustomer AT ROW 1.24 COL 4 WIDGET-ID 40
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 111.6 BY 20.24 WIDGET-ID 100.


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
         HEIGHT             = 20.24
         WIDTH              = 111.6
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
/* BROWSE-TAB BrwCustomer excel_tbCustomer DEFAULT-FRAME */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 20.24
       FRAME DEFAULT-FRAME:WIDTH            = 111.6.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BrwCustomer
/* Query rebuild information for BROWSE BrwCustomer
     _TblList          = "sports113.Customer"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > "_<CALC>"
"Customer.CustNum" "Cust Num" ">>>>9" "integer" ? ? ? ? ? ? yes "Please enter a customer number." no no "9.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > "_<CALC>"
"Customer.Name" "Name" "x(30)" "character" ? ? ? ? ? ? no "Please enter a name." no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > "_<CALC>"
"Customer.Comments" "Comments" "x(80)" "character" ? ? ? ? ? ? no "Please enter comments." no no "80" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > "_<CALC>"
"Customer.SalesRep" "Sales Rep" "x(4)" "character" ? ? ? ? ? ? no "Please Enter a Sales Rep." no no "9.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > "_<CALC>"
"Customer.RepName" "Rep Name" "x(30)" "character" ? ? ? ? ? ? no "Please enter the Name of the Salesperson." no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
  DISPLAY CustNum Name Comments SalesRep RepName 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE tbCustomer new_tbCustomer edit_tbCustomer copy_tbCustomer 
         undo_tbCustomer save_tbCustomer delete_tbCustomer refresh_tbCustomer 
         filter_tbCustomer browseconfig_tbCustomer excel_tbCustomer BrwCustomer 
         CustNum Name Comments SalesRep btnSalesRep RepName 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeComponents C-Win 
PROCEDURE InitializeComponents :
&IF DEFINED(AdvGuiWin) &THEN
DO WITH FRAME {&FRAME-NAME}:
  oComments_JBoxDevExEdit = NEW JBoxDevExEdit(THIS-PROCEDURE,Comments:HANDLE).
  oComments_JBoxDevExEdit:RegisterWithJukeBox(YES). /* YES: Visible */
  oComments_JBoxDevExEdit:CreateDisplayLink(oFmCustomer:BUFFER-HANDLE,'Comments').
  oComments_JBoxDevExEdit:cDocLoadContext = 'Customer.Comments'.
  oComments_JBoxDevExEdit:cDocLoadIdFields = 'CustNum'.
  oComments_JBoxDevExEdit:cDocLoadFormat = 'html'.
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

DEF VAR oDatePickOrderDate AS JBoxDevExDateEdit NO-UNDO.
DYNAMIC-FUNCTION("CreateStatusBar",THIS-PROCEDURE:CURRENT-WINDOW,"",0,YES,?).

DO WITH FRAME {&FRAME-NAME}:

  oBrwCustomer = NEW JBoxBrowse(brwCustomer:HANDLE).

  oFmCustomer = NEW JBoxFieldMap(FRAME {&FRAME-NAME}:HANDLE).
  oFmCustomer:updateFields = 'CustNum,Name,Comments,SalesRep'.
  oFmCustomer:displayFields = 'RepName'.
  oFmCustomer:primaryKeyFields = 'CustNum'.

  oFmCustomer:BROWSE-OBJECT = oBrwCustomer.
  oTbCustomer = NEW JBoxToolbar(tbCustomer:HANDLE,'File').

  oBrwCustomer:TOOLBAR-OBJECT = oTbCustomer.
  oFmCustomer:TOOLBAR-OBJECT = oTbCustomer.
  
  oDatePickOrderDate = NEW JBoxDevExDateEdit(THIS-PROCEDURE,OrderDate:HANDLE,0).
  RUN InitializeComponents.
END.
oBrwCustomer:OpenQuery().

DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,
                                                                                "Comments").
                                                                                

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
    WHEN "SalesRep" THEN cReturn = DYNAMIC-FUNCTION("getFieldValues","Salesrep","WHERE SalesRep = '" + ihField:SCREEN-VALUE + "'","Region").
  END CASE.

  MESSAGE PROGRAM-NAME(1) SKIP 
          cReturn
          VIEW-AS ALERT-BOX.
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

