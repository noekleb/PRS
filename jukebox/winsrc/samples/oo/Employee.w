&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          sports117        PROGRESS
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


/*** Start instance property definitions for JBoxBrowse object oBrwEmployee ***/
DEF VAR oBrwEmployee AS JBoxBrowse NO-UNDO.
DEF TEMP-TABLE Employee
    FIELD EmpNum AS integer
    FIELD FirstName AS character
    FIELD LastName AS character
    FIELD Address AS character
    FIELD Address2 AS character
    FIELD PostalCode AS character
    FIELD City AS character
    FIELD Relation AS character
    FIELD RelativeName AS character
    FIELD DeptCode AS character
    FIELD RowIdent1 AS CHARACTER 
    FIELD RowIdent2 AS CHARACTER 
    FIELD RowCount AS INTEGER
    FIELD jbCountDistinct AS INTEGER FORMAT '>>>,>>>,>>9' INIT 1
    FIELD jbAverage AS DECIMAL FORMAT '->>>,>>>,>>9.99'
    INDEX idxRowids  RowIdent1 RowIdent2
    .
DEF BUFFER v_Employee FOR Employee.


FUNCTION getBuffersAndFieldsBrwEmployee RETURNS CHARACTER():
  RETURN
    'Employee'
     + ';EmpNum'
     + ';FirstName'
     + ';LastName'
     + ';Address'
     + ';Address2'
     + ';PostalCode'
     + ';City'
     + ';DeptCode'
  + ',Family'
     + ';Relation'
     + ';RelativeName'
     .
END FUNCTION.
FUNCTION getQueryJoinBrwEmployee RETURNS CHARACTER():
  RETURN 'EACH Family OF Employee NO-LOCK'.
END FUNCTION.
DEF VAR opopupEmployee AS JBoxPopupMenu NO-UNDO.
DEF VAR oFmEmployee AS JBoxFieldMap NO-UNDO.
DEF VAR otbEmployee AS JBoxToolbar NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BrwEmployee

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Employee

/* Definitions for BROWSE BrwEmployee                                   */
&Scoped-define FIELDS-IN-QUERY-BrwEmployee Employee.EmpNum ~
Employee.FirstName Employee.LastName Employee.Address Employee.Address2 ~
Employee.PostalCode Employee.City Employee.Relation Employee.RelativeName ~
Employee.DeptCode 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BrwEmployee Employee.EmpNum 
&Scoped-define ENABLED-TABLES-IN-QUERY-BrwEmployee Employee
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BrwEmployee Employee
&Scoped-define QUERY-STRING-BrwEmployee FOR EACH Employee NO-LOCK, ~
    EACH Family OF Employee NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BrwEmployee OPEN QUERY BrwEmployee FOR EACH Employee NO-LOCK, ~
    EACH Family OF Employee NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BrwEmployee Employee
&Scoped-define FIRST-TABLE-IN-QUERY-BrwEmployee Employee


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tbEmployee new_tbEmployee edit_tbEmployee ~
copy_tbEmployee undo_tbEmployee delete_tbEmployee save_tbEmployee ~
excel_tbEmployee filter_tbEmployee browseconfig_tbEmployee ~
refresh_tbEmployee flatview_tbEmployee BrwEmployee EmpNum DeptCode ~
FirstName LastName Address PostalCode btnPostalCode City 
&Scoped-Define DISPLAYED-OBJECTS EmpNum DeptCode FirstName LastName Address ~
PostalCode City 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON browseconfig_tbEmployee 
     IMAGE-UP FILE "bmp/table.bmp":U
     LABEL "Column setup" 
     SIZE 6 BY 1.52 TOOLTIP "Column setup (ALT-C)".

DEFINE BUTTON btnPostalCode 
     LABEL "..." 
     SIZE 4 BY 1.

DEFINE BUTTON copy_tbEmployee 
     IMAGE-UP FILE "bmp/copy.bmp":U
     LABEL "Kopier" 
     SIZE 6 BY 1.52 TOOLTIP "Kopier (ALT-K)".

DEFINE BUTTON delete_tbEmployee 
     IMAGE-UP FILE "bmp/del16e.bmp":U
     LABEL "Slett" 
     SIZE 6 BY 1.52 TOOLTIP "Slett (CTRL-D)".

DEFINE BUTTON edit_tbEmployee 
     IMAGE-UP FILE "bmp/edit16e.bmp":U
     LABEL "Edit" 
     SIZE 6 BY 1.52 TOOLTIP "Edit (CTRL-E)".

DEFINE BUTTON excel_tbEmployee 
     IMAGE-UP FILE "gif/afexcel.gif":U
     LABEL "Eksporter til E&xcel" 
     SIZE 6 BY 1.52 TOOLTIP "Eksporter til E&xcel (ALT-X)".

DEFINE BUTTON filter_tbEmployee 
     IMAGE-UP FILE "gif/filter.gif":U
     LABEL "Filter" 
     SIZE 6 BY 1.52 TOOLTIP "Filter (CTRL-F)".

DEFINE BUTTON flatview_tbEmployee 
     IMAGE-UP FILE "gif/sdogen16.gif":U
     LABEL "Drill-down view" 
     SIZE 6 BY 1.52 TOOLTIP "Drill-down view (ALT-D)".

DEFINE BUTTON new_tbEmployee 
     IMAGE-UP FILE "bmp/new16e.bmp":U
     LABEL "Ny" 
     SIZE 6 BY 1.52 TOOLTIP "Ny (CTRL-N)".

DEFINE BUTTON refresh_tbEmployee 
     IMAGE-UP FILE "gif/refresh.gif":U
     LABEL "Refresh" 
     SIZE 6 BY 1.52 TOOLTIP "Refresh (F5)".

DEFINE BUTTON save_tbEmployee 
     IMAGE-UP FILE "bmp/save.bmp":U
     LABEL "Lagre" 
     SIZE 6 BY 1.52 TOOLTIP "Lagre (CTRL-S)".

DEFINE BUTTON undo_tbEmployee 
     IMAGE-UP FILE "bmp/undo16e.bmp":U
     LABEL "Angre" 
     SIZE 6 BY 1.52 TOOLTIP "Angre (CTRL-Z)".

DEFINE VARIABLE DeptCode AS CHARACTER FORMAT "x(3)" 
     LABEL "Dept Code" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1","1"
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE Address AS CHARACTER FORMAT "x(35)" 
     LABEL "Address" 
     VIEW-AS FILL-IN 
     SIZE 37 BY 1.

DEFINE VARIABLE City AS CHARACTER FORMAT "x(25)" 
     LABEL "City" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1.

DEFINE VARIABLE EmpNum AS INTEGER FORMAT "zzzzzzzzz9" INITIAL 0 
     LABEL "Emp No" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE FirstName AS CHARACTER FORMAT "x(15)" 
     LABEL "First Name" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE LastName AS CHARACTER FORMAT "x(25)" 
     LABEL "Last Name" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1.

DEFINE VARIABLE PostalCode AS CHARACTER FORMAT "x(10)" 
     LABEL "Postal Code" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE RECTANGLE tbEmployee
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 110.2 BY 1.71.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BrwEmployee FOR 
      Employee SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BrwEmployee
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BrwEmployee C-Win _STRUCTURED
  QUERY BrwEmployee NO-LOCK DISPLAY
      Employee.EmpNum COLUMN-LABEL "Emp No" FORMAT "zzzzzzzzz9":U
            WIDTH 10.2
      Employee.FirstName COLUMN-LABEL "First Name" FORMAT "x(15)":U
      Employee.LastName COLUMN-LABEL "Last Name" FORMAT "x(25)":U
      Employee.Address COLUMN-LABEL "Address" FORMAT "x(35)":U
      Employee.Address2 COLUMN-LABEL "Address2" FORMAT "x(35)":U
      Employee.PostalCode COLUMN-LABEL "Postal Code" FORMAT "x(10)":U
      Employee.City COLUMN-LABEL "City" FORMAT "x(25)":U
      Employee.Relation COLUMN-LABEL "Relation" FORMAT "x(15)":U
      Employee.RelativeName COLUMN-LABEL "Relative Name" FORMAT "x(15)":U
      Employee.DeptCode FORMAT "x(3)":U
  ENABLE
      Employee.EmpNum HELP "Please enter the Emp Num"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 109 BY 6.67 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     new_tbEmployee AT ROW 1.57 COL 3.2 WIDGET-ID 16
     edit_tbEmployee AT ROW 1.57 COL 9.4 WIDGET-ID 18
     copy_tbEmployee AT ROW 1.57 COL 15.4 WIDGET-ID 20
     undo_tbEmployee AT ROW 1.57 COL 21.4 WIDGET-ID 22
     delete_tbEmployee AT ROW 1.57 COL 27.4 WIDGET-ID 24
     save_tbEmployee AT ROW 1.57 COL 33.4 WIDGET-ID 26
     excel_tbEmployee AT ROW 1.57 COL 39.4 WIDGET-ID 28
     filter_tbEmployee AT ROW 1.57 COL 45.4 WIDGET-ID 30
     browseconfig_tbEmployee AT ROW 1.57 COL 51.4 WIDGET-ID 32
     refresh_tbEmployee AT ROW 1.57 COL 57.4 WIDGET-ID 34
     flatview_tbEmployee AT ROW 1.57 COL 63.4 WIDGET-ID 36
     BrwEmployee AT ROW 3.86 COL 3 WIDGET-ID 200
     EmpNum AT ROW 11.48 COL 18 COLON-ALIGNED
     DeptCode AT ROW 12 COL 74 COLON-ALIGNED WIDGET-ID 40
     FirstName AT ROW 12.48 COL 18 COLON-ALIGNED
     LastName AT ROW 13.48 COL 18 COLON-ALIGNED
     Address AT ROW 14.48 COL 18 COLON-ALIGNED
     PostalCode AT ROW 15.48 COL 18 COLON-ALIGNED
     btnPostalCode AT ROW 15.48 COL 35.6 WIDGET-ID 38 NO-TAB-STOP 
     City AT ROW 16.48 COL 18 COLON-ALIGNED
     tbEmployee AT ROW 1.48 COL 3 WIDGET-ID 14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 113.2 BY 17.52 WIDGET-ID 100.


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
         HEIGHT             = 17.52
         WIDTH              = 113.2
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
/* BROWSE-TAB BrwEmployee flatview_tbEmployee DEFAULT-FRAME */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 17.52
       FRAME DEFAULT-FRAME:WIDTH            = 113.2.

ASSIGN 
       BrwEmployee:PRIVATE-DATA IN FRAME DEFAULT-FRAME           = 
                "multiSortBrowse;Sort on multiple columns".

ASSIGN 
       tbEmployee:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "new;Ny,edit;Edit,copy;Kopier,undo;Angre,delete;Slett,save;Lagre,excel;Eksporter til E&xcel,filter;Filter,browseconfig;Column setup,refresh;Refresh,flatview;Drill-down viewmaxborder".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BrwEmployee
/* Query rebuild information for BROWSE BrwEmployee
     _TblList          = "sports117.Employee"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Employee.EmpNum
"Employee.EmpNum" "Emp No" "zzzzzzzzz9" "integer" ? ? ? ? ? ? yes "Please enter the Emp Num" no no "10.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Employee.FirstName
"Employee.FirstName" "First Name" "x(15)" "character" ? ? ? ? ? ? no "Please enter the First Name." no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Employee.LastName
"Employee.LastName" "Last Name" "x(25)" "character" ? ? ? ? ? ? no "Please enter the last name." no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Employee.Address
"Employee.Address" "Address" "x(35)" "character" ? ? ? ? ? ? no "Please enter the address" no no "35" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Employee.Address2
"Employee.Address2" "Address2" "x(35)" "character" ? ? ? ? ? ? no "Please enter the address." no no "35" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Employee.PostalCode
"Employee.PostalCode" "Postal Code" "x(10)" "character" ? ? ? ? ? ? no "Please enter the Postal Code." no no "11.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Employee.City
"Employee.City" "City" "x(25)" "character" ? ? ? ? ? ? no "Please enter the city." no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > Employee.Relation
"Employee.Relation" "Relation" "x(15)" "character" ? ? ? ? ? ? no "Please enter Spouse or Child." no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > Employee.RelativeName
"Employee.RelativeName" "Relative Name" "x(15)" "character" ? ? ? ? ? ? no "Please enter the Relative Name" no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > Employee.DeptCode
"Employee.DeptCode" "Dept Code" "x(3)" "character" ? ? ? ? ? ? no "Please enter the Dept Code" no no "10.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BrwEmployee */
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


&Scoped-define SELF-NAME btnPostalCode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPostalCode C-Win
ON CHOOSE OF btnPostalCode IN FRAME DEFAULT-FRAME /* ... */
DO:
  DEF VAR cRowIdList  AS CHAR NO-UNDO.

/*  cRowIdList = JBoxServerAPI:Instance:getRowIdList("SalesRep","WHERE RepName BEGINS 'B'").*/
/*  cRowIdList = JBoxServerAPI:Instance:getRowIdList("<TableList>","<table for rowid>","<query>"). */
  
/*  JBoxServerAPI:Instance:SelectorDialog("SalesRep" */      
  JBoxServerAPI:Instance:Selector("_field"      
                      + ";_field-name"  
                      + ",_file"
                      + ",_db"
                      ,"where true,first _file of _field no-lock where _file-name = '" + ldbname(1) + ".customer'",
                      cRowIdList).
  IF JBoxServerAPI:Instance:SelectorOk THEN
    MESSAGE JBoxServerAPI:Instance:SelectorRowidList skip(1)
            JBoxServerAPI:Instance:SelectorDeselectRowidList
    VIEW-AS ALERT-BOX.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BrwEmployee
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
  DISPLAY EmpNum DeptCode FirstName LastName Address PostalCode City 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE tbEmployee new_tbEmployee edit_tbEmployee copy_tbEmployee 
         undo_tbEmployee delete_tbEmployee save_tbEmployee excel_tbEmployee 
         filter_tbEmployee browseconfig_tbEmployee refresh_tbEmployee 
         flatview_tbEmployee BrwEmployee EmpNum DeptCode FirstName LastName 
         Address PostalCode btnPostalCode City 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExtraFlatViewRecord C-Win 
PROCEDURE ExtraFlatViewRecord :
DEF INPUT PARAM ihFlatView   AS HANDLE NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO INIT TRUE.

DEF VAR hFlatBrw    AS HANDLE NO-UNDO.

hFlatBrw  = DYNAMIC-FUNCTION("getBrowseHandle" IN ihFlatView).

DYNAMIC-FUNCTION("setAttribute",hFlatBrw,"buffersAndFields","Customer;Custnum;Name,SalesRep;RepName").
DYNAMIC-FUNCTION("setAttribute",hFlatBrw,"queryJoin",",FIRST SalesRep OF Customer NO-LOCK").
DYNAMIC-FUNCTION("setAttribute",hFlatBrw,"querySort","Name").
DYNAMIC-FUNCTION("setAttribute",hFlatBrw,"flatViewTitle","Kunder").


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
  
  DeptCode:DELIMITER = "|".
  DeptCode:LIST-ITEM-PAIRS = "||" + JBoxServerAPI:Instance:FieldListValid("Department;DeptName;DeptCode","where true").

  oBrwEmployee = NEW JBoxBrowse(brwEmployee:HANDLE).

  opopupEmployee = NEW JBoxPopupMenu().
  opopupEmployee:AddToolGroup('multiSortBrowse;Sort on multiple columns').

  oBrwEmployee:POPUP-MENU-OBJECT = opopupEmployee.
  oFmEmployee = NEW JBoxFieldMap(FRAME {&FRAME-NAME}:HANDLE).
  oFmEmployee:updateFields = 'EmpNum,FirstName,LastName,Address,PostalCode,City,DeptCode'.
  oFmEmployee:primaryKeyFields = 'EmpNum'.

  oFmEmployee:BROWSE-OBJECT = oBrwEmployee.
  otbEmployee = NEW JBoxToolbar(tbEmployee:HANDLE).

  oBrwEmployee:TOOLBAR-OBJECT = otbEmployee.
  oFmEmployee:TOOLBAR-OBJECT = otbEmployee.
END.
oBrwEmployee:OpenQuery().


oContainer:initResize(1000,100).

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

