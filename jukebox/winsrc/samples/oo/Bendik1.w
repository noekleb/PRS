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
DEF VAR oBirthdate_JBoxDevExEdit AS JBoxDevExDateEdit NO-UNDO.

/*** Start instance property definitions for JBoxBrowse object oBrwEmployee ***/
DEF VAR oBrwEmployee AS JBoxBrowse NO-UNDO.
DEF TEMP-TABLE Employee
    FIELD EmpNum AS integer
    FIELD FirstName AS character
    FIELD LastName AS character
    FIELD Address AS character
    FIELD Address2 AS character
    FIELD Birthdate AS date
    FIELD City AS character
    FIELD DeptCode AS character
    FIELD DeptName AS character
    FIELD RowIdent1 AS CHARACTER 
    FIELD RowIdent2 AS CHARACTER 
    FIELD RowCount AS INTEGER
    FIELD jbCountDistinct AS INTEGER FORMAT '>>>,>>>,>>9' INIT 1
    FIELD jbAverage AS DECIMAL FORMAT '->>>,>>>,>>9.99'
    INDEX idxRowids  RowIdent1 RowIdent2
    .
DEF BUFFER v_Employee FOR TEMP-TABLE Employee.


FUNCTION getBuffersAndFieldsBrwEmployee RETURNS CHARACTER():
  RETURN
    'Employee'
     + ';EmpNum'
     + ';FirstName'
     + ';LastName'
     + ';Address'
     + ';Address2'
     + ';Birthdate'
     + ';City'
     + ';DeptCode'
  + ',Department'
     + ';DeptName'
     .
END FUNCTION.
FUNCTION getQueryJoinBrwEmployee RETURNS CHARACTER():
  RETURN 'EACH Department OF Employee NO-LOCK'.
END FUNCTION.


DEF VAR oFmEmployee AS JBoxFieldMap NO-UNDO.
DEF VAR otbEmployee AS JBoxToolbar NO-UNDO.


DEF VAR opopupEmployee AS JBoxPopupMenu NO-UNDO.

DEF VAR oSelector AS JBoxDynSelector NO-UNDO.

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
Employee.Birthdate Employee.City Employee.DeptCode Employee.DeptName 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BrwEmployee Employee.EmpNum 
&Scoped-define QUERY-STRING-BrwEmployee FOR EACH Employee NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BrwEmployee OPEN QUERY BrwEmployee FOR EACH Employee NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BrwEmployee Employee
&Scoped-define FIRST-TABLE-IN-QUERY-BrwEmployee Employee


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tbEmployee new_tbEmployee edit_tbEmployee ~
copy_tbEmployee undo_tbEmployee save_tbEmployee delete_tbEmployee ~
filter_tbEmployee browseconfig_tbEmployee excel_tbEmployee BrwEmployee ~
EmpNum Birthdate FirstName LastName Address City DeptCode btnDeptCode ~
DeptName btnDeptName 
&Scoped-Define DISPLAYED-OBJECTS EmpNum Birthdate FirstName LastName ~
Address City DeptCode DeptName 

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

DEFINE BUTTON btnDeptCode 
     LABEL "..." 
     SIZE 4 BY 1.

DEFINE BUTTON btnDeptName 
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

DEFINE BUTTON new_tbEmployee 
     IMAGE-UP FILE "bmp/new16e.bmp":U
     LABEL "Ny" 
     SIZE 6 BY 1.52 TOOLTIP "Ny (CTRL-N)".

DEFINE BUTTON save_tbEmployee 
     IMAGE-UP FILE "bmp/save.bmp":U
     LABEL "Lagre" 
     SIZE 6 BY 1.52 TOOLTIP "Lagre (CTRL-S)".

DEFINE BUTTON undo_tbEmployee 
     IMAGE-UP FILE "bmp/undo16e.bmp":U
     LABEL "Angre" 
     SIZE 6 BY 1.52 TOOLTIP "Angre (CTRL-Z)".

DEFINE VARIABLE Address AS CHARACTER FORMAT "x(35)" 
     LABEL "Address" 
     VIEW-AS FILL-IN 
     SIZE 37 BY 1 TOOLTIP "Please enter the address".

DEFINE VARIABLE Birthdate AS DATE FORMAT "99/99/9999" 
     LABEL "Birthdate" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 TOOLTIP "Please enter the birth date." NO-UNDO.

DEFINE VARIABLE City AS CHARACTER FORMAT "x(25)" 
     LABEL "City" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1 TOOLTIP "Please enter the city.".

DEFINE VARIABLE DeptCode AS CHARACTER FORMAT "x(3)" 
     LABEL "Dept Code" 
     VIEW-AS FILL-IN 
     SIZE 8.6 BY 1 TOOLTIP "Please enter the Dept Code".

DEFINE VARIABLE DeptName AS CHARACTER FORMAT "x(15)" 
     LABEL "Dept Name" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 TOOLTIP "Please enter the Dept Name".

DEFINE VARIABLE EmpNum AS INTEGER FORMAT "zzzzzzzzz9" INITIAL 0 
     LABEL "Emp No" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 TOOLTIP "Please enter the Emp Num".

DEFINE VARIABLE FirstName AS CHARACTER FORMAT "x(15)" 
     LABEL "First Name" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 TOOLTIP "Please enter the First Name.".

DEFINE VARIABLE LastName AS CHARACTER FORMAT "x(25)" 
     LABEL "Last Name" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1 TOOLTIP "Please enter the last name.".

DEFINE RECTANGLE tbEmployee
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 107.8 BY 1.71.

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
      Employee.Birthdate COLUMN-LABEL "Birthdate" FORMAT "99/99/9999":U
      Employee.City COLUMN-LABEL "City" FORMAT "x(25)":U
      Employee.DeptCode COLUMN-LABEL "Dept Code" FORMAT "x(3)":U
      Employee.DeptName COLUMN-LABEL "Dept Name" FORMAT "x(15)":U
  ENABLE
      Employee.EmpNum HELP "Please enter the Emp Num"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 104 BY 7.62 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     new_tbEmployee AT ROW 1.33 COL 1.2 WIDGET-ID 18
     edit_tbEmployee AT ROW 1.33 COL 7.4 WIDGET-ID 34
     copy_tbEmployee AT ROW 1.33 COL 13.4 WIDGET-ID 20
     undo_tbEmployee AT ROW 1.33 COL 19.4 WIDGET-ID 22
     save_tbEmployee AT ROW 1.33 COL 25.4 WIDGET-ID 24
     delete_tbEmployee AT ROW 1.33 COL 31.4 WIDGET-ID 26
     filter_tbEmployee AT ROW 1.33 COL 37.4 WIDGET-ID 28
     browseconfig_tbEmployee AT ROW 1.33 COL 43.4 WIDGET-ID 30
     excel_tbEmployee AT ROW 1.33 COL 49.4 WIDGET-ID 32
     BrwEmployee AT ROW 3.14 COL 4 WIDGET-ID 200
     EmpNum AT ROW 12 COL 14 COLON-ALIGNED HELP
          "Please enter the Emp Num"
     Birthdate AT ROW 12 COL 52 COLON-ALIGNED HELP
          "Please enter the birth date."
     FirstName AT ROW 13 COL 14 COLON-ALIGNED HELP
          "Please enter the First Name."
     LastName AT ROW 14 COL 14 COLON-ALIGNED HELP
          "Please enter the last name."
     Address AT ROW 15 COL 14 COLON-ALIGNED HELP
          "Please enter the address"
     City AT ROW 16 COL 14 COLON-ALIGNED HELP
          "Please enter the city."
     DeptCode AT ROW 17 COL 14 COLON-ALIGNED HELP
          "Please enter the Dept Code"
     btnDeptCode AT ROW 17 COL 24.6 WIDGET-ID 36 NO-TAB-STOP 
     DeptName AT ROW 18 COL 14 COLON-ALIGNED HELP
          "Please enter the Dept Name"
     btnDeptName AT ROW 18 COL 33 WIDGET-ID 38 NO-TAB-STOP 
     tbEmployee AT ROW 1.24 COL 1 WIDGET-ID 16
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 108.8 BY 19.52 WIDGET-ID 100.


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
         HEIGHT             = 19.52
         WIDTH              = 108.8
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
/* BROWSE-TAB BrwEmployee excel_tbEmployee DEFAULT-FRAME */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 19.52
       FRAME DEFAULT-FRAME:WIDTH            = 108.8.

ASSIGN 
       tbEmployee:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "new;Ny,edit;Edit,copy;Kopier,undo;Angre,save;Lagre,delete;Slett,filter;Filter,browseconfig;Column setup,excel;Eksporter til E&xcelmaxborder".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BrwEmployee
/* Query rebuild information for BROWSE BrwEmployee
     _TblList          = "sports117.Employee"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > "_<CALC>"
"Employee.EmpNum" "Emp No" "zzzzzzzzz9" "integer" ? ? ? ? ? ? yes "Please enter the Emp Num" no no "10.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > "_<CALC>"
"Employee.FirstName" "First Name" "x(15)" "character" ? ? ? ? ? ? no "Please enter the First Name." no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > "_<CALC>"
"Employee.LastName" "Last Name" "x(25)" "character" ? ? ? ? ? ? no "Please enter the last name." no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > "_<CALC>"
"Employee.Address" "Address" "x(35)" "character" ? ? ? ? ? ? no "Please enter the address" no no "35" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > "_<CALC>"
"Employee.Address2" "Address2" "x(35)" "character" ? ? ? ? ? ? no "Please enter the address." no no "35" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > "_<CALC>"
"Employee.Birthdate" "Birthdate" "99/99/9999" "date" ? ? ? ? ? ? no "Please enter the birth date." no no "11.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > "_<CALC>"
"Employee.City" "City" "x(25)" "character" ? ? ? ? ? ? no "Please enter the city." no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > "_<CALC>"
"Employee.DeptCode" "Dept Code" "x(3)" "character" ? ? ? ? ? ? no "Please enter the Dept Code" no no "10.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > "_<CALC>"
"Employee.DeptName" "Dept Name" "x(15)" "character" ? ? ? ? ? ? no "Please enter the Dept Name" no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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


&Scoped-define SELF-NAME btnDeptCode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDeptCode C-Win
ON CHOOSE OF btnDeptCode IN FRAME DEFAULT-FRAME /* ... */
DO:
  DEF VAR cRowIdList  AS CHAR NO-UNDO.
  
/*  JBoxServerAPI:Instance:Lookup("Department"*/
  JBoxServerAPI:Instance:Selector("Department"
                    + ";DeptCode"
                    + ";DeptName"
                   ,"WHERE true"
                   ,cRowIdList
                   ,"DeptCode,DeptName").
  
  IF JBoxServerAPI:Instance:SelectorOk THEN
    MESSAGE JBoxServerAPI:Instance:SelectorReturnValueList
    VIEW-AS ALERT-BOX.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDeptName
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDeptName C-Win
ON CHOOSE OF btnDeptName IN FRAME DEFAULT-FRAME /* ... */
DO:
  DEF VAR cRowIdList  AS CHAR NO-UNDO.

/*  cRowIdList = JBoxServerAPI:Instance:getRowIdList("_File","WHERE RepName BEGINS 'B'").*/
/*  cRowIdList = JBoxServerAPI:Instance:getRowIdList("<TableList>","<table for rowid>","<query>"). */
  
/*  JBoxServerAPI:Instance:SelectorDialog("SalesRep" */      
  JBoxServerAPI:Instance:Selector("_file"      
                      + ";_file-name"  
                    + ",_db"
                      + ";_db-name"
                      ,"where true,first _db of _file",
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
{incl/conttrigg.i oBrwEmployee:BROWSE-HANDLE}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplayRecord C-Win 
PROCEDURE DisplayRecord :
otbEmployee:disabledTools = if avail Employee and Employee.DeptCode = "100" then "edit" else "".
  RUN SUPER.
  IF oBrwEmployee:isCurrent THEN  DO WITH FRAME {&FRAME-NAME}:
  END.

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
  DISPLAY EmpNum Birthdate FirstName LastName Address City DeptCode DeptName 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE tbEmployee new_tbEmployee edit_tbEmployee copy_tbEmployee 
         undo_tbEmployee save_tbEmployee delete_tbEmployee filter_tbEmployee 
         browseconfig_tbEmployee excel_tbEmployee BrwEmployee EmpNum Birthdate 
         FirstName LastName Address City DeptCode btnDeptCode DeptName 
         btnDeptName 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeComponents C-Win 
PROCEDURE InitializeComponents :
DO WITH FRAME {&FRAME-NAME}:


  oBirthdate_JBoxDevExEdit = NEW JBoxDevExDateEdit(THIS-PROCEDURE,Birthdate:HANDLE).
  oBirthdate_JBoxDevExEdit:RegisterWithJukeBox(YES). /* YES: Visible */
  oBirthdate_JBoxDevExEdit:CreateDisplayLink(oFmEmployee:BUFFER-HANDLE,'Birthdate').
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
DEF VAR oBrwFillInDeptCode AS JBoxBrowseFillIn NO-UNDO.
DEF VAR oBrwDropDownDeptName AS JBoxBrowseDropDown NO-UNDO.

RUN enable_UI.

oContainer = NEW JBoxContainer().
oContainer:addStatusBar().

DO WITH FRAME {&FRAME-NAME}:

  oBrwEmployee = NEW JBoxBrowse(brwEmployee:HANDLE).

  oFmEmployee = NEW JBoxFieldMap(FRAME {&FRAME-NAME}:HANDLE).
  oFmEmployee:updateFields = 'EmpNum,FirstName,LastName,Address,City,DeptCode,Birthdate'.
  oFmEmployee:displayFields = 'DeptName'.
  oFmEmployee:primaryKeyFields = 'EmpNum'.
     
  oFmEmployee:BROWSE-OBJECT = oBrwEmployee.
  otbEmployee = NEW JBoxToolbar(tbEmployee:HANDLE).

  oBrwEmployee:TOOLBAR-OBJECT = otbEmployee.
  oFmEmployee:TOOLBAR-OBJECT = otbEmployee.
  RUN InitializeComponents.
  opopupEmployee = NEW JBoxPopupMenu().
  opopupEmployee:AddToolGroup('multiSortBrowse;Sorter på flere kolonner,copyBrowseRows;Kopier rad(er),rowsToBatch;Sett rader i resultatsett').

  oBrwEmployee:POPUP-MENU-OBJECT = opopupEmployee.

  oBrwDropDownDeptName = NEW JBoxBrowseDropDown(oBrwEmployee,"deptname","deptcode","Department;Deptname;deptcode","WHERE true").

  oBrwFillInDeptCode = NEW JBoxBrowseFillIn(oBrwEmployee,"DeptCode","Department;DeptCode;DeptName","WHERE true","deptCode").
END.
oBrwEmployee:OpenQuery().


oContainer:initResize(300,200).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MySaveBrowseFillIn C-Win 
PROCEDURE MySaveBrowseFillIn :
DEF INPUT  PARAM ihFillIn AS HANDLE NO-UNDO.
DEF INPUT  PARAM ihBuffer AS HANDLE NO-UNDO.
DEF OUTPUT PARAM obOk     AS LOG    NO-UNDO INIT YES.


ihBuffer:BUFFER-FIELD (ihFillIn:NAME):BUFFER-VALUE () = ihFillIn:SCREEN-VALUE.

IF ihFillIN:NAME = "DeptCode" THEN RETURN.

oSelector:TARGET-BROWSE-OBJECT:Refresh().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE mySelectorObject C-Win 
PROCEDURE mySelectorObject :
DEF INPUT PARAM ioSelector AS JBoxDynSelector NO-UNDO.

DEF VAR oFillIn AS JBoxBrowseFillIn NO-UNDO.

oSelector = ioSelector.

ioSelector:TOOLBAR-OBJECT:addTool("Filter").
ioSelector:ExpandSelectorDialogX(400).

IF ioSelector:SOURCE-BROWSE-OBJECT:BUFFER-HANDLE:Name = "Department" THEN DO:
  ioSelector:SOURCE-BROWSE-OBJECT:BROWSE-HANDLE:GET-BROWSE-COLUMN(2):VISIBLE = NO.
  oFillIn = NEW JBoxBrowseFillIn(ioSelector:TARGET-BROWSE-OBJECT,"DeptName").
  ioSelector:TARGET-BROWSE-OBJECT:useLocalData = YES.
END.  

ioSelector:SOURCE-BROWSE-OBJECT:openQuery().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

