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

USING dotr11.3\JBoxToolbar.*.

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


/*** Start instance property definitions for JBoxBrowse object oBrwDepartment ***/
DEF VAR oBrwDepartment AS JBoxBrowse NO-UNDO.
DEF TEMP-TABLE Department
    FIELD DeptCode AS character
    FIELD DeptName AS character
    FIELD EmpNum AS integer
    FIELD Position AS character
    FIELD HomePhone AS character
    FIELD PostalCode AS character
    FIELD Fullname AS CHARACTER
    FIELD RowIdent1 AS CHARACTER 
    FIELD RowIdent2 AS CHARACTER 
    FIELD RowCount AS INTEGER
    FIELD jbCountDistinct AS INTEGER FORMAT '>>>,>>>,>>9' INIT 1
    FIELD jbAverage AS DECIMAL FORMAT '->>>,>>>,>>9.99'
    INDEX idxRowids  RowIdent1 RowIdent2
    .
DEF BUFFER v_Department FOR Department.


FUNCTION getBuffersAndFieldsBrwDepartment RETURNS CHARACTER():
  RETURN
    'Department'
     + ';DeptCode'
     + ';DeptName'
  + ',Employee'
     + ';EmpNum'
     + ';Position'
     + ';HomePhone'
     + ';PostalCode'
     + ';+Fullname|CHARACTER||jb_calc(LastName concatcomma FirstName)'
     .
END FUNCTION.
FUNCTION getQueryJoinBrwDepartment RETURNS CHARACTER():
  RETURN 'EACH Employee OF Department NO-LOCK'.
END FUNCTION.


DEF VAR oFmDepartment AS JBoxFieldMap NO-UNDO.
DEF VAR oTbDepartment AS JBoxToolbar NO-UNDO.


/*** Start instance property definitions for JBoxBrowse object oBrwFamily ***/
DEF VAR oBrwFamily AS JBoxBrowse NO-UNDO.
DEF TEMP-TABLE Family
    FIELD BenefitDate AS date
    FIELD Birthdate AS date
    FIELD Relation AS character
    FIELD RelativeName AS character
    FIELD CoveredOnBenefits AS logical
    FIELD EmpNum AS integer
    FIELD RowIdent1 AS CHARACTER 
    FIELD RowCount AS INTEGER
    FIELD jbCountDistinct AS INTEGER FORMAT '>>>,>>>,>>9' INIT 1
    FIELD jbAverage AS DECIMAL FORMAT '->>>,>>>,>>9.99'
    INDEX idxRowids  RowIdent1
    .
DEF BUFFER v_Family FOR Family.


FUNCTION getBuffersAndFieldsBrwFamily RETURNS CHARACTER():
  RETURN
    'Family'
     + ';BenefitDate'
     + ';Birthdate'
     + ';Relation'
     + ';RelativeName'
     + ';CoveredOnBenefits'
     + ';EmpNum'
     .
END FUNCTION.
FUNCTION getQueryJoinBrwFamily RETURNS CHARACTER():
  RETURN ''.
END FUNCTION.


DEF VAR oTbFamily AS JBoxToolbar NO-UNDO.


/* Code for Definitions: */

/*** Start instance property definitions for JBoxQuery object oQryCustomer ***/
DEF VAR oQryCustomer AS JBoxQuery NO-UNDO.

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
    FIELD RowIdent1 AS CHARACTER 
    FIELD RowCount AS INTEGER
    INDEX idxRowids  RowIdent1
    .
DEFINE BUFFER v_Customer FOR Customer.

FUNCTION getTableHandleQryCustomer RETURNS HANDLE().
  RETURN BUFFER Customer:HANDLE:TABLE-HANDLE.
END FUNCTION.
FUNCTION getBuffersAndFieldsQryCustomer RETURNS CHARACTER().
  RETURN 
      'Customer'.
END FUNCTION.
FUNCTION getQueryJoinQryCustomer RETURNS CHARACTER().
  RETURN ''.
END FUNCTION.
/*** End instance property settings for JBoxQuery object oQryCustomer ***/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BrwDepartment
&Scoped-define QUERY-NAME QUERY-3

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Department Family Customer

/* Definitions for BROWSE BrwDepartment                                 */
&Scoped-define FIELDS-IN-QUERY-BrwDepartment Department.DeptCode ~
Department.DeptName Department.EmpNum Department.Position ~
Department.HomePhone Department.PostalCode Department.Fullname 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BrwDepartment Department.DeptCode 
&Scoped-define QUERY-STRING-BrwDepartment FOR EACH Department NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BrwDepartment OPEN QUERY BrwDepartment FOR EACH Department NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BrwDepartment Department
&Scoped-define FIRST-TABLE-IN-QUERY-BrwDepartment Department


/* Definitions for BROWSE BrwFamily                                     */
&Scoped-define FIELDS-IN-QUERY-BrwFamily Family.BenefitDate ~
Family.Birthdate Family.Relation Family.RelativeName ~
Family.CoveredOnBenefits Family.EmpNum 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BrwFamily Family.BenefitDate 
&Scoped-define QUERY-STRING-BrwFamily FOR EACH Family NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BrwFamily OPEN QUERY BrwFamily FOR EACH Family NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BrwFamily Family
&Scoped-define FIRST-TABLE-IN-QUERY-BrwFamily Family


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Definitions for QUERY QUERY-3                                        */
&Scoped-define SELF-NAME QUERY-3
&Scoped-define QUERY-STRING-QUERY-3 FOR EACH Customer NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-QUERY-3 OPEN QUERY {&SELF-NAME} FOR EACH Customer NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-QUERY-3 Customer
&Scoped-define FIRST-TABLE-IN-QUERY-QUERY-3 Customer


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tbDepartment tbFamily new_tbDepartment ~
edit_tbDepartment copy_tbDepartment undo_tbDepartment save_tbDepartment ~
delete_tbDepartment refresh_tbDepartment filter_tbDepartment ~
browseconfig_tbDepartment excel_tbDepartment BrwDepartment DeptCode ~
Position DeptName HomePhone EmpNum PostalCode new_tbFamily edit_tbFamily ~
copy_tbFamily undo_tbFamily save_tbFamily delete_tbFamily refresh_tbFamily ~
filter_tbFamily browseconfig_tbFamily excel_tbFamily BrwFamily 
&Scoped-Define DISPLAYED-OBJECTS DeptCode Position DeptName HomePhone ~
EmpNum PostalCode 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON browseconfig_tbDepartment 
     IMAGE-UP FILE "bmp/table.bmp":U
     LABEL "Button 9" 
     SIZE 4.6 BY 1.1.

DEFINE BUTTON browseconfig_tbFamily 
     IMAGE-UP FILE "bmp/table.bmp":U
     LABEL "Button 19" 
     SIZE 4.6 BY 1.1.

DEFINE BUTTON copy_tbDepartment 
     IMAGE-UP FILE "bmp/copy.bmp":U
     LABEL "Button 3" 
     SIZE 4.6 BY 1.1.

DEFINE BUTTON copy_tbFamily 
     IMAGE-UP FILE "bmp/copy.bmp":U
     LABEL "Button 13" 
     SIZE 4.6 BY 1.1.

DEFINE BUTTON delete_tbDepartment 
     IMAGE-UP FILE "bmp/del16e.bmp":U
     LABEL "Button 6" 
     SIZE 4.6 BY 1.1.

DEFINE BUTTON delete_tbFamily 
     IMAGE-UP FILE "bmp/del16e.bmp":U
     LABEL "Button 16" 
     SIZE 4.6 BY 1.1.

DEFINE BUTTON edit_tbDepartment 
     IMAGE-UP FILE "bmp/edit16e.bmp":U
     LABEL "Button 2" 
     SIZE 4.6 BY 1.1.

DEFINE BUTTON edit_tbFamily 
     IMAGE-UP FILE "bmp/edit16e.bmp":U
     LABEL "Button 12" 
     SIZE 4.6 BY 1.1.

DEFINE BUTTON excel_tbDepartment 
     IMAGE-UP FILE "gif/afexcel.gif":U
     LABEL "Button 10" 
     SIZE 4.6 BY 1.1.

DEFINE BUTTON excel_tbFamily 
     IMAGE-UP FILE "gif/afexcel.gif":U
     LABEL "Button 20" 
     SIZE 4.6 BY 1.1.

DEFINE BUTTON filter_tbDepartment 
     IMAGE-UP FILE "gif/filter.gif":U
     LABEL "Button 8" 
     SIZE 4.6 BY 1.1.

DEFINE BUTTON filter_tbFamily 
     IMAGE-UP FILE "gif/filter.gif":U
     LABEL "Button 18" 
     SIZE 4.6 BY 1.1.

DEFINE BUTTON new_tbDepartment 
     IMAGE-UP FILE "bmp/new16e.bmp":U
     LABEL "Button 1" 
     SIZE 4.6 BY 1.1.

DEFINE BUTTON new_tbFamily 
     IMAGE-UP FILE "bmp/new16e.bmp":U
     LABEL "Button 11" 
     SIZE 4.6 BY 1.1.

DEFINE BUTTON refresh_tbDepartment 
     IMAGE-UP FILE "gif/refresh.gif":U
     LABEL "Button 7" 
     SIZE 4.6 BY 1.1.

DEFINE BUTTON refresh_tbFamily 
     IMAGE-UP FILE "gif/refresh.gif":U
     LABEL "Button 17" 
     SIZE 4.6 BY 1.1.

DEFINE BUTTON save_tbDepartment 
     IMAGE-UP FILE "bmp/save.bmp":U
     LABEL "Button 5" 
     SIZE 4.6 BY 1.1.

DEFINE BUTTON save_tbFamily 
     IMAGE-UP FILE "bmp/save.bmp":U
     LABEL "Button 15" 
     SIZE 4.6 BY 1.1.

DEFINE BUTTON undo_tbDepartment 
     IMAGE-UP FILE "bmp/undo16e.bmp":U
     LABEL "Button 4" 
     SIZE 4.6 BY 1.1.

DEFINE BUTTON undo_tbFamily 
     IMAGE-UP FILE "bmp/undo16e.bmp":U
     LABEL "Button 14" 
     SIZE 4.6 BY 1.1.

DEFINE VARIABLE DeptCode AS CHARACTER FORMAT "x(3)" 
     LABEL "Dept Code" 
     VIEW-AS FILL-IN 
     SIZE 8.6 BY 1 TOOLTIP "Please enter the Dept Code.".

DEFINE VARIABLE DeptName AS CHARACTER FORMAT "x(15)" 
     LABEL "Dept Name" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 TOOLTIP "Please enter the Dept Name".

DEFINE VARIABLE EmpNum AS INTEGER FORMAT "zzzzzzzzz9" INITIAL 0 
     LABEL "Emp No" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 TOOLTIP "Please enter the Emp Num".

DEFINE VARIABLE HomePhone AS CHARACTER FORMAT "x(20)" 
     LABEL "Home Phone" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 TOOLTIP "Please enter the home phone.".

DEFINE VARIABLE Position AS CHARACTER FORMAT "x(20)" 
     LABEL "Position" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 TOOLTIP "Please enter the position.".

DEFINE VARIABLE PostalCode AS CHARACTER FORMAT "x(10)" 
     LABEL "Postal Code" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 TOOLTIP "Please enter the Postal Code.".

DEFINE RECTANGLE tbDepartment
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 136.8 BY 1.29.

DEFINE RECTANGLE tbFamily
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 135.8 BY 1.29.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BrwDepartment FOR 
      Department SCROLLING.

DEFINE QUERY BrwFamily FOR 
      Family SCROLLING.

DEFINE QUERY QUERY-3 FOR 
      Customer SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BrwDepartment
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BrwDepartment C-Win _STRUCTURED
  QUERY BrwDepartment NO-LOCK DISPLAY
      Department.DeptCode COLUMN-LABEL "Dept Code" FORMAT "x(3)":U
      Department.DeptName COLUMN-LABEL "Dept Name" FORMAT "x(15)":U
      Department.EmpNum COLUMN-LABEL "Emp No" FORMAT "zzzzzzzzz9":U
            WIDTH 10.2
      Department.Position COLUMN-LABEL "Position" FORMAT "x(20)":U
      Department.HomePhone COLUMN-LABEL "Home Phone" FORMAT "x(20)":U
      Department.PostalCode COLUMN-LABEL "Postal Code" FORMAT "x(10)":U
      Department.Fullname COLUMN-LABEL "Name" FORMAT "X(40)":U
  ENABLE
      Department.DeptCode HELP "Please enter the Dept Code."
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 112.4 BY 8.57 FIT-LAST-COLUMN.

DEFINE BROWSE BrwFamily
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BrwFamily C-Win _STRUCTURED
  QUERY BrwFamily NO-LOCK DISPLAY
      Family.BenefitDate COLUMN-LABEL "Benefit Date" FORMAT "99/99/9999":U
      Family.Birthdate COLUMN-LABEL "Birthdate" FORMAT "99/99/9999":U
      Family.Relation COLUMN-LABEL "Relation" FORMAT "x(15)":U
      Family.RelativeName COLUMN-LABEL "Relative Name" FORMAT "x(15)":U
            WIDTH 23.2
      Family.CoveredOnBenefits COLUMN-LABEL "Covered On Benefits" FORMAT "yes/no":U
      Family.EmpNum COLUMN-LABEL "Emp No" FORMAT "zzzzzzzzz9":U
  ENABLE
      Family.BenefitDate HELP "Please enter the Date relative added to benefits."
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 100 BY 4.52 ROW-HEIGHT-CHARS .52 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     new_tbDepartment AT ROW 1.33 COL 1.2 WIDGET-ID 4
     edit_tbDepartment AT ROW 1.33 COL 5.8 WIDGET-ID 6
     copy_tbDepartment AT ROW 1.33 COL 10.4 WIDGET-ID 8
     undo_tbDepartment AT ROW 1.33 COL 15 WIDGET-ID 10
     save_tbDepartment AT ROW 1.33 COL 19.6 WIDGET-ID 12
     delete_tbDepartment AT ROW 1.33 COL 24.2 WIDGET-ID 14
     refresh_tbDepartment AT ROW 1.33 COL 28.8 WIDGET-ID 16
     filter_tbDepartment AT ROW 1.33 COL 33.4 WIDGET-ID 18
     browseconfig_tbDepartment AT ROW 1.33 COL 38 WIDGET-ID 20
     excel_tbDepartment AT ROW 1.33 COL 42.6 WIDGET-ID 22
     BrwDepartment AT ROW 3.38 COL 2.6 WIDGET-ID 200
     DeptCode AT ROW 12.43 COL 22 COLON-ALIGNED HELP
          "Please enter the Dept Code."
     Position AT ROW 12.43 COL 54 COLON-ALIGNED HELP
          "Please enter the position."
     DeptName AT ROW 13.43 COL 22 COLON-ALIGNED HELP
          "Please enter the Dept Name"
     HomePhone AT ROW 13.43 COL 54 COLON-ALIGNED HELP
          "Please enter the home phone."
     EmpNum AT ROW 14.43 COL 22 COLON-ALIGNED HELP
          "Please enter the Emp Num"
     PostalCode AT ROW 14.43 COL 54 COLON-ALIGNED HELP
          "Please enter the Postal Code."
     new_tbFamily AT ROW 16.33 COL 2.2 WIDGET-ID 26
     edit_tbFamily AT ROW 16.33 COL 6.8 WIDGET-ID 28
     copy_tbFamily AT ROW 16.33 COL 11.4 WIDGET-ID 30
     undo_tbFamily AT ROW 16.33 COL 16 WIDGET-ID 32
     save_tbFamily AT ROW 16.33 COL 20.6 WIDGET-ID 34
     delete_tbFamily AT ROW 16.33 COL 25.2 WIDGET-ID 36
     refresh_tbFamily AT ROW 16.33 COL 29.8 WIDGET-ID 38
     filter_tbFamily AT ROW 16.33 COL 34.4 WIDGET-ID 40
     browseconfig_tbFamily AT ROW 16.33 COL 39 WIDGET-ID 42
     excel_tbFamily AT ROW 16.33 COL 43.6 WIDGET-ID 44
     BrwFamily AT ROW 18.14 COL 3 WIDGET-ID 300
     tbDepartment AT ROW 1.24 COL 1 WIDGET-ID 2
     tbFamily AT ROW 16.24 COL 2 WIDGET-ID 24
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 137.8 BY 23.19 WIDGET-ID 100.


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
         HEIGHT             = 23.19
         WIDTH              = 137.8
         MAX-HEIGHT         = 23.52
         MAX-WIDTH          = 137.8
         VIRTUAL-HEIGHT     = 23.52
         VIRTUAL-WIDTH      = 137.8
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
/* BROWSE-TAB BrwDepartment excel_tbDepartment DEFAULT-FRAME */
/* BROWSE-TAB BrwFamily excel_tbFamily DEFAULT-FRAME */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 23.19
       FRAME DEFAULT-FRAME:WIDTH            = 137.8.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BrwDepartment
/* Query rebuild information for BROWSE BrwDepartment
     _TblList          = "sports113.Department"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > "_<CALC>"
"Department.DeptCode" "Dept Code" "x(3)" "character" ? ? ? ? ? ? yes "Please enter the Dept Code." no no "10.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > "_<CALC>"
"Department.DeptName" "Dept Name" "x(15)" "character" ? ? ? ? ? ? no "Please enter the Dept Name" no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > "_<CALC>"
"Department.EmpNum" "Emp No" "zzzzzzzzz9" "integer" ? ? ? ? ? ? no "Please enter the Emp Num" no no "10.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > "_<CALC>"
"Department.Position" "Position" "x(20)" "character" ? ? ? ? ? ? no "Please enter the position." no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > "_<CALC>"
"Department.HomePhone" "Home Phone" "x(20)" "character" ? ? ? ? ? ? no "Please enter the home phone." no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > "_<CALC>"
"Department.PostalCode" "Postal Code" "x(10)" "character" ? ? ? ? ? ? no "Please enter the Postal Code." no no "11.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > "_<CALC>"
"Department.Fullname" "Name" "X(40)" "CHARACTER" ? ? ? ? ? ? no "" no no "16.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BrwDepartment */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BrwFamily
/* Query rebuild information for BROWSE BrwFamily
     _TblList          = "sports113.Family"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > "_<CALC>"
"Family.BenefitDate" "Benefit Date" "99/99/9999" "date" ? ? ? ? ? ? yes "Please enter the Date relative added to benefits." no no "11.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > "_<CALC>"
"Family.Birthdate" "Birthdate" "99/99/9999" "date" ? ? ? ? ? ? no "Please enter a Birthdate" no no "11.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > "_<CALC>"
"Family.Relation" "Relation" "x(15)" "character" ? ? ? ? ? ? no "Please enter Spouse or Child." no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > "_<CALC>"
"Family.RelativeName" "Relative Name" "x(15)" "character" ? ? ? ? ? ? no "Please enter the Relative Name" no no "23.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > "_<CALC>"
"Family.CoveredOnBenefits" "Covered On Benefits" "yes/no" "logical" ? ? ? ? ? ? no "Please enter the Covered on Benefits field." no no "19.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > "_<CALC>"
"Family.EmpNum" "Emp No" "zzzzzzzzz9" "integer" ? ? ? ? ? ? no "Please enter the Emp Num." no no "10.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BrwFamily */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK QUERY QUERY-3
/* Query rebuild information for QUERY QUERY-3
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Customer NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Design-Parent    is FRAME DEFAULT-FRAME @ ( 13.38 , 111 )
*/  /* QUERY QUERY-3 */
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


&Scoped-define SELF-NAME QUERY-3
&Scoped-define BROWSE-NAME BrwDepartment
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplayRecord C-Win 
PROCEDURE DisplayRecord :
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
  DISPLAY DeptCode Position DeptName HomePhone EmpNum PostalCode 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE tbDepartment tbFamily new_tbDepartment edit_tbDepartment 
         copy_tbDepartment undo_tbDepartment save_tbDepartment 
         delete_tbDepartment refresh_tbDepartment filter_tbDepartment 
         browseconfig_tbDepartment excel_tbDepartment BrwDepartment DeptCode 
         Position DeptName HomePhone EmpNum PostalCode new_tbFamily 
         edit_tbFamily copy_tbFamily undo_tbFamily save_tbFamily 
         delete_tbFamily refresh_tbFamily filter_tbFamily browseconfig_tbFamily 
         excel_tbFamily BrwFamily 
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

  oBrwDepartment = NEW JBoxBrowse(brwDepartment:HANDLE).

  oFmDepartment = NEW JBoxFieldMap(FRAME {&FRAME-NAME}:HANDLE).
  oFmDepartment:updateFields = 'DeptCode,DeptName'.
  oFmDepartment:displayFields = 'EmpNum,Position,HomePhone,PostalCode'.
  oFmDepartment:primaryKeyFields = 'DeptCode'.

  oFmDepartment:BROWSE-OBJECT = oBrwDepartment.
 
  oBrwFamily = NEW JBoxBrowse(brwFamily:HANDLE).
    
  oTbDepartment = NEW JBoxToolbar(tbDepartment:HANDLE,'Fil').

  oBrwDepartment:TOOLBAR-OBJECT = oTbDepartment.
  oFmDepartment:TOOLBAR-OBJECT = oTbDepartment.
  oTbFamily = NEW JBoxToolbar(tbFamily:HANDLE,'Fil').

  oBrwFamily:TOOLBAR-OBJECT = oTbFamily.
  
  oQryCustomer = NEW JBoxQuery('Customer').

END.
oBrwFamily:OpenQuery().
oBrwDepartment:OpenQuery().


DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,200,200,0,0).

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
    WHEN "EmpNum" THEN DO: 
      cReturn = DYNAMIC-FUNCTION("getFieldValues","Employee","WHERE EmpNum = '" + ihField:SCREEN-VALUE + "'","LastName,FirstName").
/*      IF cReturn NE ? THEN                                                         */
/*        Fullname:SCREEN-VALUE = ENTRY(1,cReturn,"|") + ", " + ENTRY(2,cReturn,"|").*/
/*      ELSE                                                                         */
/*        Fullname:SCREEN-VALUE = ?.                                                 */
    END.
        
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

