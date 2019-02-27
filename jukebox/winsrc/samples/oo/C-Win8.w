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

/*USING oo.* FROM PROPATH.*/

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


/*** Start instance property definitions for JBoxBrowse object oBrwEmployee ***/
DEF VAR oBrwEmployee AS JBoxBrowse NO-UNDO.
DEF TEMP-TABLE Employee
    FIELD Address AS character
    FIELD Address2 AS character
    FIELD Birthdate AS date
    FIELD City AS character
    FIELD EmpNum AS integer
    FIELD RowIdent1 AS CHARACTER 
    FIELD RowCount AS INTEGER
    FIELD jbCountDistinct AS INTEGER FORMAT '>>>,>>>,>>9' INIT 1
    FIELD jbAverage AS DECIMAL FORMAT '->>>,>>>,>>9.99'
    INDEX idxRowids  RowIdent1
    .
DEF BUFFER v_Employee FOR Employee.


FUNCTION getBuffersAndFieldsBrwEmployee RETURNS CHARACTER():
  RETURN
    'Employee'
     + ';Address'
     + ';Address2'
     + ';Birthdate'
     + ';City'
     + ';EmpNum'
     .
END FUNCTION.
FUNCTION getQueryJoinBrwEmployee RETURNS CHARACTER():
  RETURN ''.
END FUNCTION.


DEF VAR oTbEmployee AS JBoxToolbar NO-UNDO.


DEF VAR oFmEmployee AS JBoxFieldMap NO-UNDO.


/*** Start instance property definitions for JBoxBrowse object oBrwFamily ***/
DEF VAR oBrwFamily AS JBoxBrowse NO-UNDO.
DEF TEMP-TABLE Family
    FIELD BenefitDate AS date
    FIELD Birthdate AS date
    FIELD CoveredOnBenefits AS logical
    FIELD EmpNum AS integer
    FIELD Relation AS character
    FIELD RelativeName AS character
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
     + ';CoveredOnBenefits'
     + ';EmpNum'
     + ';Relation'
     + ';RelativeName'
     .
END FUNCTION.
FUNCTION getQueryJoinBrwFamily RETURNS CHARACTER():
  RETURN ''.
END FUNCTION.


DEF VAR opopupEmployee AS JBoxPopupMenu NO-UNDO.

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
&Scoped-define INTERNAL-TABLES Employee Family

/* Definitions for BROWSE BrwEmployee                                   */
&Scoped-define FIELDS-IN-QUERY-BrwEmployee Employee.Address ~
Employee.Address2 Employee.Birthdate Employee.City Employee.EmpNum 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BrwEmployee Employee.Address 
&Scoped-define QUERY-STRING-BrwEmployee FOR EACH Employee NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BrwEmployee OPEN QUERY BrwEmployee FOR EACH Employee NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BrwEmployee Employee
&Scoped-define FIRST-TABLE-IN-QUERY-BrwEmployee Employee


/* Definitions for BROWSE BrwFamily                                     */
&Scoped-define FIELDS-IN-QUERY-BrwFamily Family.BenefitDate ~
Family.Birthdate Family.CoveredOnBenefits Family.EmpNum Family.Relation ~
Family.RelativeName 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BrwFamily Family.BenefitDate 
&Scoped-define QUERY-STRING-BrwFamily FOR EACH Family NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BrwFamily OPEN QUERY BrwFamily FOR EACH Family NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BrwFamily Family
&Scoped-define FIRST-TABLE-IN-QUERY-BrwFamily Family


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tbEmployee new_tbEmployee edit_tbEmployee ~
copy_tbEmployee undo_tbEmployee save_tbEmployee delete_tbEmployee ~
test_tbEmployee test2_tbEmployee word_tbEmployee excel_tbEmployee ~
close_tbEmployee help_tbEmployee www_tbEmployee email_tbEmployee ~
activate_tbEmployee accum_tbEmployee moveup_tbEmployee movedown_tbEmployee ~
prev_tbEmployee first_tbEmployee next_tbEmployee last_tbEmployee ~
BrwEmployee BrwFamily Address Address2 Birthdate City 
&Scoped-Define DISPLAYED-OBJECTS Address Address2 Birthdate City 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON accum_tbEmployee 
     IMAGE-UP FILE "gif/statusu.gif":U
     LABEL "Accumulate" 
     SIZE 6 BY 1.52 TOOLTIP "Accumulate (ALT-A)".

DEFINE BUTTON activate_tbEmployee 
     IMAGE-UP FILE "gif/active.gif":U
     LABEL "Activate" 
     SIZE 6 BY 1.52 TOOLTIP "Activate (ALT-A)".

DEFINE BUTTON close_tbEmployee 
     IMAGE-UP FILE "bmp/e-exit.bmp":U
     LABEL "Close" 
     SIZE 6 BY 1.52 TOOLTIP "Close (ALT-C)".

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

DEFINE BUTTON email_tbEmployee 
     IMAGE-UP FILE "gif/msngrWindow.gif":U
     LABEL "Email" 
     SIZE 6 BY 1.52 TOOLTIP "Email (ALT-E)".

DEFINE BUTTON excel_tbEmployee 
     IMAGE-UP FILE "gif/afexcel.gif":U
     LABEL "Eksporter til E&xcel" 
     SIZE 6 BY 1.52 TOOLTIP "Eksporter til E&xcel (ALT-X)".

DEFINE BUTTON first_tbEmployee 
     IMAGE-UP FILE "bmp/first.bmp":U
     LABEL "First" 
     SIZE 6 BY 1.52 TOOLTIP "First (ALT-F)".

DEFINE BUTTON help_tbEmployee 
     IMAGE-UP FILE "bmp/e-help.bmp":U
     LABEL "Help" 
     SIZE 6 BY 1.52 TOOLTIP "Help (ALT-H)".

DEFINE BUTTON last_tbEmployee 
     IMAGE-UP FILE "bmp/last.bmp":U
     LABEL "Last" 
     SIZE 6 BY 1.52 TOOLTIP "Last (ALT-L)".

DEFINE BUTTON movedown_tbEmployee 
     IMAGE-UP FILE "gif/movedown.gif":U
     LABEL "Move Down" 
     SIZE 6 BY 1.52 TOOLTIP "Move Down (ALT-M)".

DEFINE BUTTON moveup_tbEmployee 
     IMAGE-UP FILE "gif/moveup.gif":U
     LABEL "Move Up" 
     SIZE 6 BY 1.52 TOOLTIP "Move Up (ALT-M)".

DEFINE BUTTON new_tbEmployee 
     IMAGE-UP FILE "bmp/new16e.bmp":U
     LABEL "Ny" 
     SIZE 6 BY 1.52 TOOLTIP "Ny (CTRL-N)".

DEFINE BUTTON next_tbEmployee 
     IMAGE-UP FILE "bmp/next.bmp":U
     LABEL "Next" 
     SIZE 6 BY 1.52 TOOLTIP "Next (ALT-N)".

DEFINE BUTTON prev_tbEmployee 
     IMAGE-UP FILE "bmp/prev.bmp":U
     LABEL "Prev" 
     SIZE 6 BY 1.52 TOOLTIP "Prev (ALT-P)".

DEFINE BUTTON save_tbEmployee 
     IMAGE-UP FILE "bmp/save.bmp":U
     LABEL "Lagre" 
     SIZE 6 BY 1.52 TOOLTIP "Lagre (CTRL-S)".

DEFINE BUTTON test2_tbEmployee 
     LABEL "test2" 
     SIZE 6 BY 1.52.

DEFINE BUTTON test_tbEmployee 
     LABEL "Kunde" 
     SIZE 8.2 BY 1.52.

DEFINE BUTTON undo_tbEmployee 
     IMAGE-UP FILE "bmp/undo16e.bmp":U
     LABEL "Angre" 
     SIZE 6 BY 1.52 TOOLTIP "Angre (CTRL-Z)".

DEFINE BUTTON word_tbEmployee 
     IMAGE-UP FILE "gif/afword.gif":U
     LABEL "Word" 
     SIZE 6 BY 1.52 TOOLTIP "Word (ALT-W)".

DEFINE BUTTON www_tbEmployee 
     IMAGE-UP FILE "gif/afinternet.gif":U
     LABEL "www" 
     SIZE 6 BY 1.52 TOOLTIP "www (ALT-W)".

DEFINE VARIABLE Address AS CHARACTER FORMAT "x(35)" 
     LABEL "Address" 
     VIEW-AS FILL-IN 
     SIZE 37 BY 1.

DEFINE VARIABLE Address2 AS CHARACTER FORMAT "x(35)" 
     LABEL "Address2" 
     VIEW-AS FILL-IN 
     SIZE 37 BY 1.

DEFINE VARIABLE Birthdate AS DATE FORMAT "99/99/9999" 
     LABEL "Birthdate" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE City AS CHARACTER FORMAT "x(25)" 
     LABEL "City" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1.

DEFINE RECTANGLE tbEmployee
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 136 BY 1.71.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BrwEmployee FOR 
      Employee SCROLLING.

DEFINE QUERY BrwFamily FOR 
      Family SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BrwEmployee
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BrwEmployee C-Win _STRUCTURED
  QUERY BrwEmployee NO-LOCK DISPLAY
      Employee.Address COLUMN-LABEL "Address" FORMAT "x(35)":U
      Employee.Address2 COLUMN-LABEL "Address2" FORMAT "x(35)":U
      Employee.Birthdate COLUMN-LABEL "Birthdate" FORMAT "99/99/9999":U
      Employee.City COLUMN-LABEL "City" FORMAT "x(25)":U WIDTH 45.4
      Employee.EmpNum COLUMN-LABEL "Emp No" FORMAT "zzzzzzzzz9":U
  ENABLE
      Employee.Address HELP "Please enter the address"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 136 BY 10.71 FIT-LAST-COLUMN.

DEFINE BROWSE BrwFamily
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BrwFamily C-Win _STRUCTURED
  QUERY BrwFamily NO-LOCK DISPLAY
      Family.BenefitDate COLUMN-LABEL "Benefit Date" FORMAT "99/99/9999":U
      Family.Birthdate COLUMN-LABEL "Birthdate" FORMAT "99/99/9999":U
      Family.CoveredOnBenefits COLUMN-LABEL "Covered On Benefits" FORMAT "yes/no":U
      Family.EmpNum COLUMN-LABEL "Emp No" FORMAT "zzzzzzzzz9":U
            WIDTH 10.2
      Family.Relation COLUMN-LABEL "Relation" FORMAT "x(15)":U
      Family.RelativeName COLUMN-LABEL "Relative Name" FORMAT "x(15)":U
  ENABLE
      Family.BenefitDate HELP "Please enter the Date relative added to benefits."
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 84 BY 6.19 ROW-HEIGHT-CHARS .67 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     new_tbEmployee AT ROW 1.33 COL 2.6 WIDGET-ID 58
     edit_tbEmployee AT ROW 1.33 COL 8.8 WIDGET-ID 106
     copy_tbEmployee AT ROW 1.33 COL 14.8 WIDGET-ID 114
     undo_tbEmployee AT ROW 1.33 COL 20.8 WIDGET-ID 62
     save_tbEmployee AT ROW 1.33 COL 26.8 WIDGET-ID 66
     delete_tbEmployee AT ROW 1.33 COL 32.8 WIDGET-ID 64
     test_tbEmployee AT ROW 1.33 COL 38.8 WIDGET-ID 110
     test2_tbEmployee AT ROW 1.33 COL 47 WIDGET-ID 112
     word_tbEmployee AT ROW 1.33 COL 53 WIDGET-ID 82
     excel_tbEmployee AT ROW 1.33 COL 59 WIDGET-ID 108
     close_tbEmployee AT ROW 1.33 COL 65 WIDGET-ID 78
     help_tbEmployee AT ROW 1.33 COL 71 WIDGET-ID 80
     www_tbEmployee AT ROW 1.33 COL 77 WIDGET-ID 84
     email_tbEmployee AT ROW 1.33 COL 83 WIDGET-ID 86
     activate_tbEmployee AT ROW 1.33 COL 89 WIDGET-ID 92
     accum_tbEmployee AT ROW 1.33 COL 95 WIDGET-ID 94
     moveup_tbEmployee AT ROW 1.33 COL 101 WIDGET-ID 96
     movedown_tbEmployee AT ROW 1.33 COL 107 WIDGET-ID 98
     prev_tbEmployee AT ROW 1.33 COL 113 WIDGET-ID 70
     first_tbEmployee AT ROW 1.33 COL 119 WIDGET-ID 68
     next_tbEmployee AT ROW 1.33 COL 125 WIDGET-ID 72
     last_tbEmployee AT ROW 1.33 COL 131 WIDGET-ID 74
     BrwEmployee AT ROW 3.14 COL 2 WIDGET-ID 200
     BrwFamily AT ROW 14.19 COL 51 WIDGET-ID 300
     Address AT ROW 14.67 COL 10 COLON-ALIGNED
     Address2 AT ROW 15.67 COL 10 COLON-ALIGNED
     Birthdate AT ROW 16.67 COL 10 COLON-ALIGNED
     City AT ROW 17.67 COL 10 COLON-ALIGNED
     tbEmployee AT ROW 1.24 COL 2 WIDGET-ID 56
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 138.2 BY 19.67 WIDGET-ID 100.


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
         HEIGHT             = 19.67
         WIDTH              = 138.2
         MAX-HEIGHT         = 23.52
         MAX-WIDTH          = 150
         VIRTUAL-HEIGHT     = 23.52
         VIRTUAL-WIDTH      = 150
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
/* BROWSE-TAB BrwEmployee last_tbEmployee DEFAULT-FRAME */
/* BROWSE-TAB BrwFamily BrwEmployee DEFAULT-FRAME */
ASSIGN 
       BrwFamily:PRIVATE-DATA IN FRAME DEFAULT-FRAME           = 
                "Krediter;Krediter post¤enable".

ASSIGN 
       tbEmployee:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "new|File;Ny,edit|File;Edit,copy|File;Kopier,undo|File;Angre,save|File;Lagre,delete|File;Slett,test|File;Kunde,test2|File;test2,word|File;Word,excel|File;Eksporter til E&xcel,close|File;Close,help|File;Help,www|File;www,email|File;Email,activate|File;Activate,accum|File;Accumulate,moveup|File;Move Up,movedown|File;Move Down,prev|File;Prev,first|File;First,next|File;Next,last|File;LastFilemaxborder".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BrwEmployee
/* Query rebuild information for BROWSE BrwEmployee
     _TblList          = "sports116.Employee"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > "_<CALC>"
"Employee.Address" "Address" "x(35)" "character" ? ? ? ? ? ? yes "Please enter the address" no no "35" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > "_<CALC>"
"Employee.Address2" "Address2" "x(35)" "character" ? ? ? ? ? ? no "Please enter the address." no no "35" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > "_<CALC>"
"Employee.Birthdate" "Birthdate" "99/99/9999" "date" ? ? ? ? ? ? no "Please enter the birth date." no no "11.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > "_<CALC>"
"Employee.City" "City" "x(25)" "character" ? ? ? ? ? ? no "Please enter the city." no no "45.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > "_<CALC>"
"Employee.EmpNum" "Emp No" "zzzzzzzzz9" "integer" ? ? ? ? ? ? no "Please enter the Emp Num" no no "1.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BrwEmployee */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BrwFamily
/* Query rebuild information for BROWSE BrwFamily
     _TblList          = "sports116.Family"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > "_<CALC>"
"Family.BenefitDate" "Benefit Date" "99/99/9999" "date" ? ? ? ? ? ? yes "Please enter the Date relative added to benefits." no no "11.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > "_<CALC>"
"Family.Birthdate" "Birthdate" "99/99/9999" "date" ? ? ? ? ? ? no "Please enter a Birthdate" no no "11.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > "_<CALC>"
"Family.CoveredOnBenefits" "Covered On Benefits" "yes/no" "logical" ? ? ? ? ? ? no "Please enter the Covered on Benefits field." no no "19.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > "_<CALC>"
"Family.EmpNum" "Emp No" "zzzzzzzzz9" "integer" ? ? ? ? ? ? no "Please enter the Emp Num." no no "10.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > "_<CALC>"
"Family.Relation" "Relation" "x(15)" "character" ? ? ? ? ? ? no "Please enter Spouse or Child." no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > "_<CALC>"
"Family.RelativeName" "Relative Name" "x(15)" "character" ? ? ? ? ? ? no "Please enter the Relative Name" no no "7.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BrwFamily */
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE acceptRecord C-Win 
PROCEDURE acceptRecord :
RUN SUPER.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE accumRecord C-Win 
PROCEDURE accumRecord :
RUN SUPER.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE activateRecord C-Win 
PROCEDURE activateRecord :
RUN SUPER.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE closeRecord C-Win 
PROCEDURE closeRecord :
RUN SUPER.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE colorRecord C-Win 
PROCEDURE colorRecord :
RUN SUPER.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE commitRecord C-Win 
PROCEDURE commitRecord :
RUN SUPER.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copyRecord C-Win 
PROCEDURE copyRecord :
MESSAGE PROGRAM-NAME(1) VIEW-AS ALERT-BOX.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE documentsRecord C-Win 
PROCEDURE documentsRecord :
RUN SUPER.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE editRecord C-Win 
PROCEDURE editRecord :
MESSAGE PROGRAM-NAME(1) VIEW-AS ALERT-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE emailRecord C-Win 
PROCEDURE emailRecord :
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
  DISPLAY Address Address2 Birthdate City 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE tbEmployee new_tbEmployee edit_tbEmployee copy_tbEmployee 
         undo_tbEmployee save_tbEmployee delete_tbEmployee test_tbEmployee 
         test2_tbEmployee word_tbEmployee excel_tbEmployee close_tbEmployee 
         help_tbEmployee www_tbEmployee email_tbEmployee activate_tbEmployee 
         accum_tbEmployee moveup_tbEmployee movedown_tbEmployee prev_tbEmployee 
         first_tbEmployee next_tbEmployee last_tbEmployee BrwEmployee BrwFamily 
         Address Address2 Birthdate City 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GreppRecord C-Win 
PROCEDURE GreppRecord :
RUN SUPER.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE helpRecord C-Win 
PROCEDURE helpRecord :
RUN SUPER.


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
DEF VAR oContainer AS JBoxContainer NO-UNDO.

RUN enable_UI.

oContainer = NEW JBoxContainer().
oContainer:addStatusBar().

DO WITH FRAME {&FRAME-NAME}:

  oBrwEmployee = NEW JBoxBrowse(brwEmployee:HANDLE).
/*  oBrwEmployee:enabledColumns = "*".  */
/*  oBrwEmployee:enableOnDblClick = YES.*/
    
  oTbEmployee = NEW JBoxToolbar(tbEmployee:HANDLE).
  
  oBrwEmployee:TOOLBAR-OBJECT = oTbEmployee.
  oFmEmployee = NEW JBoxFieldMap(FRAME {&FRAME-NAME}:HANDLE).
  oFmEmployee:updateFields = 'Address,Address2,Birthdate,City'.

  oFmEmployee:BROWSE-OBJECT = oBrwEmployee.
  oBrwFamily = NEW JBoxBrowse(brwFamily:HANDLE).
 
  oBrwFamily:setParentBrowseObject(oBrwEmployee,"EmpNum").
  oBrwFamily:setNoResizeY().
  opopupEmployee = NEW JBoxPopupMenu().
  opopupEmployee:AddToolGroup('Krediter;Krediter post¤enable').

  oBrwEmployee:POPUP-MENU-OBJECT = opopupEmployee.
END.
oBrwEmployee:OpenQuery().


oContainer:initResize(900,100).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE insertRecord C-Win 
PROCEDURE insertRecord :
RUN SUPER.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KrediterRecord C-Win 
PROCEDURE KrediterRecord :
RUN SUPER.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE movedownRecord C-Win 
PROCEDURE movedownRecord :
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE moveupRecord C-Win 
PROCEDURE moveupRecord :
RUN SUPER.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE newRecord C-Win 
PROCEDURE newRecord :
/*
/* Either simple dynamic prompt (abhack: afv) */
RUN JBoxAskForValue.w ("<message>","<datatype>|<format>|<opt:init.val>",INPUT-OUTPUT <char_var>,OUTPUT iReturn).
IF iReturn NE 2 THEN RETURN.

/* Or select from a list (abhack: sis) */
RUN JBoxDSimpleSelectList.w ("<desc>|<value>|<desc>|<value>..",<opt posWidgetHld>,OUTPUT cReturn).
IF cReturn = ? THEN RETURN.

/* Or your own prompt.. */

/* Then create the record: */
RUN SUPER.

/* And update it with your prompted value(s) before you refresh the row */

/* To invoke a postUpdate procedure for this update:
DYNAMIC-FUNCTION("setPostUpdProc","mypostupdateproc.p").
*/

IF DYNAMIC-FUNCTION("DoUpdate",
        hBrowse:QUERY:GET-BUFFER-HANDLE(1):NAME,
        "Ignore",/* no (dynamic) validation. Could also be f.ex "=mycustomvalproc.p" */
        "", /* no key field - use the RowIdent1 */
        hBrowse:QUERY:GET-BUFFER-HANDLE(1)::RowIdent1,
        "field1,field2,..",
        <value1>|<value2>|..,  
        YES) /* Commit */ THEN
  DYNAMIC-FUNCTION("RefreshRowids",hBrowse,hBrowse:QUERY:GET-BUFFER-HANDLE(1)::RowIdent1).

*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rollbackRecord C-Win 
PROCEDURE rollbackRecord :
RUN SUPER.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE test2Record C-Win 
PROCEDURE test2Record :
JBoxMainMenu:Instance:StartTabWindow("Customer.w","Kundetab",THIS-PROCEDURE,yes,yes).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE testRecord C-Win 
PROCEDURE testRecord :
JBoxMainMenu:Instance:StartChildTab("Customer.w").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE undoRecord C-Win 
PROCEDURE undoRecord :
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE wordRecord C-Win 
PROCEDURE wordRecord :
RUN SUPER.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE wwwRecord C-Win 
PROCEDURE wwwRecord :
RUN SUPER.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

