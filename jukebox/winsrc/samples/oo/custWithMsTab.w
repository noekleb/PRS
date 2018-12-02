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


/*** Start instance property definitions for JBoxBrowse object oBrwCustomer ***/
DEF VAR oBrwCustomer AS JBoxBrowse NO-UNDO.
DEF TEMP-TABLE Customer
    FIELD CustNum AS integer
    FIELD Name AS character
    FIELD RowIdent1 AS CHARACTER 
    FIELD RowCount AS INTEGER
    FIELD jbCountDistinct AS INTEGER FORMAT '>>>,>>>,>>9' INIT 1
    FIELD jbAverage AS DECIMAL FORMAT '->>>,>>>,>>9.99'
    INDEX idxRowids  RowIdent1
    .
DEF BUFFER v_Customer FOR TEMP-TABLE Customer.


FUNCTION getBuffersAndFieldsBrwCustomer RETURNS CHARACTER():
  RETURN
    'Customer'
     + ';CustNum'
     + ';Name'
     .
END FUNCTION.
FUNCTION getQueryJoinBrwCustomer RETURNS CHARACTER():
  RETURN ''.
END FUNCTION.
DEF VAR oTabCustomer AS JBoxMsTabs NO-UNDO.


DEF VAR otbCustomer AS JBoxToolbar NO-UNDO.

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
&Scoped-define FIELDS-IN-QUERY-BrwCustomer Customer.CustNum Customer.Name 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BrwCustomer Customer.CustNum 
&Scoped-define ENABLED-TABLES-IN-QUERY-BrwCustomer Customer
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BrwCustomer Customer
&Scoped-define QUERY-STRING-BrwCustomer FOR EACH Customer NO-LOCK, INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BrwCustomer OPEN QUERY BrwCustomer FOR EACH Customer NO-LOCK, INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BrwCustomer Customer
&Scoped-define FIRST-TABLE-IN-QUERY-BrwCustomer Customer


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS TabCustomer searchCustomer tbCustomer ~
new_tbCustomer edit_tbCustomer copy_tbCustomer undo_tbCustomer ~
save_tbCustomer delete_tbCustomer refresh_tbCustomer filter_tbCustomer ~
browseconfig_tbCustomer excel_tbCustomer BrwCustomer 

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
     LABEL "Column setup" 
     SIZE 6 BY 1.52 TOOLTIP "Column setup (ALT-C)".

DEFINE BUTTON copy_tbCustomer 
     IMAGE-UP FILE "bmp/copy.bmp":U
     LABEL "Kopier" 
     SIZE 6 BY 1.52 TOOLTIP "Kopier (ALT-K)".

DEFINE BUTTON delete_tbCustomer 
     IMAGE-UP FILE "bmp/del16e.bmp":U
     LABEL "Slett" 
     SIZE 6 BY 1.52 TOOLTIP "Slett (CTRL-D)".

DEFINE BUTTON edit_tbCustomer 
     IMAGE-UP FILE "bmp/edit16e.bmp":U
     LABEL "Edit" 
     SIZE 6 BY 1.52 TOOLTIP "Edit (CTRL-E)".

DEFINE BUTTON excel_tbCustomer 
     IMAGE-UP FILE "gif/afexcel.gif":U
     LABEL "Eksporter til E&xcel" 
     SIZE 6 BY 1.52 TOOLTIP "Eksporter til E&xcel (ALT-X)".

DEFINE BUTTON filter_tbCustomer 
     IMAGE-UP FILE "gif/filter.gif":U
     LABEL "Filter" 
     SIZE 6 BY 1.52 TOOLTIP "Filter (CTRL-F)".

DEFINE BUTTON new_tbCustomer 
     IMAGE-UP FILE "bmp/new16e.bmp":U
     LABEL "Ny" 
     SIZE 6 BY 1.52 TOOLTIP "Ny (CTRL-N)".

DEFINE BUTTON refresh_tbCustomer 
     IMAGE-UP FILE "gif/refresh.gif":U
     LABEL "Refresh" 
     SIZE 6 BY 1.52 TOOLTIP "Refresh (F5)".

DEFINE BUTTON save_tbCustomer 
     IMAGE-UP FILE "bmp/save.bmp":U
     LABEL "Lagre" 
     SIZE 6 BY 1.52 TOOLTIP "Lagre (CTRL-S)".

DEFINE BUTTON undo_tbCustomer 
     IMAGE-UP FILE "bmp/undo16e.bmp":U
     LABEL "Angre" 
     SIZE 6 BY 1.52 TOOLTIP "Angre (CTRL-Z)".

DEFINE RECTANGLE searchCustomer
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 16 BY .91.

DEFINE RECTANGLE TabCustomer
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 106 BY 9.76.

DEFINE RECTANGLE tbCustomer
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 105.2 BY 1.71.

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
      Customer.Name COLUMN-LABEL "Name" FORMAT "x(30)":U WIDTH 91.4
  ENABLE
      Customer.CustNum HELP "Please enter a customer number."
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 106 BY 4.52 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     new_tbCustomer AT ROW 1.14 COL 2.2 WIDGET-ID 8
     edit_tbCustomer AT ROW 1.14 COL 8.4 WIDGET-ID 10
     copy_tbCustomer AT ROW 1.14 COL 14.4 WIDGET-ID 12
     undo_tbCustomer AT ROW 1.14 COL 20.4 WIDGET-ID 14
     save_tbCustomer AT ROW 1.14 COL 26.4 WIDGET-ID 16
     delete_tbCustomer AT ROW 1.14 COL 32.4 WIDGET-ID 18
     refresh_tbCustomer AT ROW 1.14 COL 38.4 WIDGET-ID 20
     filter_tbCustomer AT ROW 1.14 COL 44.4 WIDGET-ID 22
     browseconfig_tbCustomer AT ROW 1.14 COL 50.4 WIDGET-ID 24
     excel_tbCustomer AT ROW 1.14 COL 56.4 WIDGET-ID 26
     BrwCustomer AT ROW 4.29 COL 2 WIDGET-ID 200
     TabCustomer AT ROW 9.1 COL 2 WIDGET-ID 2
     searchCustomer AT ROW 3.1 COL 2 WIDGET-ID 4
     tbCustomer AT ROW 1.05 COL 2 WIDGET-ID 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 108.2 BY 18.1 WIDGET-ID 100.


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
         HEIGHT             = 18.1
         WIDTH              = 108.2
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
       FRAME DEFAULT-FRAME:HEIGHT           = 18.1
       FRAME DEFAULT-FRAME:WIDTH            = 108.2.

ASSIGN 
       tbCustomer:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "new;Ny,edit;Edit,copy;Kopier,undo;Angre,save;Lagre,delete;Slett,refresh;Refresh,filter;Filter,browseconfig;Column setup,excel;Eksporter til E&xcelmaxborder".

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
"Customer.Name" "Name" "x(30)" "character" ? ? ? ? ? ? no "Please enter a name." no no "91.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
{incl/conttrigg.i oBrwCustomer:BROWSE-HANDLE}

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
  ENABLE TabCustomer searchCustomer tbCustomer new_tbCustomer edit_tbCustomer 
         copy_tbCustomer undo_tbCustomer save_tbCustomer delete_tbCustomer 
         refresh_tbCustomer filter_tbCustomer browseconfig_tbCustomer 
         excel_tbCustomer BrwCustomer 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeComponents C-Win 
PROCEDURE InitializeComponents :
DO WITH FRAME {&FRAME-NAME}:


  oBrwCustomer:setNoResizeY().
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

  oTabCustomer = NEW JBoxMsTabs(TabCustomer:HANDLE,oBrwCustomer).
  oTabCustomer:pageOneType = "child".

  oTabCustomer:setLinkFields("CustNum").
  oTabCustomer:AddPage("Orders","CustOrderGoo.w").
oBrwCustomer:setSearchField(searchCustomer:HANDLE,"CustNum").
  otbCustomer = NEW JBoxToolbar(tbCustomer:HANDLE).

  oBrwCustomer:TOOLBAR-OBJECT = otbCustomer.
  RUN InitializeComponents.
END.
oBrwCustomer:OpenQuery().


oContainer:initResize().

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

