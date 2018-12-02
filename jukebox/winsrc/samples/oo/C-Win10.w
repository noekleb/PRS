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


/*** Start instance property definitions for JBoxBrowse object oBrwCustomer ***/
DEF VAR oBrwCustomer AS JBoxBrowse NO-UNDO.

DEF TEMP-TABLE Customer
    FIELD Contact AS character
    FIELD Country AS character
    FIELD CreditLimit AS decimal
    FIELD CustNum AS integer
    FIELD Discount AS integer
    FIELD EmailAddress AS character
    FIELD Fax AS character
    FIELD RowIdent1 AS CHARACTER 
    FIELD RowCount AS INTEGER
    FIELD jbCountDistinct AS INTEGER FORMAT '>>>,>>>,>>9' INIT 1
    FIELD jbAverage AS DECIMAL FORMAT '->>>,>>>,>>9.99'
    INDEX idxRowids  RowIdent1
    .
DEFINE BUFFER v_Customer FOR Customer.

FUNCTION getBuffersAndFieldsBrwCustomer RETURNS CHARACTER().
  RETURN 
      'Customer'
      + ';Contact'
      + ';Country'
      + ';CreditLimit'
      + ';CustNum'
      + ';Discount'
      + ';EmailAddress'
      + ';Fax'.
END FUNCTION.
FUNCTION getQueryJoinBrwCustomer RETURNS CHARACTER().
  RETURN ''.
END FUNCTION.
/*** End instance property settings for JBoxBrowse object oBrwCustomer ***/

DEF VAR otbCustomer AS JBoxToolbar NO-UNDO.


DEF VAR oJBoxFpSpread AS JBoxFpSpread NO-UNDO.


DEF VAR opopupCustomer AS JBoxPopupMenu NO-UNDO.


DEF VAR oViewCustomer AS JBoxViewer NO-UNDO.


DEF VAR oJBoxMsTreeView AS uc.JBoxMsTreeView NO-UNDO.

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
&Scoped-define FIELDS-IN-QUERY-BrwCustomer Customer.Contact Customer.Country Customer.CreditLimit Customer.CustNum Customer.Discount Customer.EmailAddress Customer.Fax   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BrwCustomer Customer.Contact   
&Scoped-define ENABLED-TABLES-IN-QUERY-BrwCustomer Customer
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BrwCustomer Customer
&Scoped-define SELF-NAME BrwCustomer
&Scoped-define QUERY-STRING-BrwCustomer FOR EACH Customer NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BrwCustomer OPEN QUERY {&SELF-NAME} FOR EACH Customer NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BrwCustomer Customer
&Scoped-define FIRST-TABLE-IN-QUERY-BrwCustomer Customer


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tbCustomer JBoxFpSpread ViewCustomer ~
new_tbCustomer copy_tbCustomer undo_tbCustomer delete_tbCustomer ~
save_tbCustomer excel_tbCustomer JBoxMsTreeView BrwCustomer 
&Scoped-Define DISPLAYED-OBJECTS JBoxMsTreeView 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON copy_tbCustomer 
     IMAGE-UP FILE "bmp/copy.bmp":U
     LABEL "Kopier" 
     SIZE 6 BY 1.52 TOOLTIP "Kopier (ALT-K)".

DEFINE BUTTON delete_tbCustomer 
     IMAGE-UP FILE "bmp/del16e.bmp":U
     LABEL "Slett" 
     SIZE 6 BY 1.52 TOOLTIP "Slett (CTRL-D)".

DEFINE BUTTON excel_tbCustomer 
     IMAGE-UP FILE "gif/afexcel.gif":U
     LABEL "Eksporter til E&xcel" 
     SIZE 6 BY 1.52 TOOLTIP "Eksporter til E&xcel (ALT-X)".

DEFINE BUTTON new_tbCustomer 
     IMAGE-UP FILE "bmp/new16e.bmp":U
     LABEL "Ny" 
     SIZE 6 BY 1.52 TOOLTIP "Ny (CTRL-N)".

DEFINE BUTTON save_tbCustomer 
     IMAGE-UP FILE "bmp/save.bmp":U
     LABEL "Lagre" 
     SIZE 6 BY 1.52 TOOLTIP "Lagre (CTRL-S)".

DEFINE BUTTON undo_tbCustomer 
     IMAGE-UP FILE "bmp/undo16e.bmp":U
     LABEL "Angre" 
     SIZE 6 BY 1.52 TOOLTIP "Angre (CTRL-Z)".

DEFINE VARIABLE JBoxMsTreeView AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 44 BY 6.43 NO-UNDO.

DEFINE RECTANGLE JBoxFpSpread
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 62 BY 5.71.

DEFINE RECTANGLE tbCustomer
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 84.4 BY 1.71.

DEFINE RECTANGLE ViewCustomer
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 158 BY 19.29.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BrwCustomer FOR 
      Customer SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BrwCustomer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BrwCustomer C-Win _FREEFORM
  QUERY BrwCustomer NO-LOCK DISPLAY
      Customer.Contact FORMAT "x(30)":U LABEL 'Contact'
Customer.Country FORMAT "x(20)":U LABEL 'Country'
Customer.CreditLimit FORMAT "->,>>>,>>9":U LABEL 'Credit Limit'
Customer.CustNum FORMAT ">>>>9":U LABEL 'Cust Num'
Customer.Discount FORMAT ">>9%":U LABEL 'Discount'
Customer.EmailAddress FORMAT "x(50)":U LABEL 'Email'
Customer.Fax FORMAT "x(20)":U LABEL 'Fax'
ENABLE Customer.Contact
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 36 BY 5.71 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     new_tbCustomer AT ROW 1.33 COL 2 WIDGET-ID 4
     copy_tbCustomer AT ROW 1.33 COL 8 WIDGET-ID 6
     undo_tbCustomer AT ROW 1.33 COL 14.2 WIDGET-ID 8
     delete_tbCustomer AT ROW 1.33 COL 20.2 WIDGET-ID 10
     save_tbCustomer AT ROW 1.33 COL 26.4 WIDGET-ID 12
     excel_tbCustomer AT ROW 1.33 COL 32.4 WIDGET-ID 14
     JBoxMsTreeView AT ROW 2.67 COL 112 NO-LABEL WIDGET-ID 26
     BrwCustomer AT ROW 3.62 COL 4 WIDGET-ID 200
     tbCustomer AT ROW 1.24 COL 1.6 WIDGET-ID 2
     JBoxFpSpread AT ROW 3.86 COL 42 WIDGET-ID 22
     ViewCustomer AT ROW 10.29 COL 4 WIDGET-ID 24
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 162 BY 28.95 WIDGET-ID 100.


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
         HEIGHT             = 28.95
         WIDTH              = 162
         MAX-HEIGHT         = 28.95
         MAX-WIDTH          = 162
         VIRTUAL-HEIGHT     = 28.95
         VIRTUAL-WIDTH      = 162
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
/* BROWSE-TAB BrwCustomer JBoxMsTreeView DEFAULT-FRAME */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 28.95
       FRAME DEFAULT-FRAME:WIDTH            = 162.

ASSIGN 
       tbCustomer:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "new;Ny,copy;Kopier,undo;Angre,delete;Slett,save;Lagre,excel;Eksporter til E&xcelmaxborder".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BrwCustomer
/* Query rebuild information for BROWSE BrwCustomer
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Customer NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
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
/* When multiple queries always check which is the current one: */
IF oBrwCustomer:IsCurrent THEN DO WITH FRAME {&FRAME-NAME}:
  otbCustomer:disabledTools = "new,edit".
END.

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
  DISPLAY JBoxMsTreeView 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE tbCustomer JBoxFpSpread ViewCustomer new_tbCustomer copy_tbCustomer 
         undo_tbCustomer delete_tbCustomer save_tbCustomer excel_tbCustomer 
         JBoxMsTreeView BrwCustomer 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeComponents C-Win 
PROCEDURE InitializeComponents :
&IF DEFINED(AdvGuiWin) &THEN
DO WITH FRAME {&FRAME-NAME}:


  oJBoxFpSpread = NEW JBoxFpSpread(THIS-PROCEDURE,JBoxFpSpread:HANDLE).
  oJBoxFpSpread:RegisterWithJukeBox(YES).
  oBrwCustomer:OpenQuery().
  oJBoxFpSpread:InitializeSpreadSheetFromHandle(oBrwCustomer:BUFFER-HANDLE,YES).
  oJBoxMsTreeView = NEW uc.JBoxMsTreeView(THIS-PROCEDURE,JBoxMsTreeView:HANDLE,"tvpics\Fold.bmp,tvpics\FoldOpen.bmp").
  oJBoxMsTreeView:RegisterWithJukeBox(YES).
  oJBoxMsTreeView:setAllowDrop(YES).
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

DYNAMIC-FUNCTION("CreateStatusBar",THIS-PROCEDURE:CURRENT-WINDOW,"",0,YES,?).

DO WITH FRAME {&FRAME-NAME}:

  oBrwCustomer = NEW JBoxBrowse(brwCustomer:HANDLE).

  otbCustomer = NEW JBoxToolbar(tbCustomer:HANDLE).

  oBrwCustomer:TOOLBAR-OBJECT = otbCustomer.
  otbCustomer:BROWSE-OBJECT = oBrwCustomer.
  RUN InitializeComponents.
  
  opopupCustomer = NEW JBoxPopupMenu().
  opopupCustomer:AddToolGroup('undo|File;Angre,save|File;Lagre,|Edit,delete|Edit;Slett,refresh|Edit;Refresh').

  oBrwCustomer:POPUP-MENU-OBJECT = opopupCustomer.
  
  oViewCustomer = NEW JBoxViewer(ViewCustomer:HANDLE,oBrwCustomer,"oneToOne","Custnum","CustView.w").
END.

dynamic-function("setNoResizeX",this-procedure:current-window,frame {&FRAME-NAME}:handle,"brwCustomer").
DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,400,200,0,0).


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

