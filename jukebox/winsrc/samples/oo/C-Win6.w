&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          sports115        PROGRESS
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


/*** Start instance property definitions for JBoxBrowse object oBrwOrder ***/
DEF VAR oBrwOrder AS JBoxBrowse NO-UNDO.

DEF TEMP-TABLE Order
    FIELD Ordernum AS integer
    FIELD OrderDate AS date
    FIELD CustNum AS integer
    FIELD OrderTot AS DECIMAL
    FIELD Instructions AS character
    FIELD Name AS character
    FIELD RowIdent1 AS CHARACTER 
    FIELD RowIdent2 AS CHARACTER 
    FIELD RowCount AS INTEGER
    FIELD jbCountDistinct AS INTEGER FORMAT '>>>,>>>,>>9' INIT 1
    FIELD jbAverage AS DECIMAL FORMAT '->>>,>>>,>>9.99'
    INDEX idxRowids  RowIdent1 RowIdent2
    .
DEF BUFFER v_Order FOR Order.


FUNCTION getBuffersAndFieldsBrwOrder RETURNS CHARACTER():
  RETURN
    'Order'
     + ';Ordernum'
     + ';OrderDate'
     + ';CustNum'
     + ';Instructions'
     + ';+OrderTot|DECIMAL||jb_total(EACH OrderLine OF Order¤Price * Qty)'
  + ',Customer'
     + ';Name'
     .
END FUNCTION.
FUNCTION getQueryJoinBrwOrder RETURNS CHARACTER():
  RETURN 'EACH Customer OF Order NO-LOCK'.
END FUNCTION.

DEF VAR oFmOrder AS JBoxFieldMap NO-UNDO.


DEF VAR oTbOrder AS JBoxToolbar NO-UNDO.


DEF VAR oOrderDate_JBoxDevExEdit AS JBoxDevExDateEdit NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME brwOrder

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Order

/* Definitions for BROWSE brwOrder                                      */
&Scoped-define FIELDS-IN-QUERY-brwOrder Order.Ordernum Order.OrderDate ~
Order.CustNum Order.OrderTot Order.Instructions Order.Name 
&Scoped-define ENABLED-FIELDS-IN-QUERY-brwOrder Order.Ordernum 
&Scoped-define QUERY-STRING-brwOrder FOR EACH Order NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-brwOrder OPEN QUERY brwOrder FOR EACH Order NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-brwOrder Order
&Scoped-define FIRST-TABLE-IN-QUERY-brwOrder Order


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tbOrder new_tbOrder edit_tbOrder ~
copy_tbOrder undo_tbOrder save_tbOrder delete_tbOrder refresh_tbOrder ~
filter_tbOrder browseconfig_tbOrder excel_tbOrder test_tbOrder word_tbOrder ~
color_tbOrder note_tbOrder documents_tbOrder first_tbOrder prev_tbOrder ~
next_tbOrder last_tbOrder brwOrder Ordernum OrderDate CustNum Name ~
Instructions 
&Scoped-Define DISPLAYED-OBJECTS Ordernum OrderDate CustNum Name ~
Instructions 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON browseconfig_tbOrder 
     IMAGE-UP FILE "bmp/table.bmp":U
     LABEL "Kolonneoppsett" 
     SIZE 4.6 BY 1.1 TOOLTIP "Kolonneoppsett (ALT-K)".

DEFINE BUTTON color_tbOrder 
     IMAGE-UP FILE "bmp/color.bmp":U
     LABEL "Farge" 
     SIZE 4.6 BY 1.1 TOOLTIP "Farge (ALT-F)".

DEFINE BUTTON copy_tbOrder 
     IMAGE-UP FILE "bmp/copy.bmp":U
     LABEL "Kopier" 
     SIZE 4.6 BY 1.1 TOOLTIP "Kopier (ALT-K)".

DEFINE BUTTON delete_tbOrder 
     IMAGE-UP FILE "bmp/del16e.bmp":U
     LABEL "Slett" 
     SIZE 4.6 BY 1.1 TOOLTIP "Slett (CTRL-D)".

DEFINE BUTTON documents_tbOrder 
     IMAGE-UP FILE "bmp/critino.bmp":U
     LABEL "Dokumenter" 
     SIZE 4.6 BY 1.1 TOOLTIP "Dokumenter (ALT-D)".

DEFINE BUTTON edit_tbOrder 
     IMAGE-UP FILE "bmp/edit16e.bmp":U
     LABEL "Endre" 
     SIZE 4.6 BY 1.1 TOOLTIP "Endre (CTRL-E)".

DEFINE BUTTON excel_tbOrder 
     IMAGE-UP FILE "gif/afexcel.gif":U
     LABEL "Eksporter til E&xcel" 
     SIZE 4.6 BY 1.1 TOOLTIP "Eksporter til E&xcel (ALT-X)".

DEFINE BUTTON filter_tbOrder 
     IMAGE-UP FILE "gif/filter.gif":U
     LABEL "Filter" 
     SIZE 4.6 BY 1.1 TOOLTIP "Filter (CTRL-F)".

DEFINE BUTTON first_tbOrder 
     IMAGE-UP FILE "bmp/first.bmp":U
     LABEL "Første" 
     SIZE 4.6 BY 1.1 TOOLTIP "Første (ALT-F)".

DEFINE BUTTON last_tbOrder 
     IMAGE-UP FILE "bmp/last.bmp":U
     LABEL "Siste" 
     SIZE 4.6 BY 1.1 TOOLTIP "Siste (ALT-S)".

DEFINE BUTTON new_tbOrder 
     IMAGE-UP FILE "bmp/new16e.bmp":U
     LABEL "Ny" 
     SIZE 4.6 BY 1.1 TOOLTIP "Ny (CTRL-N)".

DEFINE BUTTON next_tbOrder 
     IMAGE-UP FILE "bmp/next.bmp":U
     LABEL "Neste" 
     SIZE 4.6 BY 1.1 TOOLTIP "Neste (ALT-N)".

DEFINE BUTTON note_tbOrder 
     IMAGE-UP FILE "bmp/note16e.bmp":U
     LABEL "Notat" 
     SIZE 4.6 BY 1.1 TOOLTIP "Notat (ALT-N)".

DEFINE BUTTON prev_tbOrder 
     IMAGE-UP FILE "bmp/prev.bmp":U
     LABEL "Forrige" 
     SIZE 4.6 BY 1.1 TOOLTIP "Forrige (ALT-F)".

DEFINE BUTTON refresh_tbOrder 
     IMAGE-UP FILE "gif/refresh.gif":U
     LABEL "Oppdater visning" 
     SIZE 4.6 BY 1.1 TOOLTIP "Oppdater visning (F5)".

DEFINE BUTTON save_tbOrder 
     IMAGE-UP FILE "bmp/save.bmp":U
     LABEL "Lagre" 
     SIZE 4.6 BY 1.1 TOOLTIP "Lagre (CTRL-S)".

DEFINE BUTTON test_tbOrder 
     LABEL "Test" 
     SIZE 6 BY 1.1.

DEFINE BUTTON undo_tbOrder 
     IMAGE-UP FILE "bmp/undo16e.bmp":U
     LABEL "Angre" 
     SIZE 4.6 BY 1.1 TOOLTIP "Angre (CTRL-Z)".

DEFINE BUTTON word_tbOrder 
     IMAGE-UP FILE "gif/afword.gif":U
     LABEL "Word" 
     SIZE 4.6 BY 1.1 TOOLTIP "Word (ALT-W)".

DEFINE VARIABLE CustNum AS INTEGER FORMAT ">>>>9" INITIAL 0 
     LABEL "Cust Num" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 TOOLTIP "Please enter an existing customer number.".

DEFINE VARIABLE Instructions AS CHARACTER FORMAT "x(50)" 
     LABEL "Instructions" 
     VIEW-AS FILL-IN 
     SIZE 46 BY 1 TOOLTIP "Please enter Instructions".

DEFINE VARIABLE Name AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 32.4 BY 1 TOOLTIP "Please enter a name.".

DEFINE VARIABLE OrderDate AS DATE FORMAT "99/99/99" 
     LABEL "Ordered" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1 TOOLTIP "Please enter the date of order.".

DEFINE VARIABLE Ordernum AS INTEGER FORMAT "zzzzzzzzz9" INITIAL 0 
     LABEL "Order Num" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 TOOLTIP "Please enter an order number.".

DEFINE RECTANGLE tbOrder
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 107 BY 1.29.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brwOrder FOR 
      Order SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brwOrder
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brwOrder C-Win _STRUCTURED
  QUERY brwOrder NO-LOCK DISPLAY
      Order.Ordernum COLUMN-LABEL "Order Num" FORMAT "zzzzzzzzz9":U
            WIDTH 10.2
      Order.OrderDate COLUMN-LABEL "Ordered" FORMAT "99/99/99":U
      Order.CustNum COLUMN-LABEL "Cust Num" FORMAT ">>>>9":U
      Order.OrderTot COLUMN-LABEL "Order total" FORMAT "->>,>>>,>>9.99":U
      Order.Instructions COLUMN-LABEL "Instructions" FORMAT "x(50)":U
            WIDTH 36
      Order.Name COLUMN-LABEL "Name" FORMAT "x(30)":U
  ENABLE
      Order.Ordernum HELP "Please enter an order number."
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 106.6 BY 5.95 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     new_tbOrder AT ROW 1.33 COL 2.2 WIDGET-ID 4
     edit_tbOrder AT ROW 1.33 COL 7 WIDGET-ID 6
     copy_tbOrder AT ROW 1.33 COL 11.6 WIDGET-ID 8
     undo_tbOrder AT ROW 1.33 COL 16.2 WIDGET-ID 10
     save_tbOrder AT ROW 1.33 COL 20.8 WIDGET-ID 12
     delete_tbOrder AT ROW 1.33 COL 25.4 WIDGET-ID 14
     refresh_tbOrder AT ROW 1.33 COL 30 WIDGET-ID 16
     filter_tbOrder AT ROW 1.33 COL 34.6 WIDGET-ID 18
     browseconfig_tbOrder AT ROW 1.33 COL 39.2 WIDGET-ID 20
     excel_tbOrder AT ROW 1.33 COL 43.8 WIDGET-ID 22
     test_tbOrder AT ROW 1.33 COL 48.4 WIDGET-ID 40
     word_tbOrder AT ROW 1.33 COL 53 WIDGET-ID 32
     color_tbOrder AT ROW 1.33 COL 57.6 WIDGET-ID 34
     note_tbOrder AT ROW 1.33 COL 62.2 WIDGET-ID 36
     documents_tbOrder AT ROW 1.33 COL 66.8 WIDGET-ID 38
     first_tbOrder AT ROW 1.33 COL 71.4 WIDGET-ID 24
     prev_tbOrder AT ROW 1.33 COL 76 WIDGET-ID 26
     next_tbOrder AT ROW 1.33 COL 80.6 WIDGET-ID 28
     last_tbOrder AT ROW 1.33 COL 85.2 WIDGET-ID 30
     brwOrder AT ROW 3.14 COL 2.4 WIDGET-ID 200
     Ordernum AT ROW 9.57 COL 17 COLON-ALIGNED HELP
          "Please enter an order number."
     OrderDate AT ROW 10.67 COL 17 COLON-ALIGNED HELP
          "Please enter the date of order."
     CustNum AT ROW 11.76 COL 17 COLON-ALIGNED HELP
          "Please enter an existing customer number."
     Name AT ROW 11.76 COL 30.6 COLON-ALIGNED HELP
          "Please enter a name." NO-LABEL
     Instructions AT ROW 12.91 COL 17 COLON-ALIGNED HELP
          "Please enter Instructions"
     tbOrder AT ROW 1.24 COL 2 WIDGET-ID 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 109 BY 13.29 WIDGET-ID 100.


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
         HEIGHT             = 13.33
         WIDTH              = 109
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
/* BROWSE-TAB brwOrder last_tbOrder DEFAULT-FRAME */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brwOrder
/* Query rebuild information for BROWSE brwOrder
     _TblList          = "sports115.Order"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > "_<CALC>"
"Order.Ordernum" "Order Num" "zzzzzzzzz9" "integer" ? ? ? ? ? ? yes "Please enter an order number." no no "10.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > "_<CALC>"
"Order.OrderDate" "Ordered" "99/99/99" "date" ? ? ? ? ? ? no "Please enter the date of order." no no "9.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > "_<CALC>"
"Order.CustNum" "Cust Num" ">>>>9" "integer" ? ? ? ? ? ? no "Please enter an existing customer number." no no "9.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > "_<CALC>"
"Order.OrderTot" "Order total" "->>,>>>,>>9.99" "DECIMAL" ? ? ? ? ? ? no "" no no "14.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > "_<CALC>"
"Order.Instructions" "Instructions" "x(50)" "character" ? ? ? ? ? ? no "Please enter Instructions" no no "36" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > "_<CALC>"
"Order.Name" "Name" "x(30)" "character" ? ? ? ? ? ? no "Please enter a name." no no "19" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE brwOrder */
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


&Scoped-define BROWSE-NAME brwOrder
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE colorRecord C-Win 
PROCEDURE colorRecord :
RUN SUPER.


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
  DISPLAY Ordernum OrderDate CustNum Name Instructions 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE tbOrder new_tbOrder edit_tbOrder copy_tbOrder undo_tbOrder 
         save_tbOrder delete_tbOrder refresh_tbOrder filter_tbOrder 
         browseconfig_tbOrder excel_tbOrder test_tbOrder word_tbOrder 
         color_tbOrder note_tbOrder documents_tbOrder first_tbOrder 
         prev_tbOrder next_tbOrder last_tbOrder brwOrder Ordernum OrderDate 
         CustNum Name Instructions 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeComponents C-Win 
PROCEDURE InitializeComponents :
&IF DEFINED(AdvGuiWin) &THEN
DO WITH FRAME {&FRAME-NAME}:


  oOrderDate_JBoxDevExEdit = NEW JBoxDevExDateEdit(THIS-PROCEDURE,OrderDate:HANDLE).
  oOrderDate_JBoxDevExEdit:RegisterWithJukeBox(YES). /* YES: Visible */
  oOrderDate_JBoxDevExEdit:CreateDisplayLink(oFmOrder:BUFFER-HANDLE,'OrderDate').
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

  oBrwOrder = NEW JBoxBrowse(brwOrder:HANDLE).

  oFmOrder = NEW JBoxFieldMap(FRAME {&FRAME-NAME}:HANDLE).
  oFmOrder:updateFields = 'Ordernum,OrderDate,CustNum,Instructions'.
  oFmOrder:displayFields = 'Name'.
  oFmOrder:primaryKeyFields = 'Ordernum'.

  oFmOrder:BROWSE-OBJECT = oBrwOrder.
  oTbOrder = NEW JBoxToolbar(tbOrder:HANDLE,'File').

  oBrwOrder:TOOLBAR-OBJECT = oTbOrder.
  oFmOrder:TOOLBAR-OBJECT = oTbOrder.

  RUN InitializeComponents.
END.
oBrwOrder:OpenQuery().


DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,500,100,0,0).

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
&IF DEFINED(UIB_is_Running) NE 0 &THEN
  IF DYNAMIC-FUNCTION("getAttribute",THIS-PROCEDURE,"advGuiWin") = "YES" THEN
    RUN ShowForm("").
&ENDIF
THIS-PROCEDURE:CURRENT-WINDOW:WINDOW-STATE = 3.
THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
DYNAMIC-FUNCTION("DoLockWindow",?).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE testRecord C-Win 
PROCEDURE testRecord :
RUN SUPER.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE wordRecord C-Win 
PROCEDURE wordRecord :
RUN SUPER.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

