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


/*** Start instance property definitions for JBoxBrowse object oBrwCustomer ***/
DEF VAR oBrwCustomer AS JBoxBrowse NO-UNDO.
DEF TEMP-TABLE Customer
    FIELD CustNum AS integer
    FIELD Name AS character
    FIELD SalesRep AS character
    FIELD RepName AS character
    FIELD CustOrderTotal AS DECIMAL
    FIELD Comments AS character
    FIELD DocumentsExist AS LOGICAL
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
     + ';SalesRep'
     + ';Comments'
     + ';+CustOrderTotal|DECIMAL||CustOrderTotal(CustNum)'
     + ';+DocumentsExist|LOGICAL||jb_can-find(FIRST JBoxDocRel WHERE JBoxDocRel.cContext = "Customer" AND JBoxDocRel.cEntityId = STRING(customer.custnum))'
  + ',Salesrep'
     + ';RepName'
     .
END FUNCTION.
FUNCTION getQueryJoinBrwCustomer RETURNS CHARACTER():
  RETURN 'EACH Salesrep OF Customer NO-LOCK'.
END FUNCTION.
FUNCTION getCalcFieldProcBrwCustomer RETURNS CHARACTER():
  RETURN 
     'customer_browsecalc.p' /* CustOrderTotal(CustNum) */
     .
END FUNCTION.

DEF VAR oFmCustomer AS JBoxFieldMap NO-UNDO.
DEF VAR oTbCustomer AS JBoxToolbar NO-UNDO.



DEF VAR oJBoxDevExEdit AS JBoxDevExEdit NO-UNDO.

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
Customer.SalesRep Customer.RepName Customer.CustOrderTotal ~
Customer.Comments Customer.DocumentsExist 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BrwCustomer Customer.CustNum ~
Customer.CustOrderTotal 
&Scoped-define QUERY-STRING-BrwCustomer FOR EACH Customer NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BrwCustomer OPEN QUERY BrwCustomer FOR EACH Customer NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BrwCustomer Customer
&Scoped-define FIRST-TABLE-IN-QUERY-BrwCustomer Customer


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tbCustomer new_tbCustomer edit_tbCustomer ~
copy_tbCustomer undo_tbCustomer save_tbCustomer delete_tbCustomer ~
refresh_tbCustomer filter_tbCustomer browseconfig_tbCustomer ~
excel_tbCustomer activate_tbCustomer color_tbCustomer note_tbCustomer ~
documents_tbCustomer insert_tbCustomer first_tbCustomer prev_tbCustomer ~
next_tbCustomer last_tbCustomer flopp_tbCustomer BrwCustomer CustNum Name ~
SalesRep RepName Comments 
&Scoped-Define DISPLAYED-OBJECTS CustNum Name SalesRep RepName Comments 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON activate_tbCustomer 
     IMAGE-UP FILE "gif/active.gif":U
     LABEL "Aktiver" 
     SIZE 4.6 BY 1.1 TOOLTIP "Activate (ALT-A)".

DEFINE BUTTON browseconfig_tbCustomer 
     IMAGE-UP FILE "bmp/table.bmp":U
     LABEL "Kolonneoppsett" 
     SIZE 4.6 BY 1.1 TOOLTIP "Column setup (ALT-C)".

DEFINE BUTTON color_tbCustomer 
     IMAGE-UP FILE "bmp/color.bmp":U
     LABEL "Farge" 
     SIZE 4.6 BY 1.1 TOOLTIP "Color (ALT-C)".

DEFINE BUTTON copy_tbCustomer 
     IMAGE-UP FILE "bmp/copy.bmp":U
     LABEL "Kopier" 
     SIZE 4.6 BY 1.1 TOOLTIP "Kopier (ALT-K)".

DEFINE BUTTON delete_tbCustomer 
     IMAGE-UP FILE "bmp/del16e.bmp":U
     LABEL "Slett" 
     SIZE 4.6 BY 1.1 TOOLTIP "Slett (CTRL-D)".

DEFINE BUTTON documents_tbCustomer 
     IMAGE-UP FILE "bmp/critino.bmp":U
     LABEL "Dokumenter" 
     SIZE 4.6 BY 1.1 TOOLTIP "Documents (ALT-D)".

DEFINE BUTTON edit_tbCustomer 
     IMAGE-UP FILE "bmp/edit16e.bmp":U
     LABEL "Endre" 
     SIZE 4.6 BY 1.1 TOOLTIP "Edit (CTRL-E)".

DEFINE BUTTON excel_tbCustomer 
     IMAGE-UP FILE "gif/afexcel.gif":U
     LABEL "Excel" 
     SIZE 4.6 BY 1.1 TOOLTIP "Eksporter til E&xcel (ALT-X)".

DEFINE BUTTON filter_tbCustomer 
     IMAGE-UP FILE "gif/filter.gif":U
     LABEL "Filter" 
     SIZE 4.6 BY 1.1 TOOLTIP "Filter (CTRL-F)".

DEFINE BUTTON first_tbCustomer 
     IMAGE-UP FILE "bmp/first.bmp":U
     LABEL "Første" 
     SIZE 4.6 BY 1.1 TOOLTIP "First (ALT-F)".

DEFINE BUTTON flopp_tbCustomer 
     IMAGE-UP FILE "bmp\Debt.bmp":U
     LABEL "Btn 1" 
     SIZE 4.6 BY 1.1.

DEFINE BUTTON insert_tbCustomer 
     IMAGE-UP FILE "bmp/add.bmp":U
     LABEL "Sett inn" 
     SIZE 4.6 BY 1.1 TOOLTIP "Sett inn (ALT-S)".

DEFINE BUTTON last_tbCustomer 
     IMAGE-UP FILE "bmp/last.bmp":U
     LABEL "Siste" 
     SIZE 4.6 BY 1.1 TOOLTIP "Last (ALT-L)".

DEFINE BUTTON new_tbCustomer 
     IMAGE-UP FILE "bmp/new16e.bmp":U
     LABEL "Ny" 
     SIZE 4.6 BY 1.1 TOOLTIP "Ny (CTRL-N)".

DEFINE BUTTON next_tbCustomer 
     IMAGE-UP FILE "bmp/next.bmp":U
     LABEL "Neste" 
     SIZE 4.6 BY 1.1 TOOLTIP "Next (ALT-N)".

DEFINE BUTTON note_tbCustomer 
     IMAGE-UP FILE "bmp/note16e.bmp":U
     LABEL "Notat" 
     SIZE 4.6 BY 1.1 TOOLTIP "Note (ALT-N)".

DEFINE BUTTON prev_tbCustomer 
     IMAGE-UP FILE "bmp/prev.bmp":U
     LABEL "Forrige" 
     SIZE 4.6 BY 1.1 TOOLTIP "Prev (ALT-P)".

DEFINE BUTTON refresh_tbCustomer 
     IMAGE-UP FILE "gif/refresh.gif":U
     LABEL "Oppdater visning" 
     SIZE 4.6 BY 1.1 TOOLTIP "Refresh (F5)".

DEFINE BUTTON save_tbCustomer 
     IMAGE-UP FILE "bmp/save.bmp":U
     LABEL "Lagre" 
     SIZE 4.6 BY 1.1 TOOLTIP "Lagre (CTRL-S)".

DEFINE BUTTON undo_tbCustomer 
     IMAGE-UP FILE "bmp/undo16e.bmp":U
     LABEL "Angre" 
     SIZE 4.6 BY 1.1 TOOLTIP "Angre (CTRL-Z)".

DEFINE VARIABLE Comments AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 105 BY 4 NO-UNDO.

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

DEFINE VARIABLE SalesRep AS CHARACTER FORMAT "x(4)" INITIAL "bbb" 
     LABEL "Sales Rep" 
     VIEW-AS FILL-IN 
     SIZE 9.6 BY 1.

DEFINE RECTANGLE tbCustomer
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 120 BY 1.29.

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
      Customer.CustOrderTotal COLUMN-LABEL "OrderTotal" FORMAT "->>,>>9.99":U
            WIDTH 31.2
      Customer.Comments COLUMN-LABEL "Comments" FORMAT "x(80)":U
      Customer.DocumentsExist COLUMN-LABEL "Docs" FORMAT "yes/no":U
  ENABLE
      Customer.CustNum HELP "Please enter a customer number."
      Customer.CustOrderTotal
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 118 BY 9.52 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     new_tbCustomer AT ROW 1.33 COL 1.4 WIDGET-ID 60
     edit_tbCustomer AT ROW 1.33 COL 6.2 WIDGET-ID 6
     copy_tbCustomer AT ROW 1.33 COL 10.8 WIDGET-ID 8
     undo_tbCustomer AT ROW 1.33 COL 15.4 WIDGET-ID 10
     save_tbCustomer AT ROW 1.33 COL 20 WIDGET-ID 12
     delete_tbCustomer AT ROW 1.33 COL 24.6 WIDGET-ID 14
     refresh_tbCustomer AT ROW 1.33 COL 29.2 WIDGET-ID 16
     filter_tbCustomer AT ROW 1.33 COL 33.8 WIDGET-ID 18
     browseconfig_tbCustomer AT ROW 1.33 COL 38.4 WIDGET-ID 20
     excel_tbCustomer AT ROW 1.33 COL 43 WIDGET-ID 22
     activate_tbCustomer AT ROW 1.33 COL 47.6 WIDGET-ID 54
     color_tbCustomer AT ROW 1.33 COL 52.2 WIDGET-ID 48
     note_tbCustomer AT ROW 1.33 COL 56.8 WIDGET-ID 50
     documents_tbCustomer AT ROW 1.33 COL 61.4 WIDGET-ID 38
     insert_tbCustomer AT ROW 1.33 COL 66 WIDGET-ID 52
     first_tbCustomer AT ROW 1.33 COL 70.6 WIDGET-ID 40
     prev_tbCustomer AT ROW 1.33 COL 75.2 WIDGET-ID 42
     next_tbCustomer AT ROW 1.33 COL 79.8 WIDGET-ID 44
     last_tbCustomer AT ROW 1.33 COL 84.4 WIDGET-ID 46
     flopp_tbCustomer AT ROW 1.33 COL 89 WIDGET-ID 32
     BrwCustomer AT ROW 3.24 COL 2 WIDGET-ID 200
     CustNum AT ROW 13.57 COL 13 COLON-ALIGNED HELP
          "Please enter a customer number."
     Name AT ROW 14.62 COL 13 COLON-ALIGNED HELP
          "Please enter a name."
     SalesRep AT ROW 15.67 COL 13 COLON-ALIGNED HELP
          "Please Enter a Sales Rep."
     RepName AT ROW 15.67 COL 26.8 COLON-ALIGNED HELP
          "Please enter the Name of the Salesperson." NO-LABEL
     Comments AT ROW 17.43 COL 15 NO-LABEL WIDGET-ID 58
     tbCustomer AT ROW 1.24 COL 1.2 WIDGET-ID 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 152.6 BY 25.48 WIDGET-ID 100.


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
         HEIGHT             = 25.48
         WIDTH              = 152.6
         MAX-HEIGHT         = 25.48
         MAX-WIDTH          = 152.6
         VIRTUAL-HEIGHT     = 25.48
         VIRTUAL-WIDTH      = 152.6
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
/* BROWSE-TAB BrwCustomer flopp_tbCustomer DEFAULT-FRAME */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 25.48
       FRAME DEFAULT-FRAME:WIDTH            = 152.6.

ASSIGN 
       Customer.Comments:VISIBLE IN BROWSE BrwCustomer = FALSE
       Customer.DocumentsExist:VISIBLE IN BROWSE BrwCustomer = FALSE.

ASSIGN 
       SalesRep:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "bbb".

ASSIGN 
       tbCustomer:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "new;Ny,edit;Endre,copy;Kopier,undo;Angre,save;Lagre,delete;Slett,refresh;Oppdater visning,filter;Filter,browseconfig;Kolonneoppsett,excel;Excel,activate;Aktiver,color;Farge,note;Notat,documents;Dokumenter,insert;Sett inn,first;Første,prev;Forrige,next;Neste,last;Siste,flopp;Btn 1maxborder".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BrwCustomer
/* Query rebuild information for BROWSE BrwCustomer
     _TblList          = "sports115.Customer"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > "_<CALC>"
"Customer.CustNum" "Cust Num" ">>>>9" "integer" ? ? ? ? ? ? yes "Please enter a customer number." no no "9.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > "_<CALC>"
"Customer.Name" "Name" "x(30)" "character" ? ? ? ? ? ? no "Please enter a name." no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > "_<CALC>"
"Customer.SalesRep" "Sales Rep" "x(4)" "character" ? ? ? ? ? ? no "Please Enter a Sales Rep." no no "9.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > "_<CALC>"
"Customer.RepName" "Rep Name" "x(30)" "character" ? ? ? ? ? ? no "Please enter the Name of the Salesperson." no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > "_<CALC>"
"Customer.CustOrderTotal" "OrderTotal" "->>,>>9.99" "DECIMAL" ? ? ? ? ? ? yes "" no no "31.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > "_<CALC>"
"Customer.Comments" "Comments" "x(80)" "character" ? ? ? ? ? ? no "Please enter comments." no no "80" no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > "_<CALC>"
"Customer.DocumentsExist" "Docs" "yes/no" "LOGICAL" ? ? ? ? ? ? no "" no no "5" no no no "U" "" "" "" "" "" "" 0 no 0 no no
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE activateRecord C-Win 
PROCEDURE activateRecord :
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
 Notes:
------------------------------------------------------------------------------*/
IF AVAIL Customer THEN DO:
  otbCustomer:noteCurrentValue = Customer.Comments.
  otbCustomer:docAvailable = Customer.DocumentsExist.
END.  
RUN SUPER.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE documentsRecord C-Win 
PROCEDURE documentsRecord :
DEF VAR hDocTree AS HANDLE NO-UNDO. /* <- Move to Defitions */

IF NOT VALID-HANDLE(hDocTree) THEN DO:
  RUN JBoxViewDocTree.w PERSIST SET hDocTree.
  RUN InitializeObject IN hDocTree.
END.

DYNAMIC-FUNCTION('setDocContext' IN hDocTree,
     STRING(Customer.custnum),
     'Customer',
     Customer.Name).

RUN MoveToTop IN hDocTree.

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
  DISPLAY CustNum Name SalesRep RepName Comments 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE tbCustomer new_tbCustomer edit_tbCustomer copy_tbCustomer 
         undo_tbCustomer save_tbCustomer delete_tbCustomer refresh_tbCustomer 
         filter_tbCustomer browseconfig_tbCustomer excel_tbCustomer 
         activate_tbCustomer color_tbCustomer note_tbCustomer 
         documents_tbCustomer insert_tbCustomer first_tbCustomer 
         prev_tbCustomer next_tbCustomer last_tbCustomer flopp_tbCustomer 
         BrwCustomer CustNum Name SalesRep RepName Comments 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeComponents C-Win 
PROCEDURE InitializeComponents :
&IF DEFINED(AdvGuiWin) &THEN
DO WITH FRAME {&FRAME-NAME}:

  oJBoxDevExEdit = NEW JBoxDevExEdit(THIS-PROCEDURE,Comments:HANDLE).
  oJBoxDevExEdit:RegisterWithJukeBox(YES).
  oJBoxDevExEdit:CreateDisplayLink(oFmCustomer:BUFFER-HANDLE,'Comments').
  oJBoxDevExEdit:cDocLoadContext = 'Customer.Comments'.
  oJBoxDevExEdit:cDocLoadIdFields = 'CustNum'.
  oJBoxDevExEdit:cDocLoadFormat = 'html'.
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

  oFmCustomer = NEW JBoxFieldMap(FRAME {&FRAME-NAME}:HANDLE).
  oFmCustomer:updateFields = 'CustNum,Name,SalesRep'.
  oFmCustomer:displayFields = 'RepName'.
  oFmCustomer:primaryKeyFields = 'CustNum'.

  oFmCustomer:BROWSE-OBJECT = oBrwCustomer.
  oTbCustomer = NEW JBoxToolbar(tbCustomer:HANDLE,'File').

  oBrwCustomer:TOOLBAR-OBJECT = oTbCustomer.
  oFmCustomer:TOOLBAR-OBJECT = oTbCustomer.
  RUN InitializeComponents.
END.
oBrwCustomer:OpenQuery().


DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,800,200,0,0).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE insertRecord C-Win 
PROCEDURE insertRecord :
RUN SUPER.


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE noteRecord C-Win 
PROCEDURE noteRecord :
RUN SUPER.
IF otbCustomer:noteCurrentValueChanged THEN DO:
  IF DYNAMIC-FUNCTION('DoUpdate','Customer','',
     '',Customer.RowIdent1,
      "comments",otbCustomer:noteCurrentValue,
     YES) THEN
    oBrwCustomer:refreshRow().
  ELSE DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE wwwRecord C-Win 
PROCEDURE wwwRecord :
DEF VAR cURL         AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  cURL = 'http://www.gulesider.no/finn:'
/*       + <namefield>:SCREEN-VALUE                                                             */
/*       + (IF <addressfield>:SCREEN-VALUE NE '' THEN '+' + <addressfield>:SCREEN-VALUE ELSE '')*/
/*       + (IF <zipcodefield>:SCREEN-VALUE NE '' THEN '+' + <zipcodefield>:SCREEN-VALUE ELSE '')*/
       .

  DYNAMIC-FUNCTION('setWebDoc','',cURL).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

