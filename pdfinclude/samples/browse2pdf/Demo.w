&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          sports2000       PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

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

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{pdfprint.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES sports2000.Customer

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 sports2000.Customer.CustNum ~
sports2000.Customer.Name sports2000.Customer.Phone ~
sports2000.Customer.Balance / sports2000.Customer.CreditLimit 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1 
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH sports2000.Customer ~
      WHERE Customer.CustNum < 1000 NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY BROWSE-1 FOR EACH sports2000.Customer ~
      WHERE Customer.CustNum < 1000 NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 sports2000.Customer
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 sports2000.Customer


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-1 fillin_iRed fillin_iGreen TOGGLE-1 ~
ButtonPrint fillin_iBlue 
&Scoped-Define DISPLAYED-OBJECTS fillin_iRed fillin_iGreen TOGGLE-1 ~
fillin_iBlue 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CalcColumnDemo C-Win 
FUNCTION CalcColumnDemo RETURNS CHARACTER
  (ip_hQuery AS HANDLE,
   ip_iRow AS INT,
   ip_iCol AS INT)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON ButtonPrint 
     LABEL "&Print" 
     CONTEXT-HELP-ID 12037
     SIZE 15 BY 1.14 TOOLTIP "&Print".

DEFINE VARIABLE fillin_iBlue AS INTEGER FORMAT "999":U INITIAL 229 
     LABEL "&Blue" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE fillin_iGreen AS INTEGER FORMAT "999":U INITIAL 229 
     LABEL "G&reen" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE fillin_iRed AS INTEGER FORMAT "999":U INITIAL 229 
     LABEL "&Red" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE TOGGLE-1 AS LOGICAL INITIAL yes 
     LABEL "Alternative Rows Have Different Colours" 
     VIEW-AS TOGGLE-BOX
     SIZE 43 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      sports2000.Customer SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 C-Win _STRUCTURED
  QUERY BROWSE-1 NO-LOCK DISPLAY
      sports2000.Customer.CustNum FORMAT ">>>>9":U WIDTH 13.2
      sports2000.Customer.Name FORMAT "x(30)":U WIDTH 41.2
      sports2000.Customer.Phone FORMAT "x(20)":U WIDTH 33.2
      sports2000.Customer.Balance / sports2000.Customer.CreditLimit COLUMN-LABEL "Bal / Limit" FORMAT "->,>>>,>>9.99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 126 BY 15.48 EXPANDABLE TOOLTIP "Browse 1"
         CONTEXT-HELP-ID 12036.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BROWSE-1 AT ROW 1.48 COL 3
     fillin_iRed AT ROW 17.43 COL 55 COLON-ALIGNED
     fillin_iGreen AT ROW 18.62 COL 55 COLON-ALIGNED
     TOGGLE-1 AT ROW 18.86 COL 4
     ButtonPrint AT ROW 19.33 COL 115
     fillin_iBlue AT ROW 19.81 COL 55 COLON-ALIGNED
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 131.4 BY 19.95.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Customer Listing"
         HEIGHT             = 19.91
         WIDTH              = 131
         MAX-HEIGHT         = 26.95
         MAX-WIDTH          = 142.4
         VIRTUAL-HEIGHT     = 26.95
         VIRTUAL-WIDTH      = 142.4
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         CONTEXT-HELP       = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
/* BROWSE-TAB BROWSE-1 1 DEFAULT-FRAME */
ASSIGN 
       BROWSE-1:COLUMN-RESIZABLE IN FRAME DEFAULT-FRAME       = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _TblList          = "sports2000.Customer"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "Customer.CustNum < 1000"
     _FldNameList[1]   > sports2000.Customer.CustNum
"CustNum" ? ? "integer" ? ? ? ? ? ? no ? no no "13.2" yes no no "U" "" ""
     _FldNameList[2]   > sports2000.Customer.Name
"Name" ? ? "character" ? ? ? ? ? ? no ? no no "41.2" yes no no "U" "" ""
     _FldNameList[3]   > sports2000.Customer.Phone
"Phone" ? ? "character" ? ? ? ? ? ? no ? no no "33.2" yes no no "U" "" ""
     _FldNameList[4]   > "_<CALC>"
"sports2000.Customer.Balance / sports2000.Customer.CreditLimit" "Bal / Limit" "->,>>>,>>9.99" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Customer Listing */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Customer Listing */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ButtonPrint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ButtonPrint C-Win
ON CHOOSE OF ButtonPrint IN FRAME DEFAULT-FRAME /* Print */
DO:
 FIND FIRST TTPDF.
 
 ASSIGN TTPDF.creporttitle = {&WINDOW-NAME}:TITLE
        TTPDF.hbrowse = {&BROWSE-NAME}:HANDLE IN FRAME {&FRAME-NAME}
        TTPDF.loAltColours = Toggle-1:CHECKED
        TTPDF.deAltColour[1] = DEC(fillin_iRed:SCREEN-VALUE)
        TTPDF.deAltColour[2] = DEC(fillin_iGreen:SCREEN-VALUE)
        TTPDF.deAltColour[3] = DEC(fillin_iBlue:SCREEN-VALUE)
        TTPDF.cFunction[4] = "CalcColumnDemo".
        
 RUN pdflibrary.p (INPUT-OUTPUT TABLE TTPDF).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-1
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
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
  DISPLAY fillin_iRed fillin_iGreen TOGGLE-1 fillin_iBlue 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE BROWSE-1 fillin_iRed fillin_iGreen TOGGLE-1 ButtonPrint fillin_iBlue 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CalcColumnDemo C-Win 
FUNCTION CalcColumnDemo RETURNS CHARACTER
  (ip_hQuery AS HANDLE,
   ip_iRow AS INT,
   ip_iCol AS INT) :
 
 BROWSE {&BROWSE-NAME}:QUERY:REPOSITION-TO-ROW(ip_iRow). /* display row */

 RETURN BROWSE {&BROWSE-NAME}:GET-BROWSE-COLUMN(ip_iCol):SCREEN-VALUE.    /* get column screen-value */
 
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

