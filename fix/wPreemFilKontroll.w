&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
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
DEFINE VARIABLE cCopyFdir AS CHARACTER  NO-UNDO.
DEFINE TEMP-TABLE TT_Filer NO-UNDO
    FIELD butikkNr LIKE butiker.butik
    FIELD Filnavn  AS CHAR
    FIELD Dato     AS CHAR
    FIELD SourceFil AS CHAR
    FIELD DestFil   AS CHAR
    INDEX dato dato.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-fl

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES TT_Filer

/* Definitions for BROWSE BROWSE-fl                                     */
&Scoped-define FIELDS-IN-QUERY-BROWSE-fl butikkNr Filnavn Dato SourceFil DestFil   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-fl   
&Scoped-define SELF-NAME BROWSE-fl
&Scoped-define QUERY-STRING-BROWSE-fl FOR EACH TT_Filer
&Scoped-define OPEN-QUERY-BROWSE-fl OPEN QUERY {&SELF-NAME} FOR EACH TT_Filer.
&Scoped-define TABLES-IN-QUERY-BROWSE-fl TT_Filer
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-fl TT_Filer


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BROWSE-fl}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS B-Copy BROWSE-fl 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Copy 
     LABEL "Kör Copy" 
     SIZE 15 BY 1.14.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-fl FOR 
      TT_Filer SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-fl
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-fl C-Win _FREEFORM
  QUERY BROWSE-fl DISPLAY
      butikkNr
    Filnavn FORMAT "x(30)"
    Dato    FORMAT "x(10)"
    SourceFil FORMAT "x(70)"
    DestFil FORMAT "x(70)"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 202 BY 20.24 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     B-Copy AT ROW 3.14 COL 82
     BROWSE-fl AT ROW 5.05 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 213.4 BY 25.95.


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
         TITLE              = "<insert window title>"
         HEIGHT             = 25.95
         WIDTH              = 213.4
         MAX-HEIGHT         = 35.14
         MAX-WIDTH          = 213.4
         VIRTUAL-HEIGHT     = 35.14
         VIRTUAL-WIDTH      = 213.4
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



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
/* BROWSE-TAB BROWSE-fl B-Copy DEFAULT-FRAME */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-fl
/* Query rebuild information for BROWSE BROWSE-fl
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TT_Filer.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-fl */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <insert window title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Copy
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Copy C-Win
ON CHOOSE OF B-Copy IN FRAME DEFAULT-FRAME /* Kör Copy */
DO:
  RUN SkapaFillist.
  MESSAGE "Hello," SKIP
          "now we are done!!"
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-fl
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
  ASSIGN cCopyFdir = "d:\home\lindbak\ankommet\olddtl".
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
  ENABLE B-Copy BROWSE-fl 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fyllTT C-Win 
PROCEDURE fyllTT :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE dDato AS DATE       NO-UNDO.
    DO dDato = DATE(01,01,2005) TO DATE(03,31,2006):
        CREATE TT_Filer.
        ASSIGN TT_Filer.butikkNr = butiker.butik
               TT_Filer.Dato     = STRING(YEAR(dDato),"9999") + STRING(MONTH(dDato),"99") + STRING(DAY(dDato),"99")
               TT_Filer.Filnavn  = "DTL_" + string(butiker.butik) + "_" + TT_Filer.dato + "_"
               TT_Filer.SourceFil = cCopyFdir + "\" + STRING(Butiker.Butik) + "\" + TT_Filer.Filnavn + "*.txt"
               TT_Filer.DestFil   = "d:\home\lindbak\ankommet\" + STRING(butiker.butik) + "\" + TT_Filer.Filnavn + "*.txt".
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KopieraFiler C-Win 
PROCEDURE KopieraFiler :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH TT_Filer:
        OS-COMMAND SILENT VALUE("COPY " + TT_Filer.SourceFil + " " + TT_Filer.DestFil).
        DELETE TT_filer.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaFillist C-Win 
PROCEDURE SkapaFillist :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cKatalog AS CHARACTER  NO-UNDO.
    FOR EACH Butiker WHERE CAN-FIND(FIRST kasse WHERE kasse.butikknr = Butiker.butik) NO-LOCK.
        EMPTY TEMP-TABLE TT_Filer.
        RUN fyllTT.
        FOR EACH TT_filer.
            IF CAN-FIND(FIRST filer WHERE filer.filnavn BEGINS tt_filer.Filnavn) THEN
                DELETE tt_filer.
        END.
        IF CAN-FIND(FIRST tt_filer) THEN
            RUN KopieraFiler.
/*         FIND FIRST Kasse WHERE kasse.butikknr = butiker.butik NO-LOCK.    */
/*         IF TRIM(Kasse.ElJournalKatalog) = "" THEN                         */
/*             NEXT.                                                         */
/*         cKatalog = RIGHT-TRIM(Kasse.ElJournalKatalog,"\") + "\" + "\bku". */
/*         MESSAGE cKatalog                                                  */
/*             VIEW-AS ALERT-BOX INFO BUTTONS OK.                            */
/*         LEAVE.                                                            */
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

