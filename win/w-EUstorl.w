&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BR-EUskor

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES EUskor

/* Definitions for BROWSE BR-EUskor                                     */
&Scoped-define FIELDS-IN-QUERY-BR-EUskor EUskor.eustorl 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BR-EUskor 
&Scoped-define QUERY-STRING-BR-EUskor FOR EACH EUskor NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BR-EUskor OPEN QUERY BR-EUskor FOR EACH EUskor NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BR-EUskor EUskor
&Scoped-define FIRST-TABLE-IN-QUERY-BR-EUskor EUskor


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BR-EUskor}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-28 RECT-74 RECT-29 B-Ny B-Tabort B-Undo ~
B-Lagra BR-EUskor FI-eustorl RSfraktion 
&Scoped-Define DISPLAYED-OBJECTS FI-eustorl RSfraktion 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Lagra 
     IMAGE-UP FILE "icon/e-save.bmp":U
     LABEL "Lagra" 
     SIZE 4.8 BY 1.14.

DEFINE BUTTON B-Ny 
     IMAGE-UP FILE "icon/e-ny.bmp":U
     LABEL "Ny" 
     SIZE 4.8 BY 1.14.

DEFINE BUTTON B-Tabort 
     IMAGE-UP FILE "icon/e-del.bmp":U
     LABEL "Ta bort" 
     SIZE 4.8 BY 1.14.

DEFINE BUTTON B-Undo 
     IMAGE-UP FILE "icon/e-undo.bmp":U
     LABEL "Lagra" 
     SIZE 4.8 BY 1.14.

DEFINE VARIABLE FI-eustorl AS CHARACTER FORMAT "x(6)" 
     LABEL "Storlek" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE RSfraktion AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Hel", 1,
"Halv", 2,
"1/3", 3,
"2/3", 4
     SIZE 11 BY 4.05 NO-UNDO.

DEFINE RECTANGLE RECT-28
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 63.2 BY .19.

DEFINE RECTANGLE RECT-29
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 63.2 BY .19.

DEFINE RECTANGLE RECT-74
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 38 BY 5.48.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BR-EUskor FOR 
      EUskor SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BR-EUskor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BR-EUskor C-Win _STRUCTURED
  QUERY BR-EUskor NO-LOCK DISPLAY
      EUskor.eustorl FORMAT "x(6)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 15 BY 13.14 ROW-HEIGHT-CHARS .62 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     B-Ny AT ROW 1.24 COL 2
     B-Tabort AT ROW 1.24 COL 8.4
     B-Undo AT ROW 1.24 COL 14.8
     B-Lagra AT ROW 1.24 COL 21.2
     BR-EUskor AT ROW 2.86 COL 1.8
     FI-eustorl AT ROW 3.29 COL 31.6 COLON-ALIGNED
     RSfraktion AT ROW 3.29 COL 45.6 NO-LABEL
     RECT-28 AT ROW 2.48 COL 1.6
     RECT-74 AT ROW 2.91 COL 20.2
     RECT-29 AT ROW 1 COL 1.6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 64.2 BY 15.24.


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
         TITLE              = "Grundtabell EUstorlekar"
         HEIGHT             = 15.24
         WIDTH              = 64.2
         MAX-HEIGHT         = 15.24
         MAX-WIDTH          = 66.4
         VIRTUAL-HEIGHT     = 15.24
         VIRTUAL-WIDTH      = 66.4
         RESIZE             = no
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
   FRAME-NAME                                                           */
/* BROWSE-TAB BR-EUskor B-Lagra DEFAULT-FRAME */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BR-EUskor
/* Query rebuild information for BROWSE BR-EUskor
     _TblList          = "SkoTex.EUskor"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   = SkoTex.EUskor.eustorl
     _Query            is OPENED
*/  /* BROWSE BR-EUskor */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Grundtabell EUstorlekar */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Grundtabell EUstorlekar */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Lagra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Lagra C-Win
ON CHOOSE OF B-Lagra IN FRAME DEFAULT-FRAME /* Lagra */
DO:
    DEFINE VARIABLE cc AS CHAR     NO-UNDO.
    cc = FI-eustorl:SCREEN-VALUE + (IF RSfraktion:SCREEN-VALUE = "1" THEN "" ELSE IF RSfraktion:SCREEN-VALUE = "2" THEN ".5" ELSE IF RSfraktion:SCREEN-VALUE = "3" THEN " 1/3" ELSE " 2/3").
    IF NOT CAN-FIND(EUskor WHERE EUskor.EUstorl = cc) THEN DO:
        CREATE EUskor.
        ASSIGN EUskor.EUstorl = cc.
        {&OPEN-QUERY-{&BROWSE-NAME}}
    END.
    ELSE DO:
        MESSAGE "Storlek" cc "redan registrerad"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.
    FIND EUskor WHERE EUskor.EUstorl = cc NO-LOCK.
    REPOSITION {&BROWSE-NAME} TO ROWID ROWID(EUskor).
    APPLY "VALUE-CHANGED" TO BROWSE {&BROWSE-NAME}.
    RSfraktion:SCREEN-VALUE = "1".
    FI-eustorl:SENSITIVE = FALSE.
    B-Tabort:SENSITIVE = TRUE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Ny
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Ny C-Win
ON CHOOSE OF B-Ny IN FRAME DEFAULT-FRAME /* Ny */
DO:
  FI-eustorl:SENSITIVE = TRUE.
  FI-eustorl:SCREEN-VALUE = "".
  B-Tabort:SENSITIVE = FALSE.
  B-Undo:SENSITIVE = TRUE.
  APPLY "ENTRY" TO FI-eustorl.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Tabort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Tabort C-Win
ON CHOOSE OF B-Tabort IN FRAME DEFAULT-FRAME /* Ta bort */
DO:
    DEFINE VARIABLE cEU AS CHARACTER   NO-UNDO.
  IF CAN-FIND(FIRST strtstr WHERE strtstr.EUstorl = EUskor.EUstorl) THEN DO:
      FOR EACH strtstr WHERE strtstr.EUstorl = EUskor.EUstorl NO-LOCK:
          cEU = cEU + (IF cEU <> "" THEN "," ELSE "") + STRING(strtstr.strtypeid).
      END.

      MESSAGE "EU-storlek registrerad under storlekstyp:"  cEU
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
  END.
  ELSE DO:
      FIND CURRENT EUskor.
      DELETE EUskor.
      BROWSE {&BROWSE-NAME}:REFRESH().
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Undo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Undo C-Win
ON CHOOSE OF B-Undo IN FRAME DEFAULT-FRAME /* Lagra */
DO:
    {&OPEN-QUERY-{&BROWSE-NAME}}
    APPLY "VALUE-CHANGED" TO BROWSE {&BROWSE-NAME}.
    RSfraktion:SCREEN-VALUE = "1".
    FI-eustorl:SENSITIVE = FALSE.
    B-Undo:SENSITIVE = FALSE.
    B-Tabort:SENSITIVE = TRUE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BR-EUskor
&Scoped-define SELF-NAME BR-EUskor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BR-EUskor C-Win
ON ENTRY OF BR-EUskor IN FRAME DEFAULT-FRAME
DO:
  IF B-Undo:SENSITIVE THEN DO:
      APPLY "CHOOSE" TO B-Undo.
      RETURN NO-APPLY.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BR-EUskor C-Win
ON VALUE-CHANGED OF BR-EUskor IN FRAME DEFAULT-FRAME
DO:
  FI-eustorl:SCREEN-VALUE = EUskor.EUstorl.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-eustorl
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-eustorl C-Win
ON ANY-PRINTABLE OF FI-eustorl IN FRAME DEFAULT-FRAME /* Storlek */
DO:
  IF LENGTH(INPUT FI-eustorl) = 2 OR LASTKEY < 48 OR LASTKEY > 57 THEN
      RETURN NO-APPLY.
  IF LENGTH(INPUT FI-eustorl) = 1 THEN DO:
      B-Lagra:SENSITIVE = TRUE.
      RSfraktion:SCREEN-VALUE = "1".
      RSfraktion:SENSITIVE = TRUE.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-eustorl C-Win
ON BACKSPACE OF FI-eustorl IN FRAME DEFAULT-FRAME /* Storlek */
DO:
  IF B-Lagra:SENSITIVE THEN
      B-Lagra:SENSITIVE = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
  FI-eustorl:SENSITIVE = FALSE.
  B-Undo:SENSITIVE = FALSE.
  B-Lagra:SENSITIVE = FALSE.

  APPLY "VALUE-CHANGED" TO BROWSE BR-EUskor.
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
  DISPLAY FI-eustorl RSfraktion 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-28 RECT-74 RECT-29 B-Ny B-Tabort B-Undo B-Lagra BR-EUskor 
         FI-eustorl RSfraktion 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

