&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
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
DEF VAR pbOk AS LOG NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FI-FraDato B-OkRapp FI-TilDato B-FilMottak ~
RECT-1 RECT-2 
&Scoped-Define DISPLAYED-OBJECTS FI-FraDato FI-TilDato 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-FilMottak 
     LABEL "Slett alle data i filmottket" 
     SIZE 47 BY 1.14.

DEFINE BUTTON B-OkRapp 
     LABEL "Slett økonomiske rapporter i periode" 
     SIZE 47 BY 1.14.

DEFINE VARIABLE FI-FraDato AS DATE FORMAT "99/99/99":U 
     LABEL "Fra" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-TilDato AS DATE FORMAT "99/99/99":U 
     LABEL "Til" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 77 BY 2.62.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 77 BY 2.14.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     FI-FraDato AT ROW 2.19 COL 12 COLON-ALIGNED
     B-OkRapp AT ROW 2.19 COL 31
     FI-TilDato AT ROW 3.14 COL 12 COLON-ALIGNED
     B-FilMottak AT ROW 5.67 COL 31
     RECT-1 AT ROW 1.71 COL 3
     RECT-2 AT ROW 5.19 COL 3
     "Økonomiske rapporter" VIEW-AS TEXT
          SIZE 27 BY .62 AT ROW 1.05 COL 4
     "Filmottaket" VIEW-AS TEXT
          SIZE 27 BY .62 AT ROW 4.48 COL 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 6.81.


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
         TITLE              = "Rens for test av salgsdata"
         HEIGHT             = 6.81
         WIDTH              = 80
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 80
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
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Rens for test av salgsdata */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Rens for test av salgsdata */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-FilMottak
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-FilMottak C-Win
ON CHOOSE OF B-FilMottak IN FRAME DEFAULT-FRAME /* Slett alle data i filmottket */
DO:
  RUN SlettFilMottak.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-OkRapp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-OkRapp C-Win
ON CHOOSE OF B-OkRapp IN FRAME DEFAULT-FRAME /* Slett økonomiske rapporter i periode */
DO:
  RUN SlettOkonomi.
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
  DISPLAY FI-FraDato FI-TilDato 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE FI-FraDato B-OkRapp FI-TilDato B-FilMottak RECT-1 RECT-2 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettFilMottak C-Win 
PROCEDURE SlettFilMottak :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
pbOk = FALSE.
MESSAGE "Skal alle data i filmottaket slettes?"
    VIEW-AS ALERT-BOX INFO BUTTONS YES-NO 
    UPDATE pbOk.
IF pbOk = FALSE THEN
    RETURN.

FOR EACH Filer:
    Oppdatert = false.
END.
FOR EACH fillinjer:
    behandlet = FALSE.
END.
FOR EACH DataSett:
    DELETE DataSett.

END.
FOR EACH BongHode:
    FOR EACH BongLinje OF BongHode:
        DELETE BongLinje.
    END.
    DELETE BongHode.
END.
MESSAGE "Alle Filer, fillinjer, datasettt og kvitteringer slettet fra filmottaket." 
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettOkonomi C-Win 
PROCEDURE SlettOkonomi :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
    assign 
        pbOk = FALSE
        FI-FraDato
        FI-TilDato
        .
END.

IF FI-FraDato > FI-TilDato OR 
    FI-FraDato = ? or
    FI-TilDato = ? THEN
DO:
    MESSAGE "Feil datoangivelse."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
END.

MESSAGE "Skal økonomiske data i periode " FI-FraDato FI-TilDato "slettes?"
    VIEW-AS ALERT-BOX INFO BUTTONS YES-NO 
    UPDATE pbOk.
IF pbOk = FALSE THEN
    RETURN.

/* aktivitetsrapporten */
FOR EACH Akt_Rapp WHERE
    Akt_Rapp.dato >= FI-FraDato AND
    Akt_Rapp.dato <= FI-TilDato:

    DELETE Akt_Rapp.
END.

/* Dagsrapporten */
FOR EACH dags_rap WHERE
    Dags_Rap.Dato >= FI-FraDato AND
    Dags_Rap.Dato <= FI-TilDato:

    DELETE Dags_Rap.
END.

/* Kassarapporten */
FOR EACH kas_rap WHERE
    kas_rap.dato >= FI-FraDato AND
    kas_rap.dato <= FI-TilDato:

    DELETE kas_rap.
END.

/* Kontering */
FOR EACH konto WHERE
    konto.dato >= FI-FraDato AND
    konto.dato <= FI-TilDato:

    DELETE konto.
END.

/* KundeTrans */
FOR EACH KundeTrans WHERE
    KundeTrans.RegistrertDato >= FI-FraDato AND
    KundeTrans.RegistrertDato <= FI-TilDato:

    DELETE kundeTrans.
END.

/* Betalingstranser */
FOR EACH KundeBetTrans WHERE
    KundeBetTrans.RegistrertDato >= FI-FraDato AND
    KundeBetTrans.RegistrertDato <= FI-TilDato:

    DELETE KundeBetTrans.
END.


MESSAGE "Økonomiske data i periode " FI-FraDato FI-TilDato "er slettet." SKIP(1)
        "Leverandørreklamasjoner er ikke nullstilt." SKIP
        "TRansLogg er ikke nullstilt." SKIP
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

