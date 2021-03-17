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

DEFINE VARIABLE iMellankategori AS INTEGER     NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BR-Mellankat

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Mellankategori

/* Definitions for BROWSE BR-Mellankat                                  */
&Scoped-define FIELDS-IN-QUERY-BR-Mellankat Mellankategori.mkatid ~
Mellankategori.mkatbeskr 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BR-Mellankat 
&Scoped-define QUERY-STRING-BR-Mellankat FOR EACH Mellankategori NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BR-Mellankat OPEN QUERY BR-Mellankat FOR EACH Mellankategori NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BR-Mellankat Mellankategori
&Scoped-define FIRST-TABLE-IN-QUERY-BR-Mellankat Mellankategori


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BR-Mellankat}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-28 RECT-74 RECT-29 B-Ny B-Tabort B-Undo ~
B-Lagra BR-Mellankat FI-mkatid FI-mkatbeskr 
&Scoped-Define DISPLAYED-OBJECTS FI-mkatid FI-mkatbeskr 

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
     LABEL "&Lagra" 
     SIZE 4.8 BY 1.14 TOOLTIP "Alt-L".

DEFINE BUTTON B-Ny 
     IMAGE-UP FILE "icon/e-ny.bmp":U
     LABEL "&Ny" 
     SIZE 4.8 BY 1.14 TOOLTIP "Alt-N".

DEFINE BUTTON B-Tabort 
     IMAGE-UP FILE "icon/e-del.bmp":U
     LABEL "&Ta bort" 
     SIZE 4.8 BY 1.14 TOOLTIP "Alt-T".

DEFINE BUTTON B-Undo 
     IMAGE-UP FILE "icon/e-undo.bmp":U
     LABEL "&Ångra" 
     SIZE 4.8 BY 1.14 TOOLTIP "Alt-Å".

DEFINE VARIABLE FI-mkatbeskr AS CHARACTER FORMAT "x(30)" 
     LABEL "Mellankategori" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE FI-mkatid AS INTEGER FORMAT ">>9" INITIAL 0 
     LABEL "Id" 
     VIEW-AS FILL-IN 
     SIZE 6.2 BY 1.

DEFINE RECTANGLE RECT-28
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 110 BY .19.

DEFINE RECTANGLE RECT-29
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 110 BY .19.

DEFINE RECTANGLE RECT-74
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 55 BY 5.48.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BR-Mellankat FOR 
      Mellankategori SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BR-Mellankat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BR-Mellankat C-Win _STRUCTURED
  QUERY BR-Mellankat NO-LOCK DISPLAY
      Mellankategori.mkatid FORMAT ">>9":U
      Mellankategori.mkatbeskr FORMAT "x(30)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 49 BY 17.14 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     B-Ny AT ROW 1.24 COL 2
     B-Tabort AT ROW 1.24 COL 8.4
     B-Undo AT ROW 1.24 COL 14.8
     B-Lagra AT ROW 1.24 COL 21.2
     BR-Mellankat AT ROW 3.14 COL 2
     FI-mkatid AT ROW 4.81 COL 69 COLON-ALIGNED
     FI-mkatbeskr AT ROW 5.81 COL 69 COLON-ALIGNED
     RECT-28 AT ROW 2.48 COL 1.6
     RECT-74 AT ROW 3.29 COL 54
     RECT-29 AT ROW 1 COL 1.6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 111.2 BY 20.52.


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
         TITLE              = "Mellankategori"
         HEIGHT             = 19.71
         WIDTH              = 111.8
         MAX-HEIGHT         = 20.52
         MAX-WIDTH          = 111.8
         VIRTUAL-HEIGHT     = 20.52
         VIRTUAL-WIDTH      = 111.8
         MIN-BUTTON         = no
         MAX-BUTTON         = no
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
/* BROWSE-TAB BR-Mellankat B-Lagra DEFAULT-FRAME */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BR-Mellankat
/* Query rebuild information for BROWSE BR-Mellankat
     _TblList          = "SkoTex.Mellankategori"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   = SkoTex.Mellankategori.mkatid
     _FldNameList[2]   = SkoTex.Mellankategori.mkatbeskr
     _Query            is OPENED
*/  /* BROWSE BR-Mellankat */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Mellankategori */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Mellankategori */
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
    DEFINE VARIABLE lModified AS LOGICAL INIT TRUE    NO-UNDO.
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN INPUT FI-mkatid
               INPUT FI-mkatbeskr.
        IF FI-mkatid = 0 THEN DO:
            MESSAGE "Ogiltigt värde '0'"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            APPLY "ENTRY" TO FI-mkatid.
            RETURN NO-APPLY.
        END.
        IF NOT CAN-FIND(Mellankategori WHERE Mellankategori.mkatid = FI-mkatid) THEN DO:
            CREATE Mellankategori.
            ASSIGN Mellankategori.mkatid = FI-mkatid
                   Mellankategori.mkatbeskr = FI-mkatbeskr.
            {&OPEN-QUERY-{&BROWSE-NAME}}
        END.
        ELSE IF NOT FI-mkatid:SENSITIVE THEN DO:
            FIND CURRENT Mellankategori.
            Mellankategori.mkatbeskr = FI-mkatbeskr.
            FIND CURRENT Mellankategori NO-LOCK.
            BROWSE {&BROWSE-NAME}:REFRESH().
        END.
        ELSE DO:
            MESSAGE "Mellankategori" FI-mkatid "redan registrerad"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            lModified = FALSE.
        END.
        IF lModified THEN
            RUN fixElogg ("MOD",FI-mkatid).
        FIND Mellankategori WHERE Mellankategori.mkatid = FI-mkatid NO-LOCK.
        REPOSITION {&BROWSE-NAME} TO ROWID ROWID(Mellankategori).
        APPLY "VALUE-CHANGED" TO BROWSE {&BROWSE-NAME}.

        FI-mkatid:SENSITIVE = FALSE.
/*         FI-mkatbeskr:SENSITIVE = FALSE. */
        B-Tabort:SENSITIVE = TRUE.
    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Ny
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Ny C-Win
ON CHOOSE OF B-Ny IN FRAME DEFAULT-FRAME /* Ny */
DO:
  FI-mkatid:SENSITIVE = TRUE.
  FI-mkatid:SCREEN-VALUE = "0".
  FI-mkatbeskr:SENSITIVE = TRUE.
  FI-mkatbeskr:SCREEN-VALUE = "".
  B-Tabort:SENSITIVE = FALSE.
  B-Undo:SENSITIVE = TRUE.
  APPLY "ENTRY" TO FI-mkatid.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Tabort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Tabort C-Win
ON CHOOSE OF B-Tabort IN FRAME DEFAULT-FRAME /* Ta bort */
DO:
    DEFINE VARIABLE cEU AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE iId AS INTEGER     NO-UNDO.
    IF  CAN-FIND(FIRST MellanUkat WHERE MellanUkat.mkatid = Mellankategori.mkatid) THEN DO:
/*   IF CAN-FIND(FIRST artbas WHERE artbas.mkatid = Mellankategori.mkatid) THEN DO: */
      MESSAGE "Kan inte tas bort." SKIP "Mellankategorien är kopplad till Underkategori."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.
  ELSE DO:
      MESSAGE "Bekräfta borttad"
          VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lOK AS LOG.
      IF lOK THEN DO:
          FIND CURRENT Mellankategori.
          iId = Mellankategori.mkatid.
          DELETE Mellankategori.
          BROWSE {&BROWSE-NAME}:REFRESH().
          IF BROWSE {&BROWSE-NAME}:FOCUSED-ROW = ? THEN
              ASSIGN FI-mkatid:SCREEN-VALUE = "0"
                     FI-mkatbeskr:SCREEN-VALUE = "".
/*           RUN fixElogg ("DEL",iId). */
      END.
      ELSE
          RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Undo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Undo C-Win
ON CHOOSE OF B-Undo IN FRAME DEFAULT-FRAME /* Ångra */
DO:
    {&OPEN-QUERY-{&BROWSE-NAME}}
    IF BROWSE BR-Mellankat:FOCUSED-ROW <> ? THEN DO:
        APPLY "VALUE-CHANGED" TO BROWSE {&BROWSE-NAME}.
        B-Tabort:SENSITIVE = TRUE.
    END.
    FI-mkatid:SENSITIVE = FALSE.
    FI-mkatbeskr:SENSITIVE = FALSE.
    B-Undo:SENSITIVE = FALSE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BR-Mellankat
&Scoped-define SELF-NAME BR-Mellankat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BR-Mellankat C-Win
ON ENTRY OF BR-Mellankat IN FRAME DEFAULT-FRAME
DO:
    IF B-Undo:SENSITIVE THEN DO:
        APPLY "CHOOSE" TO B-Undo.
        RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BR-Mellankat C-Win
ON VALUE-CHANGED OF BR-Mellankat IN FRAME DEFAULT-FRAME
DO:
  FI-mkatid:SCREEN-VALUE    = STRING(Mellankategori.mkatid).
  FI-mkatbeskr:SCREEN-VALUE = Mellankategori.mkatbeskr.
  FI-mkatbeskr:SENSITIVE = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-mkatbeskr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-mkatbeskr C-Win
ON ANY-PRINTABLE OF FI-mkatbeskr IN FRAME DEFAULT-FRAME /* Mellankategori */
DO:
    B-Lagra:SENSITIVE = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-mkatbeskr C-Win
ON BACKSPACE OF FI-mkatbeskr IN FRAME DEFAULT-FRAME /* Mellankategori */
DO:
  IF LENGTH(INPUT fi-mkatbeskr) = 1 THEN
      B-Lagra:SENSITIVE = FALSE.
  ELSE
      B-Lagra:SENSITIVE = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-mkatid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-mkatid C-Win
ON RETURN OF FI-mkatid IN FRAME DEFAULT-FRAME /* Id */
OR "TAB" OF FI-mkatid DO:
    DO WITH FRAME {&FRAME-NAME}:
        IF INPUT FI-mkatid > 0 THEN DO:
            APPLY "ENTRY" TO FI-mkatbeskr.
            RETURN NO-APPLY.
        END.
    END.
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
{syspara.i 2 4 50 iMellankategori INT}
/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  FI-mkatid:SENSITIVE = FALSE.
  FI-mkatbeskr:SENSITIVE = FALSE.
  B-Undo:SENSITIVE = FALSE.
  B-Lagra:SENSITIVE = FALSE.
  IF BROWSE BR-Mellankat:FOCUSED-ROW <> ? THEN
      APPLY "VALUE-CHANGED" TO BROWSE BR-Mellankat.
  IF iMellankategori = 0 THEN
      MESSAGE "Systemet är satt upp att inte använda Mellankategori"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
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
  DISPLAY FI-mkatid FI-mkatbeskr 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-28 RECT-74 RECT-29 B-Ny B-Tabort B-Undo B-Lagra BR-Mellankat 
         FI-mkatid FI-mkatbeskr 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fixElogg C-Win 
PROCEDURE fixElogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER cTyp AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER iId AS INTEGER     NO-UNDO.
    FIND ELogg WHERE 
         ELogg.TabellNavn     = "Mellankategori" AND
         ELogg.EksterntSystem = "WEBBUT"    AND
         ELogg.Verdier        = STRING(iId) NO-ERROR.
    IF NOT AVAIL Elogg THEN DO:
        CREATE Elogg.
        ASSIGN ELogg.TabellNavn     = "Mellankategori"
               ELogg.EksterntSystem = "WEBBUT"   
               ELogg.Verdier        = STRING(iId).
    END.
    ASSIGN ELogg.EndringsType = IF cTyp = "MOD" THEN 1 ELSE 1. /* tilsvidare ingen del */
           ELogg.Behandlet    = FALSE.
    RELEASE ELogg.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

