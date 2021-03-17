&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tt_Underkategori NO-UNDO LIKE Underkategori.



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

DEFINE VARIABLE imkatid AS INTEGER     NO-UNDO.
DEFINE VARIABLE iMellankategori AS INTEGER     NO-UNDO.
DEFINE VARIABLE Mellbeskr  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE Underbeskr AS CHARACTER   NO-UNDO.

DEFINE BUFFER bufMellan FOR Mellankategori.
DEFINE BUFFER bufUnder FOR Underkategori.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BR-Mkat

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Mellankategori MellanUkat tt_Underkategori

/* Definitions for BROWSE BR-Mkat                                       */
&Scoped-define FIELDS-IN-QUERY-BR-Mkat Mellankategori.mkatid ~
Mellankategori.mkatbeskr 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BR-Mkat 
&Scoped-define QUERY-STRING-BR-Mkat FOR EACH Mellankategori NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BR-Mkat OPEN QUERY BR-Mkat FOR EACH Mellankategori NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BR-Mkat Mellankategori
&Scoped-define FIRST-TABLE-IN-QUERY-BR-Mkat Mellankategori


/* Definitions for BROWSE BR-Mukat                                      */
&Scoped-define FIELDS-IN-QUERY-BR-Mukat MellanUkat.mukatnr ~
MellanUkat.mkatid Mellbeskr MellanUkat.UnderKatNr Underbeskr 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BR-Mukat 
&Scoped-define QUERY-STRING-BR-Mukat FOR EACH MellanUkat ~
      WHERE MellanUkat.mkatid = imkatid NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BR-Mukat OPEN QUERY BR-Mukat FOR EACH MellanUkat ~
      WHERE MellanUkat.mkatid = imkatid NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BR-Mukat MellanUkat
&Scoped-define FIRST-TABLE-IN-QUERY-BR-Mukat MellanUkat


/* Definitions for BROWSE BR-Ukat                                       */
&Scoped-define FIELDS-IN-QUERY-BR-Ukat tt_Underkategori.UnderKatNr ~
tt_Underkategori.UnderKatTekst 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BR-Ukat 
&Scoped-define QUERY-STRING-BR-Ukat FOR EACH tt_Underkategori NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BR-Ukat OPEN QUERY BR-Ukat FOR EACH tt_Underkategori NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BR-Ukat tt_Underkategori
&Scoped-define FIRST-TABLE-IN-QUERY-BR-Ukat tt_Underkategori


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BR-Mkat}~
    ~{&OPEN-QUERY-BR-Mukat}~
    ~{&OPEN-QUERY-BR-Ukat}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BR-Mkat BR-Mukat BR-Ukat B-Ny B-Tabort 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Ny 
     LABEL "<- Ny" 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-Tabort 
     LABEL "Ta bort ->" 
     SIZE 15 BY 1.14.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BR-Mkat FOR 
      Mellankategori SCROLLING.

DEFINE QUERY BR-Mukat FOR 
      MellanUkat SCROLLING.

DEFINE QUERY BR-Ukat FOR 
      tt_Underkategori SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BR-Mkat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BR-Mkat C-Win _STRUCTURED
  QUERY BR-Mkat NO-LOCK DISPLAY
      Mellankategori.mkatid FORMAT ">>9":U
      Mellankategori.mkatbeskr FORMAT "x(30)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 54 BY 13.81 ROW-HEIGHT-CHARS .62 FIT-LAST-COLUMN.

DEFINE BROWSE BR-Mukat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BR-Mukat C-Win _STRUCTURED
  QUERY BR-Mukat NO-LOCK DISPLAY
      MellanUkat.mukatnr COLUMN-LABEL "Nr" FORMAT ">>9":U
      MellanUkat.mkatid FORMAT ">>9":U
      Mellbeskr COLUMN-LABEL "Mellankategori" FORMAT "x(30)":U
      MellanUkat.UnderKatNr FORMAT ">>9":U WIDTH 3.6
      Underbeskr COLUMN-LABEL "Underkategori" FORMAT "x(30)":U
            WIDTH 46.6
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 79 BY 13.81 FIT-LAST-COLUMN.

DEFINE BROWSE BR-Ukat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BR-Ukat C-Win _STRUCTURED
  QUERY BR-Ukat NO-LOCK DISPLAY
      tt_Underkategori.UnderKatNr FORMAT ">>9":U
      tt_Underkategori.UnderKatTekst FORMAT "x(30)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 52 BY 13.71 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BR-Mkat AT ROW 1.95 COL 4
     BR-Mukat AT ROW 1.95 COL 61.6
     BR-Ukat AT ROW 2.05 COL 162.6
     B-Ny AT ROW 6 COL 145.2
     B-Tabort AT ROW 7.43 COL 145
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 221.8 BY 15.43.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: tt_Underkategori T "?" NO-UNDO SkoTex Underkategori
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert window title>"
         HEIGHT             = 15.43
         WIDTH              = 221.8
         MAX-HEIGHT         = 32.57
         MAX-WIDTH          = 226.2
         VIRTUAL-HEIGHT     = 32.57
         VIRTUAL-WIDTH      = 226.2
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
   FRAME-NAME                                                           */
/* BROWSE-TAB BR-Mkat 1 DEFAULT-FRAME */
/* BROWSE-TAB BR-Mukat BR-Mkat DEFAULT-FRAME */
/* BROWSE-TAB BR-Ukat BR-Mukat DEFAULT-FRAME */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BR-Mkat
/* Query rebuild information for BROWSE BR-Mkat
     _TblList          = "SkoTex.Mellankategori"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   = SkoTex.Mellankategori.mkatid
     _FldNameList[2]   = SkoTex.Mellankategori.mkatbeskr
     _Query            is OPENED
*/  /* BROWSE BR-Mkat */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BR-Mukat
/* Query rebuild information for BROWSE BR-Mukat
     _TblList          = "SkoTex.MellanUkat"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "SkoTex.MellanUkat.mkatid = imkatid"
     _FldNameList[1]   > SkoTex.MellanUkat.mukatnr
"MellanUkat.mukatnr" "Nr" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   = SkoTex.MellanUkat.mkatid
     _FldNameList[3]   > "_<CALC>"
"Mellbeskr" "Mellankategori" "x(30)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > SkoTex.MellanUkat.UnderKatNr
"MellanUkat.UnderKatNr" ? ? "integer" ? ? ? ? ? ? no ? no no "3.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > "_<CALC>"
"Underbeskr" "Underkategori" "x(30)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BR-Mukat */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BR-Ukat
/* Query rebuild information for BROWSE BR-Ukat
     _TblList          = "Temp-Tables.tt_Underkategori"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   = Temp-Tables.tt_Underkategori.UnderKatNr
     _FldNameList[2]   = Temp-Tables.tt_Underkategori.UnderKatTekst
     _Query            is OPENED
*/  /* BROWSE BR-Ukat */
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


&Scoped-define SELF-NAME B-Ny
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Ny C-Win
ON CHOOSE OF B-Ny IN FRAME DEFAULT-FRAME /* <- Ny */
DO:
    DEFINE VARIABLE inr AS INTEGER     NO-UNDO.
  IF AVAIL Mellankategori AND AVAIL tt_Underkategori THEN DO:
      IF NOT CAN-FIND(MellanUkat WHERE MellanUkat.mkatid = Mellankategori.mkatid AND MellanUkat.UnderKatNr = tt_Underkategori.UnderKatNr) THEN DO:
          FIND LAST MellanUkat USE-INDEX mukatidx NO-LOCK NO-ERROR.
          inr = IF AVAIL MellanUkat THEN MellanUkat.mukatnr + 1 ELSE 1.
          CREATE MellanUkat.
          ASSIGN MellanUkat.mukatnr    = inr
                 MellanUkat.mkatid     = Mellankategori.mkatid
                 MellanUkat.UnderKatNr = tt_Underkategori.UnderKatNr.
          RUN initTTunderkategori.
          {&OPEN-QUERY-BR-Mukat}
          {&OPEN-QUERY-BR-Ukat}
      END.

  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Tabort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Tabort C-Win
ON CHOOSE OF B-Tabort IN FRAME DEFAULT-FRAME /* Ta bort -> */
DO:
    FIND FIRST ArtBasMUkategori WHERE ArtBasMUkategori.mukatnr = MellanUkat.mukatnr NO-LOCK NO-ERROR.
    IF AVAIL ArtBasMUkategori THEN DO:
        MESSAGE "Registrerad på en eller flera artiklar, bl.a: "  ArtBasMUkategori.artikkelnr
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.
    ELSE DO:
        FIND CURRENT MellanUkat.
        DELETE MellanUkat.
        RUN initTTunderkategori.
        {&OPEN-QUERY-BR-Mukat}
        {&OPEN-QUERY-BR-Ukat}
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BR-Mkat
&Scoped-define SELF-NAME BR-Mkat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BR-Mkat C-Win
ON VALUE-CHANGED OF BR-Mkat IN FRAME DEFAULT-FRAME
DO:
  imkatid = IF AVAIL mellankategori THEN mellankategori.mkatid ELSE 0.

  {&OPEN-QUERY-BR-Mukat}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BR-Mukat
&Scoped-define SELF-NAME BR-Mukat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BR-Mukat C-Win
ON ROW-DISPLAY OF BR-Mukat IN FRAME DEFAULT-FRAME
DO:
    FIND bufMellan WHERE bufMellan.mkatid = Mellanukat.mkatid NO-LOCK NO-ERROR.
    Mellbeskr = IF AVAIL bufMellan THEN bufMellan.mkatbeskr ELSE "Okänd".
    FIND bufUnder WHERE bufUnder.underkatnr = Mellanukat.underkatnr NO-LOCK NO-ERROR.
    Underbeskr = IF AVAIL bufUnder THEN bufUnder.UnderKatTekst ELSE "Okänd".

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BR-Mkat
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
    RUN initTTunderkategori.
  RUN enable_UI.
  IF BROWSE BR-Mkat:FOCUSED-ROW <> ? THEN
      APPLY "VALUE-CHANGED" TO BROWSE BR-Mkat.
    {syspara.i 2 4 50 iMellankategori INT}
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
  ENABLE BR-Mkat BR-Mukat BR-Ukat B-Ny B-Tabort 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initTTunderkategori C-Win 
PROCEDURE initTTunderkategori :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE tt_Underkategori.
    FOR EACH underkategori NO-LOCK WHERE underkategori.UnderKatNr > 0 AND NOT
        CAN-FIND(FIRST MellanUkat WHERE MellanUkat.UnderKatNr = underkategori.UnderKatNr):
        CREATE tt_Underkategori.
        BUFFER-COPY Underkategori TO tt_Underkategori.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

