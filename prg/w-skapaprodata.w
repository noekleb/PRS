&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE TT_Butiker NO-UNDO LIKE Butiker.
DEFINE TEMP-TABLE TT_ValgteButiker NO-UNDO LIKE Butiker.



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
DEFINE VARIABLE lFinansPro    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE lFinansPreem  AS LOGICAL    NO-UNDO.
DEFINE VARIABLE cFinansProDir AS CHARACTER  NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BR-TTButiker

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES TT_Butiker TT_ValgteButiker

/* Definitions for BROWSE BR-TTButiker                                  */
&Scoped-define FIELDS-IN-QUERY-BR-TTButiker TT_Butiker.Butik ~
TT_Butiker.ButNamn 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BR-TTButiker 
&Scoped-define QUERY-STRING-BR-TTButiker FOR EACH TT_Butiker NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BR-TTButiker OPEN QUERY BR-TTButiker FOR EACH TT_Butiker NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BR-TTButiker TT_Butiker
&Scoped-define FIRST-TABLE-IN-QUERY-BR-TTButiker TT_Butiker


/* Definitions for BROWSE BR-TTValgteButiker                            */
&Scoped-define FIELDS-IN-QUERY-BR-TTValgteButiker TT_ValgteButiker.Butik ~
TT_ValgteButiker.ButNamn 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BR-TTValgteButiker 
&Scoped-define QUERY-STRING-BR-TTValgteButiker FOR EACH TT_ValgteButiker NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BR-TTValgteButiker OPEN QUERY BR-TTValgteButiker FOR EACH TT_ValgteButiker NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BR-TTValgteButiker TT_ValgteButiker
&Scoped-define FIRST-TABLE-IN-QUERY-BR-TTValgteButiker TT_ValgteButiker


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BR-TTButiker}~
    ~{&OPEN-QUERY-BR-TTValgteButiker}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-56 FI-ButikkNr FI-DatoFr FI-DatoTi ~
BUTTON-1 BR-TTButiker BR-TTValgteButiker BUTTON-LeggTil BUTTON-AlleTil ~
EDITOR-1 FI-Typ FILL-IN-19 FILL-IN-18 
&Scoped-Define DISPLAYED-OBJECTS FI-ButikkNr FI-ButNamn TG-FinansPro ~
FI-DatoFr FI-DatoTi TG-FinansPreem EDITOR-1 FI-Typ FILL-IN-19 FILL-IN-18 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-1 
     LABEL "Prodata" 
     SIZE 15 BY 1.15.

DEFINE BUTTON BUTTON-AlleFra 
     LABEL "<< &Alla bort" 
     SIZE 15 BY 1.12.

DEFINE BUTTON BUTTON-AlleTil 
     LABEL ">> A&lla till" 
     SIZE 15 BY 1.12.

DEFINE BUTTON BUTTON-LeggTil 
     LABEL "Lägg till &>>" 
     SIZE 15 BY 1.12.

DEFINE BUTTON BUTTON-TrekkFra 
     LABEL "&<< Ta bort" 
     SIZE 15 BY 1.12.

DEFINE VARIABLE EDITOR-1 AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 83 BY 14.04 NO-UNDO.

DEFINE VARIABLE FI-ButikkNr AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "Butikknummer" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-ButNamn AS CHARACTER FORMAT "x(20)" 
     VIEW-AS FILL-IN 
     SIZE 41 BY 1.

DEFINE VARIABLE FI-DatoFr AS DATE FORMAT "99/99/99" 
     LABEL "Datum från/till" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-DatoTi AS DATE FORMAT "99/99/99" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Typ AS CHARACTER FORMAT "X(256)":U INITIAL "      Typ" 
      VIEW-AS TEXT 
     SIZE 14 BY .62
     BGCOLOR 11  NO-UNDO.

DEFINE VARIABLE FILL-IN-18 AS CHARACTER FORMAT "X(256)":U INITIAL " Valda butiker" 
      VIEW-AS TEXT 
     SIZE 16 BY .62
     BGCOLOR 11  NO-UNDO.

DEFINE VARIABLE FILL-IN-19 AS CHARACTER FORMAT "X(256)":U INITIAL " Tillgängliga butiker" 
      VIEW-AS TEXT 
     SIZE 19.86 BY .62
     BGCOLOR 11  NO-UNDO.

DEFINE RECTANGLE RECT-56
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 68 BY 3.85.

DEFINE VARIABLE TG-FinansPreem AS LOGICAL INITIAL no 
     LABEL "Finans til Preem" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY .81 NO-UNDO.

DEFINE VARIABLE TG-FinansPro AS LOGICAL INITIAL no 
     LABEL "Eksport finans til Pro" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BR-TTButiker FOR 
      TT_Butiker SCROLLING.

DEFINE QUERY BR-TTValgteButiker FOR 
      TT_ValgteButiker SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BR-TTButiker
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BR-TTButiker C-Win _STRUCTURED
  QUERY BR-TTButiker NO-LOCK DISPLAY
      TT_Butiker.Butik FORMAT ">>>>>9":U
      TT_Butiker.ButNamn FORMAT "x(20)":U WIDTH 37.14
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 55 BY 5.81 ROW-HEIGHT-CHARS .62 FIT-LAST-COLUMN.

DEFINE BROWSE BR-TTValgteButiker
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BR-TTValgteButiker C-Win _STRUCTURED
  QUERY BR-TTValgteButiker NO-LOCK DISPLAY
      TT_ValgteButiker.Butik FORMAT ">>>>>9":U WIDTH 12.43
      TT_ValgteButiker.ButNamn FORMAT "x(20)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 63.86 BY 5.81 ROW-HEIGHT-CHARS .62 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     FI-ButikkNr AT ROW 2.19 COL 16 COLON-ALIGNED HELP
          "Butikknummer."
     FI-ButNamn AT ROW 2.19 COL 31 COLON-ALIGNED HELP
          "Butikkens navn" NO-LABEL
     TG-FinansPro AT ROW 2.85 COL 79.43
     FI-DatoFr AT ROW 3.62 COL 15.43 COLON-ALIGNED HELP
          "Dato for utstedelse av bongen"
     FI-DatoTi AT ROW 3.62 COL 31.14 COLON-ALIGNED HELP
          "Dato for utstedelse av bongen" NO-LABEL
     BUTTON-1 AT ROW 3.62 COL 49
     TG-FinansPreem AT ROW 4.92 COL 79.43
     BR-TTButiker AT ROW 7.04 COL 3
     BR-TTValgteButiker AT ROW 7.04 COL 77.14
     BUTTON-LeggTil AT ROW 7.85 COL 60.14
     BUTTON-AlleTil AT ROW 9 COL 60.14
     BUTTON-TrekkFra AT ROW 10.15 COL 60.14
     BUTTON-AlleFra AT ROW 11.23 COL 60.14
     EDITOR-1 AT ROW 13.38 COL 3 NO-LABEL
     FI-Typ AT ROW 1.92 COL 85.57 COLON-ALIGNED NO-LABEL
     FILL-IN-19 AT ROW 6.23 COL 12 COLON-ALIGNED NO-LABEL
     FILL-IN-18 AT ROW 6.23 COL 75 COLON-ALIGNED NO-LABEL
     RECT-56 AT ROW 2.15 COL 75
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 143.8 BY 29.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: TT_Butiker T "?" NO-UNDO SkoTex Butiker
      TABLE: TT_ValgteButiker T "?" NO-UNDO SkoTex Butiker
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Skapa data till PRO/VS"
         HEIGHT             = 29
         WIDTH              = 143.86
         MAX-HEIGHT         = 29
         MAX-WIDTH          = 143.86
         VIRTUAL-HEIGHT     = 29
         VIRTUAL-WIDTH      = 143.86
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
/* BROWSE-TAB BR-TTButiker TG-FinansPreem DEFAULT-FRAME */
/* BROWSE-TAB BR-TTValgteButiker BR-TTButiker DEFAULT-FRAME */
/* SETTINGS FOR BUTTON BUTTON-AlleFra IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-TrekkFra IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-ButNamn IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-FinansPreem IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-FinansPro IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BR-TTButiker
/* Query rebuild information for BROWSE BR-TTButiker
     _TblList          = "Temp-Tables.TT_Butiker"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   = Temp-Tables.TT_Butiker.Butik
     _FldNameList[2]   > Temp-Tables.TT_Butiker.ButNamn
"TT_Butiker.ButNamn" ? ? "character" ? ? ? ? ? ? no ? no no "37.14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BR-TTButiker */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BR-TTValgteButiker
/* Query rebuild information for BROWSE BR-TTValgteButiker
     _TblList          = "Temp-Tables.TT_ValgteButiker"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.TT_ValgteButiker.Butik
"TT_ValgteButiker.Butik" ? ? "integer" ? ? ? ? ? ? no ? no no "12.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   = Temp-Tables.TT_ValgteButiker.ButNamn
     _Query            is OPENED
*/  /* BROWSE BR-TTValgteButiker */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Skapa data till PRO/VS */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Skapa data till PRO/VS */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 C-Win
ON CHOOSE OF BUTTON-1 IN FRAME DEFAULT-FRAME /* Prodata */
DO:
    DEFINE VARIABLE dDatoLoop AS DATE       NO-UNDO.
    ASSIGN FI-ButikkNr 
           FI-DatoFr
           FI-DatoTi.
    IF TG-FinansPreem:CHECKED = FALSE AND TG-FinansPro:CHECKED = FALSE THEN DO:
        MESSAGE
        "Välj mins1 1 typ"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
    IF NOT CAN-FIND(FIRST TT_ValgteButiker) THEN DO:
        FIND Butiker WHERE Butiker.butik = FI-ButikkNr NO-LOCK NO-ERROR.
        IF NOT AVAIL Butiker THEN DO:
            MESSAGE
                "Finner inte butiken"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN.
        END.
        FI-ButNamn:SCREEN-VALUE = Butiker.butnamn.
        BR-TTButiker:DESELECT-FOCUSED-ROW().
        FIND TT_Butiker WHERE TT_Butiker.Butik = FI-ButikkNr.
        REPOSITION BR-TTButiker TO ROWID ROWID(TT_Butiker) NO-ERROR.
        BR-TTButiker:SELECT-FOCUSED-ROW().
        RUN TilFra("Til").
        PROCESS EVENTS.
    END.
    IF FI-DatoFr = ? OR FI-DatoTi <> ? AND FI-DatoTi < FI-DatoFr THEN DO:
        MESSAGE 
             "Fel dato"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
    {sww.i}
    FOR EACH TT_ValgteButiker:
        EDITOR-1:SCREEN-VALUE = "".
        REPOSITION BR-TTValgteButiker TO ROWID ROWID(TT_ValgteButiker) NO-ERROR.
        BR-TTValgteButiker:SELECT-FOCUSED-ROW().
        PROCESS EVENTS.
        DATOLOOP:
        DO dDatoLoop = FI-DatoFr TO IF FI-DatoTi = ? THEN FI-DatoFr ELSE FI-DatoTi:
          EDITOR-1:INSERT-STRING("Behandlar data för butik" + string(TT_ValgteButiker.Butik) + " - " + STRING(dDatoLoop) + CHR(10)).
          PROCESS EVENTS.
          FOR EACH Kasse WHERE Kasse.ButikkNr = TT_ValgteButiker.Butik AND Kasse.GruppeNr = 1 NO-LOCK:
            IF CAN-FIND(FIRST BongHode WHERE BongHode.ButikkNr = TT_ValgteButiker.Butik  AND
                                             Bonghode.GruppeNr = Kasse.GruppeNr AND
                                             Bonghode.KasseNr  = Kasse.KasseNr  AND
                                             bonghode.dato     = dDatoLoop) THEN DO:
                RUN oppdaterPro.p (TT_ValgteButiker.Butik,dDatoLoop,TG-FinansPro:CHECKED,TG-FinansPreem:CHECKED,cFinansProDir).
/*                 RUN oppdaterpro.p (TT_ValgteButiker.Butik,dDatoLoop). */
                NEXT DATOLOOP.
            END.
          END.
        END.
        APPLY "CHOOSE" TO BUTTON-TrekkFra.
        PROCESS EVENTS.
    END.
    {swn.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-AlleFra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-AlleFra C-Win
ON CHOOSE OF BUTTON-AlleFra IN FRAME DEFAULT-FRAME /* << Alla bort */
DO:
    Run TilFra("AlleFra").
    ASSIGN BUTTON-LeggTil:SENSITIVE IN FRAME {&FRAME-NAME} = BROWSE BR-TTButiker:FOCUSED-ROW <> ?
           BUTTON-TrekkFra:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           BUTTON-AlleTil:SENSITIVE = BR-TTButiker:FOCUSED-ROW <> ?

           SELF:SENSITIVE = NO.
    APPLY "ENTRY" TO BR-TTButiker.
    BR-TTButiker:SELECT-FOCUSED-ROW().
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-AlleTil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-AlleTil C-Win
ON CHOOSE OF BUTTON-AlleTil IN FRAME DEFAULT-FRAME /* >> Alla till */
DO:
    Run TilFra("AlleTil").
    ASSIGN BUTTON-LeggTil:SENSITIVE IN FRAME {&FRAME-NAME} = FALSE
           BUTTON-TrekkFra:SENSITIVE IN FRAME {&FRAME-NAME} = YES
           BUTTON-AlleFra:SENSITIVE IN FRAME {&FRAME-NAME} = YES
           SELF:SENSITIVE = NO.
    APPLY "ENTRY" TO BR-TTValgteButiker.
    BR-TTValgteButiker:SELECT-FOCUSED-ROW().
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-LeggTil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-LeggTil C-Win
ON CHOOSE OF BUTTON-LeggTil IN FRAME DEFAULT-FRAME /* Lägg till >> */
DO:
     RUN TilFra("Til").
     IF BR-TTButiker:FOCUSED-ROW = ? THEN DO:
         ASSIGN SELF:SENSITIVE = FALSE.
         APPLY "ENTRY" TO BR-TTValgteButiker.
     END.
     ELSE DO:
         BR-TTButiker:SELECT-FOCUSED-ROW().
         APPLY "ENTRY" TO BR-TTButiker.
     END.
     ASSIGN BUTTON-AlleFra:SENSITIVE = BR-TTValgteButiker:FOCUSED-ROW <> ?
            BUTTON-AlleTil:SENSITIVE = BR-TTButiker:FOCUSED-ROW <> ?
            BUTTON-TrekkFra:SENSITIVE = BUTTON-AlleFra:SENSITIVE.
/*      ASSIGN SELF:SENSITIVE = BR-TTButiker:SELECT-NEXT-ROW()       */
/*             BUTTON-AlleFra:SENSITIVE IN FRAME {&FRAME-NAME} = YES */
/*             BUTTON-TrekkFra:SENSITIVE = YES.                      */
/*      IF BR-TTButiker:FOCUSED-ROW <> ? THEN                        */
/*         APPLY "ENTRY" TO BROWSE BR-TTButiker.                     */
/*      ELSE                                                         */
/*          APPLY "ENTRY" TO BROWSE BR-TTValgteButiker.              */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-TrekkFra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-TrekkFra C-Win
ON CHOOSE OF BUTTON-TrekkFra IN FRAME DEFAULT-FRAME /* << Ta bort */
DO:
     RUN TilFra("Fra").
     IF BR-TTValgteButiker:FOCUSED-ROW = ? THEN DO:
         ASSIGN SELF:SENSITIVE = FALSE
                BUTTON-AlleFra:SENSITIVE = FALSE.
         APPLY "ENTRY" TO BR-TTButiker.
         BR-TTButiker:SELECT-FOCUSED-ROW().
     END.
     ELSE DO:
         BR-TTValgteButiker:SELECT-FOCUSED-ROW().
         APPLY "ENTRY" TO BR-TTValgteButiker.
     END.
     ASSIGN BUTTON-LeggTil:SENSITIVE = BR-TTButiker:FOCUSED-ROW <> ?
            BUTTON-AlleTil:SENSITIVE = BR-TTButiker:FOCUSED-ROW <> ?
            .
/*      IF BR-TTValgteButiker:FOCUSED-ROW <> ? THEN DO:                     */
/*          ASSIGN SELF:SENSITIVE = BR-TTValgteButiker:SELECT-FOCUSED-ROW() */
/*                 BUTTON-AlleFra:SENSITIVE IN FRAME {&FRAME-NAME} = YES.   */
/*          APPLY "ENTRY" TO BR-TTValgteButiker.                            */
/*      END.                                                                */
/*      ELSE DO:                                                            */
/*          ASSIGN SELF:SENSITIVE = NO                                      */
/*                 BUTTON-AlleFra:SENSITIVE IN FRAME {&FRAME-NAME} = NO     */
/*                 BUTTON-LeggTil:SENSITIVE IN FRAME {&FRAME-NAME} =        */
/*                                   BR-TTButiker:NUM-SELECTED-ROWS > 0.    */
/*          APPLY "ENTRY" TO BR-TTButiker.                                  */
/*      END.                                                                */
/*     RETURN NO-APPLY.                                                     */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BR-TTButiker
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
  RUN SkapaTT_Butiker.
  RUN enable_UI.
  RUN InitToggle.
  APPLY "ENTRY" TO BR-TTButiker.
  BR-TTButiker:SELECT-FOCUSED-ROW().

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
  DISPLAY FI-ButikkNr FI-ButNamn TG-FinansPro FI-DatoFr FI-DatoTi TG-FinansPreem 
          EDITOR-1 FI-Typ FILL-IN-19 FILL-IN-18 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-56 FI-ButikkNr FI-DatoFr FI-DatoTi BUTTON-1 BR-TTButiker 
         BR-TTValgteButiker BUTTON-LeggTil BUTTON-AlleTil EDITOR-1 FI-Typ 
         FILL-IN-19 FILL-IN-18 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initToggle C-Win 
PROCEDURE initToggle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cTekst AS CHARACTER  NO-UNDO.
  /* Kontroll av om Export till Pro skall köras */
  ASSIGN cTekst = "".
  {syspara.i 200 2 100 cTekst}
  IF CAN-DO("1,yes,true,ja",cTekst) THEN
      lFinansPro = TRUE.
  ELSE
      lFinansPro = FALSE.
  IF lFinansPro = TRUE THEN DO:
      {syspar2.i 200 2 100 cFinansProDir}
/*       /* Skall flyttning av filer ske */                                                  */
/*       ASSIGN cTekst = "".                                                                 */
/*       {syspara.i 200 2 102 cTekst}                                                        */
/*       IF CAN-DO("1,yes,true,ja",cTekst) THEN DO:                                          */
/*           /* Hämta kommando */                                                            */
/*           {syspar2.i 200 2 102 FI-FlyttKommando}                                          */
/*           IF TRIM(FI-FlyttKommando) <> "" THEN DO:                                        */
/*               IF SEARCH(FI-FlyttKommando) = ? THEN                                        */
/*                   ASSIGN FI-FlyttKommando = "".                                           */
/*           END.                                                                            */
/*           ASSIGN FI-FlyttKommando:SCREEN-VALUE IN FRAME {&FRAME-NAME} = FI-FlyttKommando. */
/*       END.                                                                                */
  END.
  /* Kontroll av om Oppdater Preem skall köras */
  ASSIGN cTekst = "".
  {syspara.i 200 2 101 cTekst}
  IF CAN-DO("1,yes,true,ja",cTekst) THEN
      lFinansPreem = TRUE.
  ELSE
      lFinansPreem = FALSE.

  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN TG-FinansPro:SENSITIVE   = lFinansPro
             TG-FinansPro:TOOLTIP     = cFinansProDir
             TG-FinansPreem:SENSITIVE = lFinansPreem.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaTT_Butiker C-Win 
PROCEDURE SkapaTT_Butiker :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FOR EACH Butiker NO-LOCK.
      CREATE TT_Butiker.
      BUFFER-COPY Butiker USING Butik Butnamn TO TT_Butiker.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TilFra C-Win 
PROCEDURE TilFra :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER wType       AS CHAR NO-UNDO.
    DEFINE VAR             wAntSel     AS INTE NO-UNDO.
    DEFINE VAR             wOpen-Query AS LOGI NO-UNDO.    
    DEFINE VAR             wNum-Sel    AS INTE NO-UNDO.
    DEFINE VAR             wLoop       as INTE NO-UNDO.
    DEFINE VARIABLE        rRowId      AS ROWID      NO-UNDO.
    CASE wType:
        WHEN "Til" THEN DO:
            DO wAntSel = 1 TO BROWSE BR-TTButiker:NUM-SELECTED-ROWS:
                BROWSE BR-TTButiker:FETCH-SELECTED-ROW (wAntsel).
                   CREATE TT_ValgteButiker.
                   BUFFER-COPY TT_Butiker USING Butik Butnamn TO TT_ValgteButiker.
                   ASSIGN rRowId = ROWID(TT_ValgteButiker).
                   DELETE TT_Butiker.
            END.
            BROWSE BR-TTButiker:DELETE-SELECTED-ROWS().
            {&OPEN-QUERY-BR-TTValgteButiker}
            REPOSITION BR-TTValgteButiker TO ROWID rRowId NO-ERROR.
        END.
        WHEN "Fra" THEN DO:
            IF BROWSE BR-TTValgteButiker:NUM-SELECTED-ROWS > 1 THEN DO:
                SLETT:
                DO wAntSel = 1 TO BROWSE BR-TTValgteButiker:NUM-SELECTED-ROWS:
                    BROWSE BR-TTValgteButiker:FETCH-SELECTED-ROW (wAntSel).
                    CREATE TT_Butiker.
                    BUFFER-COPY TT_ValgteButiker USING Butik Butnamn TO TT_Butiker.
                    DELETE TT_ValgteButiker.
                    ASSIGN rRowId = ROWID(TT_Butiker).
                END. /* SLETT */
                BROWSE BR-TTValgteButiker:DELETE-SELECTED-ROWS().
            END.
            ELSE DO:
                IF BROWSE BR-TTValgteButiker:NUM-SELECTED-ROWS = 1 THEN DO:
                     CREATE TT_Butiker.
                     BUFFER-COPY TT_ValgteButiker USING Butik Butnamn TO TT_Butiker.
                     DELETE TT_ValgteButiker.
                     BROWSE BR-TTValgteButiker:DELETE-SELECTED-ROWS().
                     ASSIGN rRowId = ROWID(TT_Butiker).
                END.
                wOpen-Query = YES.
            END.
        END.
        WHEN "AlleFra" THEN DO:
            FOR EACH TT_ValgteButiker:
                CREATE TT_Butiker.
                BUFFER-COPY TT_ValgteButiker USING Butik Butnamn TO TT_Butiker.
                DELETE TT_ValgteButiker.
            END.
            {&OPEN-QUERY-BR-TTValgteButiker}
            wOpen-Query = YES.
       END.
        WHEN "AlleTil" THEN DO:
            FOR EACH TT_Butiker:
                CREATE TT_ValgteButiker.
                BUFFER-COPY TT_Butiker USING Butik Butnamn TO TT_ValgteButiker.
                DELETE TT_Butiker.
            END.
            {&OPEN-QUERY-BR-TTValgteButiker}
            wOpen-Query = YES.
       END.
    END CASE.
    IF wOpen-Query THEN DO:
        {&OPEN-QUERY-BR-TTButiker}
        IF rRowId <> ? THEN
            REPOSITION BR-TTButiker TO ROWID rRowId NO-ERROR.
/*         REPOSITION BROWSE-Tag TO ROW 1 NO-ERROR. */
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

