&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE TT_Butiker NO-UNDO LIKE Butiker.



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

{runlib.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-ApnSkjema

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ApnSkjema TT_Butiker

/* Definitions for BROWSE BROWSE-ApnSkjema                              */
&Scoped-define FIELDS-IN-QUERY-BROWSE-ApnSkjema ApnSkjema.ButikkNr ~
GetButNavn() ApnSkjema.Ukelengde ApnSkjema.Beskrivelse 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-ApnSkjema 
&Scoped-define QUERY-STRING-BROWSE-ApnSkjema FOR EACH ApnSkjema ~
      WHERE ApnSkjema.Ar = CB-Aar NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-ApnSkjema OPEN QUERY BROWSE-ApnSkjema FOR EACH ApnSkjema ~
      WHERE ApnSkjema.Ar = CB-Aar NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-ApnSkjema ApnSkjema
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-ApnSkjema ApnSkjema


/* Definitions for BROWSE BROWSE-TT_Butiker                             */
&Scoped-define FIELDS-IN-QUERY-BROWSE-TT_Butiker TT_Butiker.Butik ~
TT_Butiker.ButNamn TT_Butiker.ApningsDato TT_Butiker.NedlagtDato 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-TT_Butiker 
&Scoped-define QUERY-STRING-BROWSE-TT_Butiker FOR EACH TT_Butiker NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-TT_Butiker OPEN QUERY BROWSE-TT_Butiker FOR EACH TT_Butiker NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-TT_Butiker TT_Butiker
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-TT_Butiker TT_Butiker


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BROWSE-ApnSkjema}~
    ~{&OPEN-QUERY-BROWSE-TT_Butiker}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS B-SlettSkjema RECT-1 RECT-2 RECT-27 RECT-28 ~
CB-Aar BUTTON-2 BUTTON-3 BUTTON-4 FI-Beskrivelse Btn_Help BROWSE-TT_Butiker ~
BUTTON-Ok BROWSE-ApnSkjema B-EndreSkjema FI-ButTxt FI-SkjemaTxt ~
FI-NySkjemaTxt 
&Scoped-Define DISPLAYED-OBJECTS CB-Aar FI-Beskrivelse FI-ButTxt ~
FI-SkjemaTxt FI-NySkjemaTxt 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetButNavn C-Win 
FUNCTION GetButNavn RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-EndreSkjema 
     IMAGE-UP FILE "icon/e-detail.bmp":U NO-FOCUS
     LABEL "" 
     SIZE 4.6 BY 1.05.

DEFINE BUTTON B-SlettSkjema 
     IMAGE-UP FILE "icon/e-del.bmp":U NO-FOCUS
     LABEL "" 
     SIZE 4.6 BY 1.05.

DEFINE BUTTON Btn_Help DEFAULT 
     IMAGE-UP FILE "icon\e-help":U NO-FOCUS FLAT-BUTTON
     LABEL "&Help" 
     SIZE 4.6 BY 1.05
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-2 
     LABEL "5" 
     SIZE 4.6 BY 1.05.

DEFINE BUTTON BUTTON-3 
     LABEL "6" 
     SIZE 4.6 BY 1.05.

DEFINE BUTTON BUTTON-4 
     LABEL "7" 
     SIZE 4.6 BY 1.05.

DEFINE BUTTON BUTTON-Ok 
     IMAGE-UP FILE "icon\e-exit":U NO-FOCUS FLAT-BUTTON
     LABEL "&Ok" 
     SIZE 4.6 BY 1.05 TOOLTIP "Lagre og avslutt".

DEFINE VARIABLE CB-Aar AS INTEGER FORMAT ">>>9-":U INITIAL 0 
     LABEL "År" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Beskrivelse AS CHARACTER FORMAT "X(30)" 
     LABEL "Beskrivelse" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE FI-ButTxt AS CHARACTER FORMAT "X(256)":U INITIAL " Butikker uten skjema" 
      VIEW-AS TEXT 
     SIZE 26 BY .62
     FGCOLOR 9 FONT 6 NO-UNDO.

DEFINE VARIABLE FI-NySkjemaTxt AS CHARACTER FORMAT "X(256)":U INITIAL "Nytt skjema: antall åpne dager, beskrivelse" 
      VIEW-AS TEXT 
     SIZE 52.8 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-SkjemaTxt AS CHARACTER FORMAT "X(256)":U INITIAL " Registrerte skjemaer" 
      VIEW-AS TEXT 
     SIZE 26 BY .62
     FGCOLOR 9 FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88.6 BY 24.1.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 85.6 BY 24.1.

DEFINE RECTANGLE RECT-27
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 174.6 BY .1.

DEFINE RECTANGLE RECT-28
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 174.8 BY .19.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-ApnSkjema FOR 
      ApnSkjema SCROLLING.

DEFINE QUERY BROWSE-TT_Butiker FOR 
      TT_Butiker SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-ApnSkjema
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-ApnSkjema C-Win _STRUCTURED
  QUERY BROWSE-ApnSkjema NO-LOCK DISPLAY
      ApnSkjema.ButikkNr FORMAT ">>>>>9":U
      GetButNavn() COLUMN-LABEL "Butikknavn" FORMAT "x(30)":U WIDTH 30.8
      ApnSkjema.Ukelengde FORMAT "9":U
      ApnSkjema.Beskrivelse FORMAT "X(30)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 81.6 BY 21.19 FIT-LAST-COLUMN.

DEFINE BROWSE BROWSE-TT_Butiker
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-TT_Butiker C-Win _STRUCTURED
  QUERY BROWSE-TT_Butiker NO-LOCK DISPLAY
      TT_Butiker.Butik FORMAT ">>>>>9":U
      TT_Butiker.ButNamn FORMAT "x(20)":U WIDTH 50.8
      TT_Butiker.ApningsDato COLUMN-LABEL "Åpnet" FORMAT "99/99/99":U
            WIDTH 10
      TT_Butiker.NedlagtDato COLUMN-LABEL "Nedlagt" FORMAT "99/99/99":U
            WIDTH 10.8
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 85.6 BY 21.19 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     B-SlettSkjema AT ROW 4.57 COL 98.6
     CB-Aar AT ROW 1.29 COL 6 COLON-ALIGNED
     BUTTON-2 AT ROW 4.57 COL 4.8
     BUTTON-3 AT ROW 4.57 COL 10.2
     BUTTON-4 AT ROW 4.57 COL 15.6
     FI-Beskrivelse AT ROW 4.57 COL 32.2 COLON-ALIGNED HELP
          "Kort beskrivelse av åpningsskjemaet."
     Btn_Help AT ROW 1.33 COL 166.8 NO-TAB-STOP 
     BROWSE-TT_Butiker AT ROW 5.91 COL 4.4
     BUTTON-Ok AT ROW 1.33 COL 171.8 NO-TAB-STOP 
     BROWSE-ApnSkjema AT ROW 5.91 COL 93
     B-EndreSkjema AT ROW 4.57 COL 93.6
     FI-ButTxt AT ROW 2.95 COL 19.4 COLON-ALIGNED NO-LABEL
     FI-SkjemaTxt AT ROW 3 COL 116 COLON-ALIGNED NO-LABEL
     FI-NySkjemaTxt AT ROW 3.71 COL 3.6 COLON-ALIGNED NO-LABEL
     RECT-1 AT ROW 3.29 COL 2.4
     RECT-2 AT ROW 3.29 COL 91
     RECT-27 AT ROW 1.1 COL 1.4
     RECT-28 AT ROW 2.33 COL 1.2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: TT_Butiker T "?" NO-UNDO skotex Butiker
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Skjemaoversikt"
         HEIGHT             = 26.52
         WIDTH              = 175.8
         MAX-HEIGHT         = 26.52
         MAX-WIDTH          = 184.4
         VIRTUAL-HEIGHT     = 26.52
         VIRTUAL-WIDTH      = 184.4
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
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
   FRAME-NAME Size-to-Fit                                               */
/* BROWSE-TAB BROWSE-TT_Butiker Btn_Help DEFAULT-FRAME */
/* BROWSE-TAB BROWSE-ApnSkjema BUTTON-Ok DEFAULT-FRAME */
ASSIGN 
       FRAME DEFAULT-FRAME:SCROLLABLE       = FALSE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-ApnSkjema
/* Query rebuild information for BROWSE BROWSE-ApnSkjema
     _TblList          = "skotex.ApnSkjema"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "skotex.ApnSkjema.Ar = CB-Aar"
     _FldNameList[1]   = skotex.ApnSkjema.ButikkNr
     _FldNameList[2]   > "_<CALC>"
"GetButNavn()" "Butikknavn" "x(30)" ? ? ? ? ? ? ? no ? no no "30.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   = skotex.ApnSkjema.Ukelengde
     _FldNameList[4]   = skotex.ApnSkjema.Beskrivelse
     _Query            is OPENED
*/  /* BROWSE BROWSE-ApnSkjema */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-TT_Butiker
/* Query rebuild information for BROWSE BROWSE-TT_Butiker
     _TblList          = "Temp-Tables.TT_Butiker"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   = Temp-Tables.TT_Butiker.Butik
     _FldNameList[2]   > Temp-Tables.TT_Butiker.ButNamn
"ButNamn" ? ? "character" ? ? ? ? ? ? no ? no no "50.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.TT_Butiker.ApningsDato
"ApningsDato" "Åpnet" ? "date" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.TT_Butiker.NedlagtDato
"NedlagtDato" "Nedlagt" ? "date" ? ? ? ? ? ? no ? no no "10.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BROWSE-TT_Butiker */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Skjemaoversikt */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Skjemaoversikt */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-EndreSkjema
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-EndreSkjema C-Win
ON CHOOSE OF B-EndreSkjema IN FRAME DEFAULT-FRAME
DO:
    DEFINE VARIABLE rApnSkjema AS ROWID      NO-UNDO.
    ASSIGN rApnSkjema = ROWID(ApnSkjema).
    /* ?,?,?,"" är parametrar vid nyreg */
    RUN w-ButOppetMal.w (INPUT-OUTPUT rApnSkjema,?,?,?,"").
    IF RETURN-VALUE <> "AVBRYT" THEN DO:
        FIND CURRENT ApnSkjema NO-LOCK.
        BROWSE BROWSE-ApnSkjema:REFRESH().
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SlettSkjema
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SlettSkjema C-Win
ON CHOOSE OF B-SlettSkjema IN FRAME DEFAULT-FRAME
DO:
  DEFINE VARIABLE rRowId AS ROWID      NO-UNDO.
  IF BROWSE BROWSE-ApnSkjema:FOCUSED-ROW = ? THEN
      RETURN NO-APPLY.
  MESSAGE "Slette skjema?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lSlett AS LOGICAL.
  IF NOT lSlett THEN
      RETURN.
  FIND CURRENT ApnSkjema EXCLUSIVE.
  FIND Butiker WHERE Butiker.Butik = ApnSkjema.ButikkNr NO-LOCK NO-ERROR.
  IF AVAIL Butiker THEN DO:
      RELEASE TT_Butiker.
      BUFFER-COPY Butiker TO TT_Butiker.
      ASSIGN rRowId = ROWID(TT_Butiker).
      {&OPEN-QUERY-BROWSE-TT_Butiker}
      REPOSITION BROWSE-TT_Butiker TO ROWID rRowId.
  END.
  DELETE ApnSkjema.
  BROWSE BROWSE-ApnSkjema:DELETE-CURRENT-ROW().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help C-Win
ON CHOOSE OF Btn_Help IN FRAME DEFAULT-FRAME /* Help */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
    {winhlp.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 C-Win
ON CHOOSE OF BUTTON-2 IN FRAME DEFAULT-FRAME /* 5 */
DO:
  RUN SkapaSkjema (5).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-3 C-Win
ON CHOOSE OF BUTTON-3 IN FRAME DEFAULT-FRAME /* 6 */
DO:
    RUN SkapaSkjema (6).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-4 C-Win
ON CHOOSE OF BUTTON-4 IN FRAME DEFAULT-FRAME /* 7 */
DO:
    RUN SkapaSkjema (7).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Ok C-Win
ON CHOOSE OF BUTTON-Ok IN FRAME DEFAULT-FRAME /* Ok */
DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Aar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Aar C-Win
ON VALUE-CHANGED OF CB-Aar IN FRAME DEFAULT-FRAME /* År */
DO:
    ASSIGN CB-Aar.
    RUN InitTT_Butiker.
    {&OPEN-QUERY-BROWSE-TT_Butiker}
    {&OPEN-QUERY-BROWSE-ApnSkjema}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-ApnSkjema
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
  RUN InitCB.
  RUN InitTT_Butiker.
  RUN enable_UI.
  {lng.i}

  BROWSE BROWSE-TT_Butiker:SET-REPOSITIONED-ROW(14,"ALWAYS").
  BROWSE BROWSE-ApnSkjema:SET-REPOSITIONED-ROW(14,"ALWAYS").
  
  DYNAMIC-FUNCTION("InitTranslation",THIS-PROCEDURE:CURRENT-WINDOW).
  
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
  DISPLAY CB-Aar FI-Beskrivelse FI-ButTxt FI-SkjemaTxt FI-NySkjemaTxt 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE B-SlettSkjema RECT-1 RECT-2 RECT-27 RECT-28 CB-Aar BUTTON-2 BUTTON-3 
         BUTTON-4 FI-Beskrivelse Btn_Help BROWSE-TT_Butiker BUTTON-Ok 
         BROWSE-ApnSkjema B-EndreSkjema FI-ButTxt FI-SkjemaTxt FI-NySkjemaTxt 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EndreSkjemaIkkeAktiv C-Win 
PROCEDURE EndreSkjemaIkkeAktiv :
DEFINE VARIABLE cOpenClosed AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cBeskrivelse LIKE ApnSkjema.Beskrivelse NO-UNDO.
    DEFINE VARIABLE cNotat       LIKE ApnSkjema.Notat       NO-UNDO.

    IF BROWSE BROWSE-ApnSkjema:FOCUSED-ROW = ? THEN
        RETURN NO-APPLY.
    ASSIGN cBeskrivelse = ApnSkjema.Beskrivelse
           cNotat       = ApnSkjema.Notat      
           cOpenClosed  = ApnSkjema.OpenClosed.
    
        RUN d-ButOppetMal.w (ApnSkjema.ButikkNr,ApnSkjema.Ar,ApnSkjema.Ukelengde,"ENDRE",INPUT-OUTPUT cBeskrivelse,INPUT-OUTPUT cNotat,INPUT-OUTPUT cOpenClosed).
/*     RUN wButOppetMal.w (ApnSkjema.ButikkNr,ApnSkjema.Ar,ApnSkjema.Ukelengde,"ENDRE",INPUT-OUTPUT cBeskrivelse,INPUT-OUTPUT cNotat,INPUT-OUTPUT cOpenClosed). */
    IF RETURN-VALUE <> "AVBRYT" THEN DO:
        FIND CURRENT ApnSkjema EXCLUSIVE.
        ASSIGN ApnSkjema.Beskrivelse = cBeskrivelse
               ApnSkjema.Notat       = cNotat
               ApnSkjema.OpenClosed  = cOpenClosed.
        FIND CURRENT ApnSkjema NO-LOCK.
        BROWSE BROWSE-ApnSkjema:REFRESH().
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitCB C-Win 
PROCEDURE InitCB :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iCount     AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cListItems AS CHARACTER  NO-UNDO.
    DO iCount = YEAR(TODAY) - 1 TO YEAR(TODAY) + 1:
        ASSIGN cListItems = cListItems + (IF cListItems = "" THEN "" ELSE ",") 
                        + STRING(iCount).
    END.
    ASSIGN CB-Aar:LIST-ITEMS IN FRAME {&FRAME-NAME} = cListItems
           CB-Aar            = YEAR(TODAY).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitTT_Butiker C-Win 
PROCEDURE InitTT_Butiker :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE TT_Butiker.
    FOR EACH Butiker NO-LOCK WHERE NOT 
        CAN-FIND(ApnSkjema WHERE ApnSkjema.ButikkNr = Butiker.Butik AND
                                 ApnSkjema.Ar       = CB-Aar):
        BUFFER-COPY Butiker TO TT_Butiker.
        RELEASE TT_Butiker.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaSkjema C-Win 
PROCEDURE SkapaSkjema :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT  PARAMETER iUkeLengd AS INTEGER    NO-UNDO.
   DEFINE VARIABLE         rNyRowId    AS ROWID      NO-UNDO.
   DEFINE VARIABLE         cOpenClosed  AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE         cDummy1      AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE         cDummy2      AS CHARACTER  NO-UNDO.
   IF AVAIL TT_Butiker THEN DO WITH FRAME {&FRAME-NAME}:
       IF TT_Butiker.ApningsDato = ? THEN DO:
           MESSAGE "Registrer butikkens åpningsdato."
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
           RETURN NO-APPLY.
           RETURN.
       END.
       ELSE IF YEAR(TT_Butiker.ApningsDato) > CB-Aar THEN DO:
           MESSAGE "Butikken ikke åpnet ønsket år."
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
           RETURN.
       END.
       ELSE IF YEAR(TT_Butiker.NedlagtDato) < CB-Aar THEN DO:
           MESSAGE "Butikken stengt ønsket år."
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
           RETURN.
       END.
       ASSIGN FI-Beskrivelse.
       RUN w-ButOppetMal.w (INPUT-OUTPUT rNyRowId,TT_Butiker.Butik,CB-Aar,iUkeLengd,FI-Beskrivelse).
/*        RUN wButOppetMal.w (TT_Butiker.Butik,CB-Aar,iUkeLengd,"NY",INPUT-OUTPUT cDummy1,INPUT-OUTPUT cDummy2,INPUT-OUTPUT cOpenClosed). */
       IF rNyRowId <> ? THEN DO:
         DELETE TT_Butiker.
         BROWSE BROWSE-TT_Butiker:DELETE-CURRENT-ROW().
         {&OPEN-QUERY-BROWSE-ApnSkjema}
         REPOSITION BROWSE-ApnSkjema TO ROWID rNyRowId.
       END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetButNavn C-Win 
FUNCTION GetButNavn RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND Butiker WHERE Butiker.Butik = ApnSkjema.ButikkNr NO-LOCK NO-ERROR.
  RETURN IF AVAIL Butiker THEN skotex.Butiker.ButNamn ELSE "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

