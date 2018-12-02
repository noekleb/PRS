&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win

/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE TT_Butiker NO-UNDO LIKE Butiker
       FIELD Opendag   AS DATE
       FIELD HarSchema AS LOGICAL
       FIELD StengtDato AS DATE
       FIELD Oppdatert  AS LOGICAL.


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

DEFINE VARIABLE cHKinst      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cKontrolltabell AS CHARACTER  NO-UNDO. /* Kontroll av vilken data vi skall testa mot */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-TT_Butiker

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES TT_Butiker

/* Definitions for BROWSE BROWSE-TT_Butiker                             */
&Scoped-define FIELDS-IN-QUERY-BROWSE-TT_Butiker TT_Butiker.Butik ~
TT_Butiker.ButNamn Opendag HarSchema Oppdatert StengtDato 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-TT_Butiker 
&Scoped-define QUERY-STRING-BROWSE-TT_Butiker FOR EACH TT_Butiker NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-TT_Butiker OPEN QUERY BROWSE-TT_Butiker FOR EACH TT_Butiker NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-TT_Butiker TT_Butiker
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-TT_Butiker TT_Butiker


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BROWSE-TT_Butiker}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS CB-Aar RS-AntDar B-SlettSkjema FI-OpenDate ~
BUTTON-5 BROWSE-TT_Butiker B-Start B-Kontrollera 
&Scoped-Define DISPLAYED-OBJECTS CB-Aar RS-AntDar FI-OpenDate 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Kontrollera 
     LABEL "Kontrollera" 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-SlettSkjema 
     LABEL "Ta bort alla skjema" 
     SIZE 21 BY 1.14.

DEFINE BUTTON B-Start 
     LABEL "Starta" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-5 
     LABEL "Fixa öppningsdag" 
     SIZE 19 BY 1.14.

DEFINE VARIABLE CB-Aar AS INTEGER FORMAT ">>>9-":U INITIAL 0 
     LABEL "År" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FI-OpenDate AS DATE FORMAT "99/99/99":U 
     LABEL "Öppningsdato" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE RS-AntDar AS INTEGER INITIAL 7 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "5 dagar", 5,
"6 dagar", 6,
"7 dagar", 7
     SIZE 38 BY .95 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-TT_Butiker FOR 
      TT_Butiker SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-TT_Butiker
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-TT_Butiker C-Win _STRUCTURED
  QUERY BROWSE-TT_Butiker NO-LOCK DISPLAY
      TT_Butiker.Butik FORMAT ">>>>>9":U
      TT_Butiker.ButNamn FORMAT "x(20)":U WIDTH 29.4
      Opendag COLUMN-LABEL "Öppningsdag" FORMAT "99/99/99":U
      HarSchema COLUMN-LABEL "Har schema" FORMAT "Ja/":U WIDTH 12.8
      Oppdatert COLUMN-LABEL "Oppdatert" FORMAT "Ja/":U
      StengtDato WIDTH .2
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 77 BY 16.91 ROW-HEIGHT-CHARS .62 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     CB-Aar AT ROW 1.24 COL 5.2 COLON-ALIGNED
     RS-AntDar AT ROW 1.29 COL 25 NO-LABEL
     B-SlettSkjema AT ROW 2.67 COL 7
     FI-OpenDate AT ROW 4.1 COL 17 COLON-ALIGNED
     BUTTON-5 AT ROW 4.1 COL 35
     BROWSE-TT_Butiker AT ROW 5.76 COL 3
     B-Start AT ROW 5.76 COL 82
     B-Kontrollera AT ROW 7.19 COL 82
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 97.2 BY 22.14.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: TT_Butiker T "?" NO-UNDO skotex Butiker
      ADDITIONAL-FIELDS:
          FIELD Opendag   AS DATE
          FIELD HarSchema AS LOGICAL
          FIELD StengtDato AS DATE
          FIELD Oppdatert  AS LOGICAL
      END-FIELDS.
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Fixa öppetschema"
         HEIGHT             = 22.14
         WIDTH              = 97.2
         MAX-HEIGHT         = 22.14
         MAX-WIDTH          = 97.2
         VIRTUAL-HEIGHT     = 22.14
         VIRTUAL-WIDTH      = 97.2
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
/* BROWSE-TAB BROWSE-TT_Butiker BUTTON-5 DEFAULT-FRAME */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-TT_Butiker
/* Query rebuild information for BROWSE BROWSE-TT_Butiker
     _TblList          = "Temp-Tables.TT_Butiker"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   = Temp-Tables.TT_Butiker.Butik
     _FldNameList[2]   > Temp-Tables.TT_Butiker.ButNamn
"TT_Butiker.ButNamn" ? ? "character" ? ? ? ? ? ? no ? no no "29.4" yes no no "U" "" ""
     _FldNameList[3]   > "_<CALC>"
"Opendag" "Öppningsdag" "99/99/99" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > "_<CALC>"
"HarSchema" "Har schema" "Ja/" ? ? ? ? ? ? ? no ? no no "12.8" yes no no "U" "" ""
     _FldNameList[5]   > "_<CALC>"
"Oppdatert" "Oppdatert" "Ja/" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > "_<CALC>"
"StengtDato" ? ? ? ? ? ? ? ? ? no ? no no ".2" yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BROWSE-TT_Butiker */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Fixa öppetschema */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Fixa öppetschema */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Kontrollera
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Kontrollera C-Win
ON CHOOSE OF B-Kontrollera IN FRAME DEFAULT-FRAME /* Kontrollera */
DO:
    ASSIGN INPUT CB-Aar.
    FOR EACH TT_Butiker WHERE TT_Butiker.HarSchema = TRUE.
        FIND ApnSkjema WHERE ApnSkjema.ButikkNr = TT_Butiker.Butik AND 
             ApnSkjema.Ar = CB-Aar NO-LOCK NO-ERROR.
        IF NOT AVAIL ApnSkjema THEN DO:
            MESSAGE "Finner ikke skjema"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            NEXT.
        END.
        RUN KontrollerData (ROWID(ApnSkjema)).
    END.
   {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SlettSkjema
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SlettSkjema C-Win
ON CHOOSE OF B-SlettSkjema IN FRAME DEFAULT-FRAME /* Ta bort alla skjema */
DO:
    MESSAGE "Slett alle skjema/alle butikker. ?" 
        VIEW-AS ALERT-BOX BUTTONS YES-NO UPDATE lBekreft AS LOGICAL FORMAT "J/N".
    IF NOT lBekreft THEN
        RETURN NO-APPLY.
    PROCESS EVENTS.
    FOR EACH ApnSkjema:
        DELETE ApnSkjema.
    END.
    RUN SkapaTT_Butiker.
    {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Start
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Start C-Win
ON CHOOSE OF B-Start IN FRAME DEFAULT-FRAME /* Starta */
DO:
  MESSAGE "Skjema genereres for alle butikker uten registrer skjema " CB-Aar " ?." SKIP
          "(Skjema tidligere enn åpningsdato for butikk ->" SKIP
          "åpningsdato endres til 1/1 " CB-Aar
      VIEW-AS ALERT-BOX BUTTONS YES-NO UPDATE lBekreft AS LOGICAL FORMAT "J/N".
  IF NOT lBekreft THEN
      RETURN NO-APPLY.
  PROCESS EVENTS.
  FOR EACH TT_Butiker WHERE TT_Butiker.HarSchema = FALSE AND Opendag <> ? AND YEAR(Opendag) <= INT(CB-Aar:SCREEN-VALUE):
      IF TT_Butiker.StengtDato <> ? AND YEAR(TT_Butiker.StengtDato) < CB-Aar THEN
          NEXT.
      REPOSITION BROWSE-TT_Butiker TO ROWID ROWID(TT_Butiker) NO-ERROR.
      BROWSE BROWSE-TT_Butiker:REFRESH().
      RUN FixaSchema (TT_Butiker.Butik).
      BROWSE BROWSE-TT_Butiker:REFRESH().
  END.
  RUN SkapaTT_Butiker.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-5 C-Win
ON CHOOSE OF BUTTON-5 IN FRAME DEFAULT-FRAME /* Fixa öppningsdag */
DO:
  IF INPUT FI-OpenDate = ? THEN DO:
      MESSAGE "Angi dato!"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.
  MESSAGE "Endring av åpningsdato for alla butikker. " INPUT FI-OpenDate " ?" 
      VIEW-AS ALERT-BOX BUTTONS YES-NO UPDATE lBekreft AS LOGICAL FORMAT "J/N".
  IF NOT lBekreft THEN
      RETURN.
  PROCESS EVENTS.
  FOR EACH Butiker:
      ASSIGN Butiker.ApningsDato = INPUT FI-OpenDate.
  END.
  RUN SkapaTT_Butiker.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Aar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Aar C-Win
ON VALUE-CHANGED OF CB-Aar IN FRAME DEFAULT-FRAME /* År */
DO:
    ASSIGN CB-Aar.
    RUN SkapaTT_Butiker.
    {&OPEN-QUERY-BROWSE-TT_Butiker}
    {&OPEN-QUERY-BROWSE-ApnSkjema}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RS-AntDar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RS-AntDar C-Win
ON VALUE-CHANGED OF RS-AntDar IN FRAME DEFAULT-FRAME
DO:
  ASSIGN INPUT RS-AntDar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-TT_Butiker
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
{syspara.i 1 1 18 cHKinst}
IF CAN-DO("1,J,yes",cHKinst) THEN DO:
    {syspara.i 1 1 25 cKontrolltabell}
    IF NOT CAN-DO("1,2",cKontrolltabell) THEN
        ASSIGN cKontrolltabell = "1".
END.
ELSE
    ASSIGN cKontrolltabell = "1".

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN InitCB.
  RUN SkapaTT_Butiker.
  RUN enable_UI.
  BROWSE BROWSE-TT_Butiker:SET-REPOSITIONED-ROW(BROWSE BROWSE-TT_Butiker:DOWN,"ALWAYS").
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
  DISPLAY CB-Aar RS-AntDar FI-OpenDate 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE CB-Aar RS-AntDar B-SlettSkjema FI-OpenDate BUTTON-5 BROWSE-TT_Butiker 
         B-Start B-Kontrollera 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FixaSchema C-Win 
PROCEDURE FixaSchema :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE INPUT  PARAMETER ipButik     LIKE Butiker.Butik NO-UNDO.
 DEFINE        VARIABLE  rNyRowId    AS ROWID      NO-UNDO.
 DEFINE        VARIABLE  d31DecFgAr  AS DATE       NO-UNDO.
 DEFINE        VARIABLE  iCount      AS INTEGER    NO-UNDO.
 DEFINE        VARIABLE  cOpenClosed AS CHARACTER  NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
      IF CB-Aar < YEAR(TT_Butiker.Opendag) THEN DO:
          FIND Butiker WHERE Butiker.Butik = ipButik.
          ASSIGN Butiker.ApningsDato = DATE(1,1,CB-Aar).
          RELEASE Butiker.
      END.
      RUN w-ButOppetMal.w (INPUT-OUTPUT rNyRowId,ipButik,CB-Aar,RS-AntDar,"Autogen").
/*       IF rNyRowId <> ? THEN DO:                                                                                                  */
/*           FIND ApnSkjema WHERE ROWID(ApnSkjema) = rNyRowId NO-ERROR.                                                             */
/*           IF AVAIL ApnSkjema THEN DO:                                                                                            */
/*               ASSIGN d31DecFgAr = DATE(12,31,ApnSkjema.Ar - 1)                                                                   */
/*                      cOpenClosed = ApnSkjema.OpenClosed.                                                                         */
/*               DO iCount = 1 TO NUM-ENTRIES(cOpenClosed):                                                                         */
/*                   IF d31DecFgAr + iCount >= TODAY THEN                                                                           */
/*                       LEAVE.                                                                                                     */
/* /*                   IF CAN-FIND(FIRST Datasett WHERE Datasett.ButikkNr = ipButik AND                                         */ */
/* /*                                                     Datasett.Dato  = d31DecFgAr + iCount AND Datasett.SettStatus > 1) THEN */ */
/* /*                       ASSIGN ENTRY(iCount,cOpenClosed) = "4".                                                              */ */
/*                   IF cKontrolltabell = "1" AND                                                                                   */
/*                       CAN-FIND(FIRST Datasett WHERE Datasett.ButikkNr = ipButik AND                                              */
/*                                                   Datasett.Dato  = d31DecFgAr + iCount AND Datasett.SettStatus > 1) THEN         */
/*                           ASSIGN ENTRY(iCount,cOpenClosed) = "4".                                                                */
/*                   ELSE IF cKontrolltabell = "2" AND                                                                              */
/*                       CAN-FIND(FIRST BokforingsBilag WHERE BokforingsBilag.OmsetningsDato  = d31DecFgAr + iCount AND             */
/*                                                            BokforingsBilag.ButikkNr = ipButik) THEN                              */
/*                           ASSIGN ENTRY(iCount,cOpenClosed) = "4".                                                                */
/*               END.                                                                                                               */
/*               ASSIGN ApnSkjema.OpenClosed = cOpenClosed.                                                                         */
/*           END.                                                                                                                   */
/*       END.                                                                                                                       */
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KontrollerData C-Win 
PROCEDURE KontrollerData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER rApnSkjema  AS ROWID      NO-UNDO.
    DEFINE        VARIABLE  d31DecFgAar AS DATE       NO-UNDO.
    DEFINE        VARIABLE  dSisteDato  AS DATE       NO-UNDO.
    DEFINE        VARIABLE  iCount      AS INTEGER    NO-UNDO.
    DEFINE        VARIABLE  cOpenClosed AS CHARACTER  NO-UNDO.

    FIND ApnSkjema WHERE ROWID(ApnSkjema) = rApnSkjema.
    ASSIGN d31DecFgAar = DATE(12,31,ApnSkjema.Ar - 1)
           dSisteDato  = IF ApnSkjema.Ar = YEAR(TODAY) THEN
               TODAY - 1 ELSE DATE(12,31,ApnSkjema.Ar)
           cOpenClosed = ApnSkjema.OpenClosed.

    DO iCount = 1 TO dSisteDato - d31DecFgAar:
/*         IF CAN-DO("1,2",ENTRY(iCount,cOpenClosed)) AND                                                          */
/*             CAN-FIND(FIRST Datasett WHERE Datasett.ButikkNr = ApnSkjema.ButikkNr AND                            */
/*                                         Datasett.Dato  = d31DecFgAar + iCount AND Datasett.SettStatus > 1) THEN */
/*             ASSIGN ENTRY(iCount,cOpenClosed) = "4".                                                             */
/*         IF CAN-DO("1,2",ENTRY(iCount,cOpenClosed)) THEN DO: */
        /* vi gør det alltid */
        DO:
            IF cKontrolltabell = "1" AND
                CAN-FIND(FIRST Datasett WHERE Datasett.ButikkNr = ApnSkjema.ButikkNr AND
                                            Datasett.Dato  = d31DecFgAar + iCount AND Datasett.SettStatus > 1) THEN
                    ASSIGN ENTRY(iCount,cOpenClosed) = "4".
            ELSE IF cKontrolltabell = "2" AND
                CAN-FIND(FIRST BokforingsBilag WHERE BokforingsBilag.OmsetningsDato  = d31DecFgAar + iCount AND
                                                     BokforingsBilag.ButikkNr = ApnSkjema.ButikkNr) THEN 
                    ASSIGN ENTRY(iCount,cOpenClosed) = "4".
        END.
    END.
    IF cOpenClosed <> ApnSkjema.OpenCLosed THEN
        ASSIGN ApnSkjema.OpenClosed = cOpenClosed
               TT_Butiker.Oppdatert = TRUE.
    ELSE
        ASSIGN TT_Butiker.Oppdatert = FALSE.
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
  DO WITH FRAME {&FRAME-NAME}:
      EMPTY TEMP-TABLE TT_Butiker.
    FOR EACH Butiker NO-LOCK:
        BUFFER-COPY Butiker USING butik butnamn TO TT_Butiker.
        ASSIGN TT_Butiker.OpenDag = Butiker.ApningsDato
               TT_Butiker.HarSchema = CAN-FIND(FIRST ApnSkjema WHERE ApnSkjema.ButikkNr = Butiker.Butik AND
                                               ApnSkjema.Ar = CB-Aar)
               TT_Butiker.StengtDato = Butiker.NedlagtDato.
        RELEASE TT_Butiker.
    END.
    {&OPEN-QUERY-{&BROWSE-NAME}}
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

