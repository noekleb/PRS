&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
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
DEF VAR bOk      AS LOG    NO-UNDO.
DEF VAR cTekst   AS CHAR   NO-UNDO.
DEF VAR hParent  AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS B-sokButikk FraDato TilDato ButikkNrLst ~
E-Tekst Generer Avslutt btnCalFraDato btnCalFraDato-2 FI-Tekst RECT-1 
&Scoped-Define DISPLAYED-OBJECTS FraDato TilDato ButikkNrLst E-Tekst ~
FI-Tekst 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Avslutt DEFAULT 
     LABEL "Avslutt" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON B-sokButikk 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnCalFraDato 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON NO-CONVERT-3D-COLORS
     LABEL "..." 
     SIZE 3.8 BY 1.

DEFINE BUTTON btnCalFraDato-2 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON NO-CONVERT-3D-COLORS
     LABEL "..." 
     SIZE 3.8 BY 1.

DEFINE BUTTON Generer DEFAULT 
     LABEL "Start generering" 
     SIZE 28 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE E-Tekst AS CHARACTER 
     VIEW-AS EDITOR
     SIZE 75 BY 8.57 NO-UNDO.

DEFINE VARIABLE ButikkNrLst AS CHARACTER FORMAT "X(256)":U 
     LABEL "Butikker" 
     VIEW-AS FILL-IN 
     SIZE 56 BY 1 TOOLTIP "Fra medlemsnummer" NO-UNDO.

DEFINE VARIABLE FI-Tekst AS CHARACTER FORMAT "X(256)":U INITIAL "Generering av TimeGrip eksport av bonger i angitt periode" 
      VIEW-AS TEXT 
     SIZE 78 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FraDato AS DATE FORMAT "99/99/99":U 
     LABEL "Fra dato" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 TOOLTIP "Fra dato" NO-UNDO.

DEFINE VARIABLE TilDato AS DATE FORMAT "99/99/99":U 
     LABEL "Til dato" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 TOOLTIP "Til dato" NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 80 BY 12.19.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     B-sokButikk AT ROW 4.57 COL 70
     FraDato AT ROW 3.43 COL 12 COLON-ALIGNED
     TilDato AT ROW 3.43 COL 48 COLON-ALIGNED
     ButikkNrLst AT ROW 4.57 COL 12 COLON-ALIGNED
     E-Tekst AT ROW 6.24 COL 3.4 NO-LABEL
     Generer AT ROW 15.24 COL 1.4
     Avslutt AT ROW 15.19 COL 66
     btnCalFraDato AT ROW 3.43 COL 34
     btnCalFraDato-2 AT ROW 3.43 COL 70
     FI-Tekst AT ROW 1.71 COL 2 NO-LABEL
     RECT-1 AT ROW 2.91 COL 1.2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80.4 BY 15.62
         DEFAULT-BUTTON Avslutt.


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
         TITLE              = "Generering av TimeGrip eksport"
         HEIGHT             = 15.62
         WIDTH              = 80.4
         MAX-HEIGHT         = 25.33
         MAX-WIDTH          = 116.4
         VIRTUAL-HEIGHT     = 25.33
         VIRTUAL-WIDTH      = 116.4
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
   FRAME-NAME Custom                                                    */
ASSIGN 
       E-Tekst:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN FI-Tekst IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
ASSIGN 
       FI-Tekst:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Generering av TimeGrip eksport */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Generering av TimeGrip eksport */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Avslutt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Avslutt C-Win
ON CHOOSE OF Avslutt IN FRAME DEFAULT-FRAME /* Avslutt */
DO:
  &IF "{&PROCEDURE-TYPE}" EQ "SmartPanel" &THEN
    &IF "{&ADM-VERSION}" EQ "ADM1.1" &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
    &ELSE
      RUN exitObject.
    &ENDIF
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-sokButikk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-sokButikk C-Win
ON CHOOSE OF B-sokButikk IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF ButikkNrLst
DO:
    /*THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.*/
    cTekst = ButikkNrLst:SCREEN-VALUE.
    /* Invoke the selector: */
    RUN JBoxSelector.w (THIS-PROCEDURE,0,
                        "Butiker;ButNamn;Butik"
                        ,"where true"
                        ,INPUT-OUTPUT cTekst,
                        "Butik",
                        INPUT-OUTPUT cTekst,
                        "","",
                        OUTPUT bOK).

    this-procedure:current-window:sensitive = true.
    ASSIGN
        ButikkNrLst:SCREEN-VALUE = cTekst.
    
    APPLY "ENTRY" TO ButikkNrLst.
    RETURN NO-APPLY.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalFraDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalFraDato C-Win
ON CHOOSE OF btnCalFraDato IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF FraDato
DO:
  RUN Cal.w (FraDato:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalFraDato-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalFraDato-2 C-Win
ON CHOOSE OF btnCalFraDato-2 IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF TilDato
DO:
  RUN Cal.w (TilDato:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Generer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Generer C-Win
ON CHOOSE OF Generer IN FRAME DEFAULT-FRAME /* Start generering */
DO:
    RUN genererTimeGrip.
    IF VALID-HANDLE(hParent) THEN
        RUN refreshListe IN hParent.
    
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
ON CLOSE OF THIS-PROCEDURE DO:
    IF VALID-HANDLE(hParent) THEN
        RUN refreshListe IN hParent.
    RUN disable_UI.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.

  ASSIGN
      FraDato:SCREEN-VALUE IN FRAME Default-Frame = STRING(TODAY) 
      TilDato:SCREEN-VALUE IN FRAME Default-Frame = STRING(TODAY) 
      E-Tekst = 'Leser alle bongdata innenfor gitte kriterier ' +
                'og genererer TimeGrip eksport på grunnlag av dette.' + CHR(10) +  CHR(10) +
                'Eventuelle tidligere genererte dataeksportsett innefor angitt periode, vil IKKE bli'  + CHR(10) +
                'generert pånytt, hvis ikke dette er angitt' 
      .
  DISPLAY 
      E-Tekst 
      WITH FRAME Default-Frame.

  APPLY "ENTRY" TO FRAME Default-Frame.

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
  DISPLAY FraDato TilDato ButikkNrLst E-Tekst FI-Tekst 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE B-sokButikk FraDato TilDato ButikkNrLst E-Tekst Generer Avslutt 
         btnCalFraDato btnCalFraDato-2 FI-Tekst RECT-1 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE genererTimeGrip C-Win 
PROCEDURE genererTimeGrip :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR iAntPostert AS INT NO-UNDO.
  DEF VAR iAntLest    AS INT NO-UNDO.
  DEF VAR cMsgs       AS CHAR NO-UNDO.

  DO WITH FRAME DEFAULT-FRAME:
    IF DATE(FraDato:SCREEN-VALUE) = ? OR 
       DATE(TilDato:SCREEN-VALUE) = ? OR 
       ButikkNrLst:SCREEN-VALUE = ''
         THEN
    DO:
        MESSAGE 'Fra/til dato og butikk(er) må angis.'
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.

    IF INPUT TilDato < INPUT FraDato THEN
    DO:
        MESSAGE 'Feil datoangivelse'
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.

    bOk = FALSE.
    MESSAGE 'Bekreft at generering av eksportdatasett skal starte.'
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE 'Bekreft'
        UPDATE bOk.
    
    IF bOk = FALSE THEN RETURN.
  END.

  SESSION:SET-WAIT-STATE("GENERAL").

  RUN generertimegripeksport.p (DATE(FraDato:SCREEN-VALUE),
                           DATE(TilDato:SCREEN-VALUE),
                           ButikkNrLst:SCREEN-VALUE,
                           INPUT-OUTPUT iAntLest,
                           INPUT-OUTPUT iAntPostert,
                           OUTPUT cMsgs).
  SESSION:SET-WAIT-STATE("").
  STATUS DEFAULT ''.

  IF cMsgS <> '' THEN
  DO:
      MESSAGE 'Generering av eksportdatasett er avbrudt med følgende melding:' SKIP(1)
              cMsgs
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
  END.
  ELSE MESSAGE 'Generering av eksportdatasett til TimeGrip ferdig.' SKIP(1)
               'Antall leste bonger:' iAntLest SKIP
               'Antall posterte bonger:' iAntPostert
      VIEW-AS ALERT-BOX INFO BUTTONS OK.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setParentHandle C-Win 
PROCEDURE setParentHandle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER phParent AS HANDLE NO-UNDO.

  ASSIGN
      hParent = phParent.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

