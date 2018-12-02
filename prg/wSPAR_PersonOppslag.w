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
DEF OUTPUT PARAMETER cMedlemsNr AS CHAR NO-UNDO.

/* Local Variable Definitions ---                                       */

DEF VAR cReturn-Value  AS CHAR INITIAL 'AVBRYT' NO-UNDO.
DEF VAR cConnectString AS CHARACTER NO-UNDO.
DEF VAR hServer        AS HANDLE NO-UNDO. 
DEF VAR lConnected     AS LOGICAL NO-UNDO. 

DEF VAR cMedlemskort AS CHAR FORMAT "x(20)" NO-UNDO.
DEF VAR cSkrivRader  AS CHAR FORMAT "x(20)" NO-UNDO.
DEF VAR cMedlemsNamn AS CHAR FORMAT "x(40)" NO-UNDO.
DEF VAR cStatus      AS CHAR FORMAT "x(20)" NO-UNDO.
DEF VAR lOK          AS LOG                 NO-UNDO.
DEF VAR cMelding     AS CHAR FORMAT "x(40)" NO-UNDO.
DEF VAR iCl          AS INT                 NO-UNDO.

DEFINE VARIABLE cLengdeLst  AS CHARACTER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FI-Personnr BtnOK BtnDone 
&Scoped-Define DISPLAYED-OBJECTS FI-Personnr 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnDone DEFAULT 
     LABEL "&Avbryt" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON BtnOK AUTO-GO DEFAULT 
     LABEL "Oppslag mot SPAR..." 
     SIZE 27 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE FI-Personnr AS CHARACTER FORMAT "X(10)":U 
     LABEL "Personnr" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 TOOLTIP "Legg inn personnr. 10 siffer." NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     FI-Personnr AT ROW 2.19 COL 13 COLON-ALIGNED HELP
          "Legg inn personnr. 10 siffer."
     BtnOK AT ROW 2.19 COL 46 WIDGET-ID 4
     BtnDone AT ROW 3.86 COL 2 WIDGET-ID 8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 83.2 BY 4.1
         DEFAULT-BUTTON BtnOK WIDGET-ID 100.


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
         TITLE              = "Opprett nytt medlem (Via personnr. oppslag)"
         HEIGHT             = 4.1
         WIDTH              = 83.2
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 83.2
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 83.2
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
   FRAME-NAME                                                           */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Opprett nytt medlem (Via personnr. oppslag) */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Opprett nytt medlem (Via personnr. oppslag) */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnDone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone C-Win
ON CHOOSE OF BtnDone IN FRAME DEFAULT-FRAME /* Avbryt */
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


&Scoped-define SELF-NAME BtnOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnOK C-Win
ON CHOOSE OF BtnOK IN FRAME DEFAULT-FRAME /* Oppslag mot SPAR... */
DO:
    FIND Bruker NO-LOCK WHERE 
        Bruker.BrukerId = USERID('SkoTex') NO-ERROR.
    IF NOT AVAILABLE Bruker THEN
    DO:
        MESSAGE 'Ukjent brukerid ' USERID('SkoTex') SKIP
            'Oppslag mot SPAR krever at det er angitt et gyldig brukerid.'
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
    DO WITH FRAME DEFAULT-FRAME:
        IF TRIM(FI-Personnr:SCREEN-VALUE) = '' THEN
        DO:
            MESSAGE 'Personnr. er ikke angitt.'
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN NO-APPLY.
        END.

        /* Sjekker om legnden på medlemsid er en av de tillatte lengder. */
        IF NOT CAN-DO(cLengdeLst,STRING(LENGTH(TRIM(FI-Personnr:SCREEN-VALUE)))) THEN 
        DO:
            MESSAGE '** Ugyldig lengde på medlemsid ' + FI-Personnr:SCREEN-VALUE 
                + '. Tillatte lengder: ' + cLengdeLst + '.'
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN NO-APPLY.
        END.

        /* Svens personnr */
        IF LENGTH(TRIM(FI-Personnr:SCREEN-VALUE)) = 10 THEN
        SPAR: 
        DO:
            /* 7607262388 */
            {sww.i}
            RUN asMedlem.p (Bruker.ButikkNr,
                            FI-Personnr:SCREEN-VALUE,
                            Bruker.BrukerId,
                            OUTPUT cMedlemsNr,
                            OUTPUT cMedlemskort,
                            OUTPUT cSkrivRader,
                            OUTPUT cMedlemsNamn, 
                            OUTPUT cStatus,
                            OUTPUT lOK,
                            OUTPUT cMelding
                           ).
            cReturn-Value = cMedlemsNr.
            {swn.i}
/* MESSAGE 'Etter asMedlem.p' SKIP        */
/*     'cMedlemsNr'   cMedlemsNr  SKIP    */
/*     'cMedlemskort' cMedlemskort SKIP   */
/*     'cSkrivRader'  cSkrivRader  SKIP   */
/*     'cMedlemsNamn' cMedlemsNamn  SKIP  */
/*     'cStatus'      cStatus      SKIP   */
/*     'lOK'          lOK         SKIP    */
/*     'cMelding'      cMelding           */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK. */
            IF cStatus = '0' THEN
                MESSAGE 'Feil ved oppkobling mot SPAR. Kunne ikke opprette medlem.' SKIP(1)
                    'cMedlemsNr' cMedlemsNr SKIP
                    cMelding
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
            ELSE IF cStatus = '1' THEN
                MESSAGE 'Nytt medlem opprettet.' SKIP
                       'cMedlemsNr' cMedlemsNr SKIP
                        cMedlemsNamn
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
            ELSE IF cStatus = '2' THEN
                MESSAGE 'Medlemmet finnes fra før.' SKIP
                       'cMedlemsNr' cMedlemsNr SKIP
                        cMedlemsNamn
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
            
            CASE cStatus:
                WHEN '0' THEN RETURN NO-APPLY.
                WHEN '1' THEN RETURN cMedlemsNr.
                WHEN '2' THEN RETURN cMedlemsNr.
            END CASE.
        END. /* SPAR */
        /* Oppretter lokale medlemsid. */
        ELSE IF LENGTH(TRIM(FI-Personnr:SCREEN-VALUE)) < 10 OR
                LENGTH(TRIM(FI-Personnr:SCREEN-VALUE)) = 11 THEN
        DO:
            IF AVAILABLE Medlem THEN RELEASE Medlem.
            IF AVAILABLE Medlemskort THEN RELEASE Medlemskort.
            FIND Medlem NO-LOCK WHERE 
                Medlem.MedlemsNr = DEC(TRIM(FI-Personnr:SCREEN-VALUE)) NO-ERROR.
            IF NOT AVAILABLE Medlem THEN
            DO:
                FIND FIRST MedlemsKort NO-LOCK WHERE 
                    MedlemsKort.KortNr = TRIM(FI-Personnr:SCREEN-VALUE) NO-ERROR.
                IF AVAILABLE Medlemskort THEN
                    FIND Medlem OF Medlemskort NO-LOCK NO-ERROR.
            END.
            IF AVAILABLE Medlem THEN
            DO:
                ASSIGN
                    cStatus      = '2'
                    cMedlemsNr = STRING(Medlem.MedlemsNr) /*TRIM(FI-Personnr:SCREEN-VALUE)*/
                    .
                APPLY 'CHOOSE' TO BtnDone.
            END.
            ELSE DO TRANSACTION:
                lOk = FALSE.
                MESSAGE 'Nytt medlem opprettes med medlemsnr: ' FI-Personnr:SCREEN-VALUE '.' SKIP
                    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lOk.
                IF lOk <> TRUE THEN
                    RETURN NO-APPLY.
                CREATE Medlem.
                ASSIGN
                    Medlem.MedlemsNr = DEC(TRIM(FI-Personnr:SCREEN-VALUE))
                    Medlem.MedGruppe = 1
                    Medlem.Medtype   = 1
                    Medlem.ButikkNr  = iCL.
                CREATE MedlemsKort.
                ASSIGN
                    MedlemsKort.MedlemsNr = Medlem.MedlemsNr
                    MedlemsKort.KortNr    = TRIM(FI-Personnr:SCREEN-VALUE)
                    MedlemsKort.Innehaver = 'Medlem ' + TRIM(FI-Personnr:SCREEN-VALUE).
                ASSIGN
                    cStatus    = '1'
                    cMedlemsNr = STRING(Medlem.MedlemsNr)
                    .
                RELEASE Medlem.
                RELEASE Medlemskort.
                APPLY 'CHOOSE' TO BtnDone.
            END. /* TRANSACTION */
        END.
        ELSE DO:
            MESSAGE '** Ugyldig medlemsid ' + FI-Personnr:SCREEN-VALUE + '.'
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
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

{syspara.i 5 1 1 iCL INT}

{syspara.i 14 1 25 cLengdeLst}
IF cLengdeLst = '' THEN 
    cLengdeLst = '10'.

{syspara.i 1 1 31 cConnectString}.
IF cConnectString = '' THEN
    cConnectString = '-H localhost -AppService asbroker1'.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

RETURN cReturn-Value.

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
  DISPLAY FI-Personnr 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE FI-Personnr BtnOK BtnDone 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

