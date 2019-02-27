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

  DEF VAR cRowIdList AS CHAR NO-UNDO.
  DEF VAR cIdList    AS CHAR NO-UNDO.
  DEF VAR bOk        AS LOG  NO-UNDO.

  DEF OUTPUT PARAMETER cVgResultat  AS CHAR NO-UNDO.
  DEF OUTPUT PARAMETER cLevResultat AS CHAR NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RS-Telling btnUtvalgAvdeling btnUtvalgHuvGr ~
btnUtvalgVarGr btnUtvalgLevBas BtnDone BtnCancel 
&Scoped-Define DISPLAYED-OBJECTS EDITOR-2 EDITOR-1 RS-Telling ~
fi-cListAvdeling fi-cListHuvGr fi-cListVarGr fi-cListLevBas FILL-IN-1 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnCancel AUTO-END-KEY DEFAULT 
     LABEL "Avbryt" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON BtnDone DEFAULT 
     LABEL "&Ok" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON btnUtvalgAvdeling 
     LABEL "Avdeling..." 
     SIZE 25.8 BY 1.14.

DEFINE BUTTON btnUtvalgHuvGr 
     LABEL "Hovedgruppe..." 
     SIZE 25.8 BY 1.14.

DEFINE BUTTON btnUtvalgLevBas 
     LABEL "Leverandør..." 
     SIZE 25.8 BY 1.14.

DEFINE BUTTON btnUtvalgVarGr 
     LABEL "Varegruppe..." 
     SIZE 25.8 BY 1.14.

DEFINE VARIABLE EDITOR-1 AS CHARACTER 
     VIEW-AS EDITOR
     SIZE 128 BY 5.71
     BGCOLOR 15 FGCOLOR 0 FONT 8 NO-UNDO.

DEFINE VARIABLE EDITOR-2 AS CHARACTER 
     VIEW-AS EDITOR
     SIZE 136 BY 7.62
     BGCOLOR 12 FGCOLOR 12 FONT 8 NO-UNDO.

DEFINE VARIABLE fi-cListAvdeling AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE 101.2 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cListHuvGr AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE 101 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cListLevBas AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE 100.8 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cListVarGr AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE 101 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-1 AS CHARACTER FORMAT "X(256)":U INITIAL "Telling skal gjøres for:" 
      VIEW-AS TEXT 
     SIZE 33 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE RS-Telling AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Avdeling", 1,
"Hovedgruppe", 2,
"Varegruppe", 3,
"Leverandør", 4
     SIZE 93 BY 1.67 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     EDITOR-2 AT ROW 1 COL 1 NO-LABEL 
     EDITOR-1 AT ROW 1.91 COL 5 NO-LABEL 
     RS-Telling AT ROW 9.57 COL 38 NO-LABEL 
     btnUtvalgAvdeling AT ROW 11.38 COL 5.2 
     fi-cListAvdeling AT ROW 11.43 COL 29.8 COLON-ALIGNED NO-LABEL 
     btnUtvalgHuvGr AT ROW 12.62 COL 5.2 
     fi-cListHuvGr AT ROW 12.67 COL 30 COLON-ALIGNED NO-LABEL 
     btnUtvalgVarGr AT ROW 13.86 COL 5.2 
     fi-cListVarGr AT ROW 13.91 COL 30 COLON-ALIGNED NO-LABEL 
     btnUtvalgLevBas AT ROW 15.05 COL 5.2 
     fi-cListLevBas AT ROW 15.1 COL 30.2 COLON-ALIGNED NO-LABEL 
     BtnDone AT ROW 21.48 COL 2 
     BtnCancel AT ROW 21.48 COL 121 
     FILL-IN-1 AT ROW 10.05 COL 1 COLON-ALIGNED NO-LABEL 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 136.2 BY 21.81
         DEFAULT-BUTTON BtnDone CANCEL-BUTTON BtnCancel .


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
         TITLE              = "Initiering av rullerende telleliste"
         HEIGHT             = 21.81
         WIDTH              = 136.2
         MAX-HEIGHT         = 21.81
         MAX-WIDTH          = 136.2
         VIRTUAL-HEIGHT     = 21.81
         VIRTUAL-WIDTH      = 136.2
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
/* SETTINGS FOR EDITOR EDITOR-1 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR EDITOR EDITOR-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-cListAvdeling IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       fi-cListAvdeling:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN fi-cListHuvGr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       fi-cListHuvGr:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN fi-cListLevBas IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       fi-cListLevBas:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN fi-cListVarGr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       fi-cListVarGr:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-1 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN-1:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Initiering av rullerende telleliste */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Initiering av rullerende telleliste */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnDone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone C-Win
ON CHOOSE OF BtnDone IN FRAME DEFAULT-FRAME /* Ok */
DO:
  DO WITH FRAME DEFAULT-FRAME:
      ASSIGN
          fi-cListAvdeling
          fi-cListHuvGr
          fi-cListVarGr
          fi-cListLevBas
          bOk = FALSE
          .

      IF TRIM(fi-cListAvdeling + fi-cListHuvGr + fi-cListVarGr + fi-cListLevBas) = '' THEN
      DO:
          MESSAGE 'Det er ikke angitt noen kriterier. Tellelisten vil være tom.' SKIP
                  'Funksjonen "Utvalg" kan senere benyttes for å legge inn linjer i listen.'
              VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE bOk.
          IF bOk = FALSE THEN
              RETURN.

      END.
      ELSE RUN ByggResultat.
  END.


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


&Scoped-define SELF-NAME btnUtvalgAvdeling
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUtvalgAvdeling C-Win
ON CHOOSE OF btnUtvalgAvdeling IN FRAME DEFAULT-FRAME /* Avdeling... */
DO:

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "Avdeling;AvdelingNr;AvdelingNavn",
                      "where true",
                      INPUT-OUTPUT cRowIdList,
                      "AvdelingNr",
                      INPUT-OUTPUT cIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.
  ASSIGN 
        fi-cListAvdeling              = cIdList.
        fi-cListAvdeling:SCREEN-VALUE = cIdList.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUtvalgHuvGr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUtvalgHuvGr C-Win
ON CHOOSE OF btnUtvalgHuvGr IN FRAME DEFAULT-FRAME /* Hovedgruppe... */
DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "HuvGr;Hg;HgBeskr,Avdeling;AvdelingNr;AvdelingNavn",
                      "where true, first Avdeling OF HuvGr NO-LOCK",
                      INPUT-OUTPUT cRowIdList,
                      "Hg",
                      INPUT-OUTPUT cIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.
  ASSIGN
      fi-cListHuvGr              = cIdList
      fi-cListHuvGr:SCREEN-VALUE = cIdList.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUtvalgLevBas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUtvalgLevBas C-Win
ON CHOOSE OF btnUtvalgLevBas IN FRAME DEFAULT-FRAME /* Leverandør... */
DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "LevBas;levnr;levnamn;KjedeAvtale|Kjedeavtale|J/N",
                      "where true",
                      INPUT-OUTPUT cRowIdList,
                      "Levnr",
                      INPUT-OUTPUT cIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  ASSIGN
      fi-cListLevBas              = cIdList
      fi-cListLevBas:SCREEN-VALUE = cIdList.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUtvalgVarGr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUtvalgVarGr C-Win
ON CHOOSE OF btnUtvalgVarGr IN FRAME DEFAULT-FRAME /* Varegruppe... */
DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "VarGr;Vg;VgBeskr,HuvGr;Hg;HgBeskr",
                      "where true, first HuvGr OF VarGr NO-LOCK ",
                      INPUT-OUTPUT cRowIdList,
                      "Vg",
                      INPUT-OUTPUT cIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.
  ASSIGN
      fi-cListVarGr              = cIdList
      fi-cListVarGr:SCREEN-VALUE = cIdList.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RS-Telling
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RS-Telling C-Win
ON VALUE-CHANGED OF RS-Telling IN FRAME DEFAULT-FRAME
DO:
  RUN EableDisable (INPUT INPUT RS-Telling).
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


  ASSIGN 
      Editor-1:SCREEN-VALUE = "Ved rullerende telling, skal tellelisten initieres for deler av varelageret." + 
                          " Initiering gjøres for avdeling, hovedgruppe, varegruppe eller leverandør."
      .

  RUN EableDisable (1).

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggResultat C-Win 
PROCEDURE ByggResultat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR piLoop AS INT NO-UNDO.
  
  ASSIGN
      cVgResultat  = fi-cListVarGr
      cVgResultat  = REPLACE(cVgResultat,'|',',')
      cLevResultat = fi-cListLevBas
      cLevResultat = REPLACE(cLevResultat,'|',',').

  IF fi-cListHuvGr <> '' THEN
  DO:
      DO piLoop = 1 TO NUM-ENTRIES(fi-cListHuvGr,'|'):
          FOR EACH VarGr NO-LOCK WHERE 
              VarGr.Hg = INT(ENTRY(piLoop,fi-cListHuvGr,'|')):

              IF NOT CAN-DO(cVgResultat,STRING(VarGr.Vg)) THEN
                  cVgResultat = cVgResultat + 
                              (IF cVgResultat <> '' THEN ',' ELSE '') + 
                              STRING(VarGr.Vg).

          END.
      END.
  END.
  IF fi-cListAvdeling <> '' THEN
  DO:
      DO piLoop = 1 TO NUM-ENTRIES(fi-cListAvdeling,'|'):
        FOR EACH Huvgr NO-LOCK WHERE
            HuvGr.AvdelingNr = INT(ENTRY(piLoop,fi-cListAvdeling,'|')):
            FOR EACH VarGr NO-LOCK WHERE 
                VarGr.Hg = HuvGr.Hg:

                IF NOT CAN-DO(cVgResultat,STRING(VarGr.Vg)) THEN
                    cVgResultat = cVgResultat + 
                                (IF cVgResultat <> '' THEN ',' ELSE '') + 
                                STRING(VarGr.Vg).

            END.
        END.
      END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EableDisable C-Win 
PROCEDURE EableDisable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER iValg AS INT NO-UNDO.

  DO WITH FRAME DEFAULT-FRAME:
      ASSIGN
          btnUtvalgAvdeling:SENSITIVE = (iValg = 1)
          btnUtvalgHuvGr:SENSITIVE    = (iValg = 2)
          btnUtvalgVarGr:SENSITIVE    = (iValg = 3)
          btnUtvalgLevBas:SENSITIVE   = (iValg = 4)
          .
  END.

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
  DISPLAY EDITOR-2 EDITOR-1 RS-Telling fi-cListAvdeling fi-cListHuvGr 
          fi-cListVarGr fi-cListLevBas FILL-IN-1 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RS-Telling btnUtvalgAvdeling btnUtvalgHuvGr btnUtvalgVarGr 
         btnUtvalgLevBas BtnDone BtnCancel 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

