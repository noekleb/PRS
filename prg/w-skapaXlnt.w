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

/* 
DEFINE INPUT  PARAMETER iButik     LIKE Butiker.Butik    NO-UNDO.
DEFINE INPUT  PARAMETER dDato      AS DATE               NO-UNDO.
DEFINE INPUT  PARAMETER cKatalog   AS CHARACTER          NO-UNDO.
DEFINE INPUT  PARAMETER cExtraPara AS CHARACTER  NO-UNDO. 
 
 */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FI-ButikkNr FI-FraDato FI-TilDato FI-Katalog ~
B-Skapa B-Nytt 
&Scoped-Define DISPLAYED-OBJECTS FI-ButikkNr FI-FraDato FI-TilDato ~
FI-Katalog EDITOR-1 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Nytt 
     LABEL "Nytt urval" 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-Skapa 
     LABEL "Skapa" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE EDITOR-1 AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 57 BY 9.76
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-ButikkNr AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "Butik" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-FraDato AS DATE FORMAT "99/99/99":U 
     LABEL "Från/till datum" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Katalog AS CHARACTER FORMAT "X(256)":U INITIAL "~\home~\lindbak~\sendes" 
     LABEL "Katalog" 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE FI-TilDato AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     FI-ButikkNr AT ROW 2.43 COL 19 COLON-ALIGNED HELP
          "Butikknummer."
     FI-FraDato AT ROW 3.52 COL 19 COLON-ALIGNED
     FI-TilDato AT ROW 3.52 COL 34 COLON-ALIGNED NO-LABEL
     FI-Katalog AT ROW 4.67 COL 19 COLON-ALIGNED
     B-Skapa AT ROW 3.05 COL 55
     B-Nytt AT ROW 4.38 COL 55
     EDITOR-1 AT ROW 5.86 COL 20.8 NO-LABEL NO-TAB-STOP 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 81.2 BY 15.43.


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
         TITLE              = "<insert window title>"
         HEIGHT             = 15.43
         WIDTH              = 81.2
         MAX-HEIGHT         = 15.43
         MAX-WIDTH          = 81.2
         VIRTUAL-HEIGHT     = 15.43
         VIRTUAL-WIDTH      = 81.2
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
   FRAME-NAME Custom                                                    */
/* SETTINGS FOR EDITOR EDITOR-1 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
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


&Scoped-define SELF-NAME B-Nytt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Nytt C-Win
ON CHOOSE OF B-Nytt IN FRAME DEFAULT-FRAME /* Nytt urval */
DO:
  RUN EnaDis (TRUE).
  EDITOR-1:SCREEN-VALUE = "".
  APPLY "ENTRY" TO FI-ButikkNr.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Skapa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Skapa C-Win
ON CHOOSE OF B-Skapa IN FRAME DEFAULT-FRAME /* Skapa */
DO:
    DEFINE VARIABLE dDatoLoop AS DATE       NO-UNDO.
    RUN KontrollerInput.
    IF RETURN-VALUE <> "AVBRYT" THEN DO dDatoLoop = FI-FraDato TO FI-TilDato:
        {sww.i}
        RUN EnaDis (FALSE).
        RUN skapaXlntFil.p (FI-ButikkNr,dDatoLoop,FI-Katalog,"").
        EDITOR-1:INSERT-STRING(RETURN-VALUE + CHR(10)).
        {swn.i}
    END.
    RETURN NO-APPLY.
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
    RUN disable_UI.
    {swn.i}
END.

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
  DISPLAY FI-ButikkNr FI-FraDato FI-TilDato FI-Katalog EDITOR-1 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE FI-ButikkNr FI-FraDato FI-TilDato FI-Katalog B-Skapa B-Nytt 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EnaDis C-Win 
PROCEDURE EnaDis :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER lEnable AS LOGICAL    NO-UNDO.
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN FI-ButikkNr:SENSITIVE = lEnable
               FI-FraDato:SENSITIVE  = lEnable
               FI-Katalog:SENSITIVE  = lEnable
               FI-TilDato:SENSITIVE  = lEnable
               B-Skapa:SENSITIVE = lEnable.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KontrollerInput C-Win 
PROCEDURE KontrollerInput :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cAvbryt   AS CHARACTER  NO-UNDO.
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN INPUT FI-ButikkNr
               INPUT FI-FraDato 
               INPUT FI-TilDato
               INPUT FI-Katalog.
        IF NOT CAN-FIND(Butiker WHERE Butiker.Butik = FI-ButikkNr) THEN DO:
            MESSAGE "Finner inte butik"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            APPLY "ENTRY" TO FI-ButikkNr.
            ASSIGN cAvbryt = "AVBRYT".
        END.
        ELSE IF FI-FraDato = ? OR FI-FraDato > TODAY OR FI-FraDato > FI-TilDato THEN DO:
            MESSAGE "Fel från datum"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            APPLY "ENTRY" TO FI-FraDato.
            ASSIGN cAvbryt = "AVBRYT".
        END.
        ELSE IF FI-TilDato = ? OR FI-TilDato > TODAY THEN DO:
            MESSAGE "Fel till datum"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            APPLY "ENTRY" TO FI-TilDato.
            ASSIGN cAvbryt = "AVBRYT".
        END.
        RETURN cAvbryt.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

