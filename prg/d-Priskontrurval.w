&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
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
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
    DEFINE INPUT-OUTPUT PARAMETER lAktuell AS LOGICAL    NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iButFra  AS INTEGER    NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iButTil  AS INTEGER    NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER dFraDato AS DATE       NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER dTilDato AS DATE       NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER cEAN     AS CHARACTER  NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iSortering AS INTEGER    NO-UNDO.

/* Local Variable Definitions ---                                       */
    DEFINE VARIABLE cReturVerdi AS CHARACTER INIT "AVBRYT" NO-UNDO.
    DEFINE VARIABLE iFirstBut   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iLastBut    AS INTEGER    NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RS-Sortering TG-Aktuell FI-ButikkNrFra ~
FI-ButikkNrTil TG-AllaBut FI-DatoFra FI-DatoTil FI-Strekkode ~
TG-AllaStrekKode Btn_OK Btn_Cancel Btn_Help RECT-59 RECT-60 
&Scoped-Define DISPLAYED-OBJECTS RS-Sortering TG-Aktuell FI-ButikkNrFra ~
FI-ButikkNrTil TG-AllaBut FI-DatoFra FI-DatoTil FI-Strekkode ~
TG-AllaStrekKode 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_Help 
     LABEL "&Help" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE FI-ButikkNrFra AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "Butiker från/till" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-ButikkNrTil AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-DatoFra AS DATE FORMAT "99/99/99" 
     LABEL "Dato från till" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-DatoTil AS DATE FORMAT "99/99/99" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Strekkode AS CHARACTER FORMAT "X(20)" 
     LABEL "EAN" 
     VIEW-AS FILL-IN 
     SIZE 28.6 BY 1 NO-UNDO.

DEFINE VARIABLE RS-Sortering AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Artikel", 1,
"Artikel/butik", 2,
"Dag/butik", 3,
"Butik/dag", 4
     SIZE 60.2 BY 1.19 NO-UNDO.

DEFINE RECTANGLE RECT-59
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 63.2 BY 5.71.

DEFINE RECTANGLE RECT-60
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 63.2 BY 1.38.

DEFINE VARIABLE TG-Aktuell AS LOGICAL INITIAL no 
     LABEL "Aktuell lista" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .81 NO-UNDO.

DEFINE VARIABLE TG-AllaBut AS LOGICAL INITIAL yes 
     LABEL "Alla" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .81 NO-UNDO.

DEFINE VARIABLE TG-AllaStrekKode AS LOGICAL INITIAL yes 
     LABEL "Alla" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     RS-Sortering AT ROW 6.95 COL 3.8 NO-LABEL
     TG-Aktuell AT ROW 1.81 COL 3
     FI-ButikkNrFra AT ROW 3.48 COL 15.6 COLON-ALIGNED HELP
          "Butikknummer."
     FI-ButikkNrTil AT ROW 3.48 COL 30.2 COLON-ALIGNED HELP
          "Butikknummer." NO-LABEL
     TG-AllaBut AT ROW 3.52 COL 48.2
     FI-DatoFra AT ROW 4.67 COL 15.6 COLON-ALIGNED HELP
          "Salgsdato"
     FI-DatoTil AT ROW 4.67 COL 30.2 COLON-ALIGNED HELP
          "Salgsdato" NO-LABEL
     FI-Strekkode AT ROW 5.86 COL 15.6 COLON-ALIGNED HELP
          "Strekkode inklusive sjekksiffer."
     TG-AllaStrekKode AT ROW 5.95 COL 48.2
     Btn_OK AT ROW 1.71 COL 66
     Btn_Cancel AT ROW 2.95 COL 66
     Btn_Help AT ROW 4.95 COL 66
     RECT-59 AT ROW 2.91 COL 1.8
     RECT-60 AT ROW 1.52 COL 1.8
     SPACE(17.19) SKIP(5.95)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Rapportutval"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   Custom                                                               */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON GO OF FRAME Dialog-Frame /* Rapportutval */
DO:
    ASSIGN lAktuell = TG-Aktuell:CHECKED
           iButFra  = INPUT FI-ButikkNrFra 
           iButTil  = INPUT FI-ButikkNrTil
           dFraDato = INPUT FI-DatoFra
           dTilDato = INPUT FI-DatoTil
           cEAN     = INPUT FI-Strekkode
           iSortering = INPUT RS-Sortering
           cReturVerdi = "OK".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Rapportutval */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help Dialog-Frame
ON CHOOSE OF Btn_Help IN FRAME Dialog-Frame /* Help */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  MESSAGE "Help for File: {&FILE-NAME}" VIEW-AS ALERT-BOX INFORMATION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  IF NOT TG-Aktuell:CHECKED THEN DO:
      RUN KontrollerInput.
      IF RETURN-VALUE = "AVBRYT" THEN
          RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-Aktuell
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-Aktuell Dialog-Frame
ON VALUE-CHANGED OF TG-Aktuell IN FRAME Dialog-Frame /* Aktuell lista */
DO:
    IF SELF:CHECKED THEN DO:
        ASSIGN FI-ButikkNrFra:SENSITIVE = FALSE
               FI-ButikkNrTil:SENSITIVE = FALSE 
               FI-DatoFra:SENSITIVE = FALSE
               FI-DatoTil:SENSITIVE = FALSE
               FI-Strekkode:SENSITIVE = FALSE
               TG-AllaBut:SENSITIVE = FALSE
               TG-AllaStrekKode:SENSITIVE = FALSE
               RS-Sortering:SENSITIVE = FALSE.
    END.
    ELSE DO:
        ASSIGN TG-AllaBut:SENSITIVE = TRUE
               TG-AllaStrekKode:SENSITIVE = TRUE
               FI-DatoFra:SENSITIVE = TRUE
               FI-DatoTil:SENSITIVE = TRUE
               RS-Sortering:SENSITIVE = TRUE.
        APPLY "VALUE-CHANGED" TO TG-AllaBut.
        APPLY "VALUE-CHANGED" TO TG-AllaStrekKode.
        APPLY "ENTRY" TO FI-DatoFra.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-AllaBut
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-AllaBut Dialog-Frame
ON VALUE-CHANGED OF TG-AllaBut IN FRAME Dialog-Frame /* Alla */
DO:
  ASSIGN FI-ButikkNrFra:SENSITIVE = NOT SELF:CHECKED
         FI-ButikkNrFra:SCREEN-VALUE = IF NOT SELF:CHECKED AND iButFra = 0 THEN "" ELSE 
                                       IF NOT SELF:CHECKED THEN STRING(iButFra) ELSE STRING(iFirstBut)
         FI-ButikkNrTil:SENSITIVE = FI-ButikkNrFra:SENSITIVE
         FI-ButikkNrTil:SCREEN-VALUE = IF NOT SELF:CHECKED AND iButTil = 0 THEN "" ELSE 
                                       IF NOT SELF:CHECKED THEN STRING(iButTil) ELSE STRING(iLastBut).
  IF NOT SELF:CHECKED THEN 
      APPLY "ENTRY" TO FI-ButikkNrFra.                     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-AllaStrekKode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-AllaStrekKode Dialog-Frame
ON VALUE-CHANGED OF TG-AllaStrekKode IN FRAME Dialog-Frame /* Alla */
DO:
  ASSIGN FI-StrekKode:SENSITIVE = NOT SELF:CHECKED
         FI-StrekKode:SCREEN-VALUE = IF NOT SELF:CHECKED AND 
                                FI-StrekKode:SCREEN-VALUE = "*" THEN "" ELSE IF SELF:CHECKED THEN "*" ELSE FI-StrekKode:SCREEN-VALUE.
  IF NOT SELF:CHECKED THEN 
    APPLY "ENTRY" TO FI-Strekkode.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  FIND FIRST Butiker NO-LOCK.
  ASSIGN iFirstBut = Butiker.Butik.
  FIND LAST Butiker NO-LOCK.
  ASSIGN TG-Aktuell     = lAktuell
         iLastBut = Butiker.Butik
         FI-ButikkNrFra = IF iButFra = 0 AND lAktuell = FALSE THEN iFirstBut ELSE iButFra
         FI-ButikkNrTil = IF iButTil = 0 AND lAktuell = FALSE THEN iLastBut ELSE iButTil
         FI-DatoFra     = dFraDato
         FI-DatoTil     = dTilDato
         FI-StrekKode   = IF cEAN = "" THEN "*" ELSE cEAN
         TG-AllaBut     = FI-ButikkNrFra = iFirstBut AND FI-ButikkNrTil = iLastBut
         TG-AllaStrekKode = FI-StrekKode = "*"
         RS-Sortering     = iSortering.
  RUN enable_UI.
  APPLY "VALUE-CHANGED" TO TG-AllaBut.
  APPLY "VALUE-CHANGED" TO TG-AllaStrekKode.
  IF lAktuell = FALSE THEN
      APPLY "ENTRY" TO FI-DatoFra.
  ELSE DO:
      APPLY "VALUE-CHANGED" TO TG-Aktuell.
      APPLY "ENTRY" TO TG-Aktuell.
  END.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.
RETURN cReturVerdi.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
  DISPLAY RS-Sortering TG-Aktuell FI-ButikkNrFra FI-ButikkNrTil TG-AllaBut 
          FI-DatoFra FI-DatoTil FI-Strekkode TG-AllaStrekKode 
      WITH FRAME Dialog-Frame.
  ENABLE RS-Sortering TG-Aktuell FI-ButikkNrFra FI-ButikkNrTil TG-AllaBut 
         FI-DatoFra FI-DatoTil FI-Strekkode TG-AllaStrekKode Btn_OK Btn_Cancel 
         Btn_Help RECT-59 RECT-60 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KontrollerInput Dialog-Frame 
PROCEDURE KontrollerInput :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lOK AS LOGICAL NO-UNDO.
  DOBLOCK:
  DO WITH FRAME {&FRAME-NAME}:
      IF NOT TG-AllaBut:CHECKED THEN DO:
          FIND Butiker WHERE Butiker.Butik = INPUT FI-ButikkNrFra NO-LOCK NO-ERROR.
          IF NOT AVAIL Butiker THEN DO:
              MESSAGE "Finner inte butik från"
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
              LEAVE DOBLOCK.
          END.
          FIND Butiker WHERE Butiker.Butik = INPUT FI-ButikkNrTil NO-LOCK NO-ERROR.
          IF NOT AVAIL Butiker THEN DO:
              MESSAGE "Finner inte butik till"
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
              LEAVE DOBLOCK.
          END.
          IF INPUT FI-ButikkNrFra > FI-ButikkNrTil THEN DO:
              MESSAGE "Butik från > till"
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
              LEAVE DOBLOCK.
          END.
      END.
      IF INPUT FI-DatoFra = ? THEN DO:
          APPLY "ENTRY" TO FI-DatoFra.
          MESSAGE "Ange dato från"
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          LEAVE DOBLOCK.
      END.
      IF INPUT FI-DatoTil = ? THEN DO:
          APPLY "ENTRY" TO FI-DatoTil.
          MESSAGE "Ange dato till"
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          LEAVE DOBLOCK.
      END.
      IF INPUT FI-DatoFra > INPUT FI-DatoTil THEN DO:
          APPLY "ENTRY" TO FI-DatoFra.
          MESSAGE "Ange från > till"
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          LEAVE DOBLOCK.
      END.
      IF NOT TG-AllaStrekKode:CHECKED THEN DO:
          IF FI-Strekkode:SCREEN-VALUE = "" THEN DO:
              APPLY "ENTRY" TO FI-Strekkode.
              MESSAGE "Ange EAN"
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
              LEAVE DOBLOCK.
          END.
          IF NOT CAN-FIND(Strekkode WHERE Strekkode.Kode = INPUT FI-Strekkode) THEN DO:
              APPLY "ENTRY" TO FI-Strekkode.
              MESSAGE "Finner inte EAN"
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
              LEAVE DOBLOCK.
          END.
      END.
      ASSIGN lOK = TRUE.
  END.
  IF NOT lOK THEN
      RETURN "AVBRYT".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

