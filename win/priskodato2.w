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
/*          This .W file was created with the Progress AppBulder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEF INPUT-OUTPUT PARAMETER dDato  AS DATE NO-UNDO.
DEF INPUT-OUTPUT PARAMETER dcTid  AS DEC  NO-UNDO.
DEF INPUT-OUTPUT PARAMETER d2Dato AS DATE NO-UNDO.
DEF INPUT-OUTPUT PARAMETER dc2Tid AS DEC  NO-UNDO.
DEF INPUT-OUTPUT PARAMETER dcPris AS DEC  NO-UNDO.

DEF VAR cStatus AS CHAR INITIAL "AVBRYT" NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FI-DATO FI-Tid FI-DATO2 FI-Tid2 FI-Pris ~
Btn_OK Btn_Cancel Btn_Help RECT-55 
&Scoped-Define DISPLAYED-OBJECTS FI-DATO FI-Tid FI-DATO2 FI-Tid2 FI-Pris 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_Help 
     LABEL "&Hjelp" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE FI-DATO AS DATE FORMAT "99/99/99":U 
     LABEL "Aktiveringsdato" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-DATO2 AS DATE FORMAT "99/99/99":U 
     LABEL "Gyldig til dato" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Pris AS DECIMAL FORMAT "->>,>>>,>>9.99":U INITIAL 0 
     LABEL "Pris" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Tid AS DECIMAL FORMAT "99.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Tid2 AS DECIMAL FORMAT "99.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-55
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 55 BY 4.76.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     FI-DATO AT ROW 2.19 COL 22 COLON-ALIGNED
     FI-Tid AT ROW 2.19 COL 36 COLON-ALIGNED NO-LABEL
     FI-DATO2 AT ROW 3.29 COL 22 COLON-ALIGNED
     FI-Tid2 AT ROW 3.29 COL 36 COLON-ALIGNED NO-LABEL
     FI-Pris AT ROW 4.38 COL 22 COLON-ALIGNED
     Btn_OK AT ROW 1.24 COL 58
     Btn_Cancel AT ROW 2.43 COL 58
     Btn_Help AT ROW 4.81 COL 58
     RECT-55 AT ROW 1.24 COL 2
     SPACE(17.19) SKIP(0.04)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Endring av aktiveringsdato"
         CANCEL-BUTTON Btn_Cancel.


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
ON GO OF FRAME Dialog-Frame /* Endring av aktiveringsdato */
DO:
  ASSIGN
      dDato    = INPUT FI-Dato
      dcTid    = INPUT FI-Tid
      d2Dato   = INPUT FI-Dato2
      dc2Tid   = INPUT FI-Tid2
      dcPris   = INPUT FI-Pris
      cStatus = ""
      .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Endring av aktiveringsdato */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel Dialog-Frame
ON CHOOSE OF Btn_Cancel IN FRAME Dialog-Frame /* Avbryt */
DO:
  ASSIGN
      cStatus = "AVBRYT".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help Dialog-Frame
ON CHOOSE OF Btn_Help IN FRAME Dialog-Frame /* Hjelp */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  MESSAGE "Help for File: {&FILE-NAME}" VIEW-AS ALERT-BOX INFORMATION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-DATO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-DATO Dialog-Frame
ON LEAVE OF FI-DATO IN FRAME Dialog-Frame /* Aktiveringsdato */
DO:
  IF INPUT FI-Dato < TODAY THEN
  DO:
      MESSAGE "Ugyldig datoangivelse." SKIP 
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.
  IF (INPUT FI-Dato2 < TODAY) or
     (INPUT FI-Dato2 < INPUT FI-Dato) THEN
  DO:
      MESSAGE "Ugyldig datoangivelse." SKIP 
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.
  IF INPUT FI-Dato = INPUT FI-Dato2 THEN
  DO:
      IF INPUT FI-Tid2 < FI-Tid THEN
      DO:
          MESSAGE "Ugyldig datoangivelse." SKIP 
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN NO-APPLY.
      END.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-DATO2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-DATO2 Dialog-Frame
ON LEAVE OF FI-DATO2 IN FRAME Dialog-Frame /* Gyldig til dato */
DO:
  IF INPUT FI-Dato2 < TODAY THEN
    DO:
        MESSAGE "Ugyldig datoangivelse." SKIP 
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
  IF (INPUT FI-Dato2 < TODAY) or
     (INPUT FI-Dato2 < INPUT FI-Dato) THEN
  DO:
      MESSAGE "Ugyldig datoangivelse." SKIP 
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.
  IF INPUT FI-Dato = INPUT FI-Dato2 THEN
  DO:
      IF INPUT FI-Tid2 < FI-Tid THEN
      DO:
          MESSAGE "Ugyldig datoangivelse." SKIP 
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN NO-APPLY.
      END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Tid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Tid Dialog-Frame
ON LEAVE OF FI-Tid IN FRAME Dialog-Frame
DO:
  IF INT(SUBSTRING(STRING(INPUT FI-Tid,"99.99"),4,2)) > 59 OR 
     INT(SUBSTRING(STRING(INPUT FI-Tid,"99.99"),1,2)) > 23 THEN
  DO:
      MESSAGE "Ugyldig tidsangivelse."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Tid2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Tid2 Dialog-Frame
ON LEAVE OF FI-Tid2 IN FRAME Dialog-Frame
DO:
  IF INT(SUBSTRING(STRING(INPUT FI-Tid2,"99.99"),4,2)) > 59 OR 
     INT(SUBSTRING(STRING(INPUT FI-Tid2,"99.99"),1,2)) > 23 THEN
  DO:
      MESSAGE "Ugyldig tidsangivelse."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.
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

  ASSIGN
      FI-Dato  = dDato
      FI-Tid   = dcTid
      FI-Dato2 = d2Dato
      FI-Tid2  = dc2Tid
      FI-Pris = dcPris
      .
  {lng.i}
  RUN enable_UI.

  APPLY "ENTRY" TO FI-Dato IN FRAME {&FRAME-NAME}.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

RETURN cStatus.

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
  DISPLAY FI-DATO FI-Tid FI-DATO2 FI-Tid2 FI-Pris 
      WITH FRAME Dialog-Frame.
  ENABLE FI-DATO FI-Tid FI-DATO2 FI-Tid2 FI-Pris Btn_OK Btn_Cancel Btn_Help 
         RECT-55 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

