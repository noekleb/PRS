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
DEF INPUT PARAMETER gBestNr LIKE BestHode.BestNr NO-UNDO.

/* Local Variable Definitions ---                                       */

{runlib.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FI-NyttBestNr B-Forste B-Neste Btn_OK ~
Btn_Cancel Btn_Help RECT-53 
&Scoped-Define DISPLAYED-OBJECTS FI-GmlBestNr FI-NyttBestNr 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Forste 
     LABEL "Første ledige" 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-Neste 
     LABEL "Neste ledige" 
     SIZE 15 BY 1.14.

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

DEFINE VARIABLE FI-GmlBestNr AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "Gammelt bestillingsnummer" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE FI-NyttBestNr AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "Nytt bestillingsnummer" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-53
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 63 BY 5.48.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     FI-GmlBestNr AT ROW 1.71 COL 30 COLON-ALIGNED
     FI-NyttBestNr AT ROW 2.91 COL 30 COLON-ALIGNED
     B-Forste AT ROW 4.1 COL 32
     B-Neste AT ROW 5.29 COL 32
     Btn_OK AT ROW 6.95 COL 2
     Btn_Cancel AT ROW 6.95 COL 18
     Btn_Help AT ROW 6.95 COL 50
     RECT-53 AT ROW 1.24 COL 2
     SPACE(0.19) SKIP(1.37)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Endre bestillingsnummer"
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
                                                                        */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FI-GmlBestNr IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Endre bestillingsnummer */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Forste
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Forste Dialog-Frame
ON CHOOSE OF B-Forste IN FRAME Dialog-Frame /* Første ledige */
DO:
  RUN ForsteLedige.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Neste
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Neste Dialog-Frame
ON CHOOSE OF B-Neste IN FRAME Dialog-Frame /* Neste ledige */
DO:
  FIND LAST BestHode NO-LOCK NO-ERROR.
  IF AVAILABLE BestHode THEN
      FI-NyttBestNr = BestHode.BestNr + 1.
  ELSE
      FI-NyttBestNr = 1.
  DISPLAY
      FI-NyttBestNr
  WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help Dialog-Frame
ON CHOOSE OF Btn_Help IN FRAME Dialog-Frame /* Hjelp */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  {diahelp.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  IF INPUT FI-NyttBestNr = 0 THEN
  DO:
      MESSAGE "Nytt bestillingsnummer må angis." 
          VIEW-AS ALERT-BOX MESSAGE TITLE "Melding".
      RETURN.
  END.
  IF can-find(BestHode WHERE BestHode.BestNr = INPUT FI-NyttBestNr) THEN
  DO:
      MESSAGE "Bestillingsnummeret er i bruk." 
          VIEW-AS ALERT-BOX MESSAGE TITLE "Melding".
      RETURN.
  END.

  RUN ByttBestNr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

ASSIGN
    FI-GmlBestNr = gBestNr.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  {lng.i}
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByttBestNr Dialog-Frame 
PROCEDURE ByttBestNr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iBestNr AS INT NO-UNDO.

DO WITH FRAME Dialog-Frame:

  ASSIGN
      iBestNr = INPUT FI-NyttBestNr.
                                    
  FOR EACH BestHode EXCLUSIVE-LOCK WHERE
      BestHode.BestNr = FI-GmlBestNr:

      FOR EACH BestLinje OF BestHode EXCLUSIVE-LOCK:
          BestLinje.BestNr = iBestNr.
      END.
      FOR EACH BestPris OF BestHode EXCLUSIVE-LOCK:
          BestPris.BestNr = iBestNr.
      END.
      FOR EACH BestSort OF BestHode EXCLUSIVE-LOCK:
          BestSort.BestNr = iBestNr.
      END.
      FOR EACH BestStr OF BestHode EXCLUSIVE-LOCK:
          BestStr.BestNr = iBestNr.
      END.
      FOR EACH BestHLev OF BestHode EXCLUSIVE-LOCK:
          BestHLev.BestNr = iBestNr.
      END.
      FOR EACH BestLevert OF BestHode EXCLUSIVE-LOCK:
          BestLevert.BestNr = iBestNr.
      END.
      FOR EACH FriButik OF BestHode EXCLUSIVE-LOCK:
          FriButik.BestNr = iBestNr.
      END.
      FOR EACH BestKasse OF BestHode EXCLUSIVE-LOCK:
          BestKasse.BestNr = iBestNr.
      END.

      ASSIGN
          BestHode.BestNr = iBestNr.
  END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  DISPLAY FI-GmlBestNr FI-NyttBestNr 
      WITH FRAME Dialog-Frame.
  ENABLE FI-NyttBestNr B-Forste B-Neste Btn_OK Btn_Cancel Btn_Help RECT-53 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ForsteLedige Dialog-Frame 
PROCEDURE ForsteLedige :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR iLoop AS INT NO-UNDO.

  DO iLoop = INT(FI-NyttBestNr:SCREEN-VALUE IN FRAME Dialog-Frame) TO 999999:
      IF iLoop = 0 THEN NEXT.
      IF NOT CAN-FIND(BestHode WHERE BestHode.BestNr = iLoop) THEN
      DO:
          FI-NyttBestNr = iLoop.
          DISPLAY FI-NyttBestNr WITH FRAME Dialog-Frame.
          RETURN.
      END.
  END.

  MESSAGE "Ingen ledige bestillingsnummer !!!"
      VIEW-AS ALERT-BOX MESSAGE TITLE "Melding".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

