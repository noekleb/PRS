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
DEFINE OUTPUT PARAMETER iBildNr AS INTEGER    NO-UNDO.
/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-63 FI-Vg FI-LopNr FI-ArtikkelNr ~
FI-Strekkode 
&Scoped-Define DISPLAYED-OBJECTS FI-Vg FI-LopNr FI-ArtikkelNr FI-Strekkode 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE VARIABLE FI-ArtikkelNr AS DECIMAL FORMAT "zzzzzzzzzzzz9":U INITIAL 0 
     LABEL "ArtikkelNr" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE FI-LopNr AS INTEGER FORMAT "zzz9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Strekkode AS DECIMAL FORMAT "zzzzzzzzzzzz9":U INITIAL 0 
     LABEL "Strekkode" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Vg AS INTEGER FORMAT "zzzzz9":U INITIAL 0 
     LABEL "Vg/LøpeNr" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-63
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 38 BY 4.52.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     FI-Vg AT ROW 1.71 COL 14 COLON-ALIGNED
     FI-LopNr AT ROW 1.71 COL 24 COLON-ALIGNED NO-LABEL
     FI-ArtikkelNr AT ROW 2.71 COL 14 COLON-ALIGNED
     FI-Strekkode AT ROW 3.71 COL 14 COLON-ALIGNED
     RECT-63 AT ROW 1.24 COL 2
     SPACE(1.99) SKIP(0.42)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Hent bilde".


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

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Hent bilde */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-ArtikkelNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-ArtikkelNr Dialog-Frame
ON TAB OF FI-ArtikkelNr IN FRAME Dialog-Frame /* ArtikkelNr */
or "return" of FI-ArtikkelNr
DO:
  if input FI-ArtikkelNr <> 0 THEN do:
      find ArtBas no-lock WHERE ArtBas.ArtikkelNr = INPUT FI-ArtikkelNr no-error.
      if not available ArtBas THEN DO:
              message "Ukjent artikkelnummer!" view-as alert-box message title "Melding".
              APPLY "ENTRY" TO SELF.
              return no-apply.
      END.
      else do:
            assign iBildNr = Artbas.BildNr.
            apply "GO" TO FRAME {&FRAME-NAME}.
    end.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-LopNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-LopNr Dialog-Frame
ON TAB OF FI-LopNr IN FRAME Dialog-Frame
or "return" of FI-LopNr
DO:
  if input FI-LopNr > 0 then
    do:
      find first ArtBas no-lock where
        ArtBas.Vg    = input FI-Vg and
        ArtBAs.LopNr = input FI-LopNr no-error.
      if not available ArtBas then
        do:
          message "Ukjent artikkel!" view-as alert-box message title "Melding".
          APPLY "ENTRY" TO SELF.
          return no-apply.
        end.
      else do:
          iBildNr = ArtBas.BildNr.
          APPLY "GO" TO FRAME {&FRAME-NAME}.
      end.
    end.
      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Strekkode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Strekkode Dialog-Frame
ON TAB OF FI-Strekkode IN FRAME Dialog-Frame /* Strekkode */
or "return" of FI-Strekkode
DO:
    DEFINE VARIABLE cStrekKode AS CHARACTER  NO-UNDO.
  if input FI-Strekkode <> 0 THEN do:
      ASSIGN   
          cStrekKode = string(INPUT FI-Strekkode)
          .
      /* Sjekker rått. */
      find Strekkode no-lock WHERE Strekkode.Kode = cStrekkode no-error.
      IF NOT AVAILABLE Strekkode THEN
      DO:
          ASSIGN
              cStrekKode = FILL("0",13 - LENGTH(cStrekkode)) + cStrekKode.
          /* Sjekksifferkontroll */
          find Strekkode no-lock WHERE Strekkode.Kode = cStrekkode no-error.
      END.
      IF NOT AVAIL Strekkode THEN DO:
          MESSAGE "Ukjent strekkode!" view-as alert-box message title "Melding".
          APPLY "ENTRY" TO SELF.
          RETURN NO-APPLY.
      END.
      ELSE
          FIND ArtBas NO-LOCK WHERE ArtBas.ArtikkelNr = StrekKode.ArtikkelNr NO-ERROR.
      IF AVAIL ArtBas THEN DO:
          iBildNr = ArtBas.BildNr.
          APPLY "GO":U TO FRAME {&FRAME-NAME}.
      END.
      ELSE DO:
          MESSAGE "Ukjent artikkel!" view-as alert-box message title "Melding".
          APPLY "ENTRY" TO SELF.
          RETURN NO-APPLY.
      END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Vg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Vg Dialog-Frame
ON TAB OF FI-Vg IN FRAME Dialog-Frame /* Vg/LøpeNr */
or "return" of FI-Vg
DO:
  if input FI-Vg > 0 then
    do:
      find VarGr no-lock where
        VarGr.Vg = input FI-Vg no-error.
      if not available VarGr then
        do:
          message "Ukjent varegruppe!" view-as alert-box message title "Melding".
          APPLY "ENTRY" TO SELF.
          return no-apply.
        end.
    end.
  
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
  RUN enable_UI.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

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
  DISPLAY FI-Vg FI-LopNr FI-ArtikkelNr FI-Strekkode 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-63 FI-Vg FI-LopNr FI-ArtikkelNr FI-Strekkode 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

