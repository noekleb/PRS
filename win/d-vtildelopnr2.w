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
def input parameter wArtBasRecid as recid no-undo.
DEF OUTPUT PARAMETER iLopNr AS INT NO-UNDO.

/* Local Variable Definitions ---                                       */
def var wStatus as char initial "AVBRYT".
def var wLapTop as log  no-undo.

def buffer bufArtBas for ArtBas.

{runlib.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS B-Neste B-Forste FI-LopNr Btn_OK Btn_Cancel ~
Btn_Help RECT-51 
&Scoped-Define DISPLAYED-OBJECTS FI-LopNr 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Forste 
     LABEL "&Første ledige" 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-Neste 
     LABEL "&Neste ledige" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Avbryt" 
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

DEFINE VARIABLE FI-LopNr AS INTEGER FORMAT "zzz9":U INITIAL 0 
     LABEL "Løpenummer" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-51
     EDGE-PIXELS 3 GRAPHIC-EDGE  NO-FILL   
     SIZE 54.4 BY 4.76.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     B-Neste AT ROW 1.95 COL 39
     B-Forste AT ROW 3.29 COL 39
     FI-LopNr AT ROW 3.38 COL 21 COLON-ALIGNED
     Btn_OK AT ROW 6.24 COL 2
     Btn_Cancel AT ROW 6.24 COL 18
     Btn_Help AT ROW 6.24 COL 41
     RECT-51 AT ROW 1.24 COL 2
     SPACE(0.00) SKIP(1.47)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Tildeling av løpenummer artikkel"
         CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME Custom                                                    */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Tildeling av løpenummer artikkel */
DO:
  APPLY "END-ERROR":U TO SELF.
  return wStatus.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Forste
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Forste Dialog-Frame
ON CHOOSE OF B-Forste IN FRAME Dialog-Frame /* Første ledige */
DO:
  def var wLoop as int no-undo.
      
  FINN-NESTE:  
  repeat wLoop = input FI-LopNr to 9999:
  
    if wLoop = 0 then
      next FINN-NESTE.
      
    if can-find(bufArtBas no-lock where
      bufArtBas.Vg    = ArtBas.Vg and
      bufArtBas.LopNr = wLoop) then
      do:
        display wLoop @ FI-LopNr with frame Dialog-Frame.
        next FINN-NESTE.
      end.
    else
      leave FINN-NESTE.          
  end. /* FINN-NESTE */
  
  if wLoop > 9999 then
    display " " @ FI-LopNr with frame Dialog-Frame.
  else
    display wLoop @ FI-LopNr with frame Dialog-Frame.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Neste
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Neste Dialog-Frame
ON CHOOSE OF B-Neste IN FRAME Dialog-Frame /* Neste ledige */
DO:
  find last bufArtBas no-lock where
    bufArtBas.Vg = ArtBas.Vg and
    bufArtBas.LopNr <> ? no-error.
  if not available bufArtBas then
    display 1 @ FI-LopNr with frame Dialog-Frame.
  else do:
    if bufArtBas.Lopnr + 1 > 9999 then
      do:
        message "Siste løpenummer er i bruk. Prøv Første Ledige.."
          view-as alert-box message title "Melding".
        display " " @ FI-LopNr with frame Dialog-Frame.
        return no-apply.
      end.
    else
      display bufArtBas.LopNr + 1 @ FI-LopNr with frame Dialog-Frame.
  end.
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
  run LagreLopNr.
  if return-value <> "AVBRYT" then
    do:
      wStatus = "OK".
      apply "close":u to this-procedure.
    end.
  else 
    return no-apply wStatus.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-LopNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-LopNr Dialog-Frame
ON TAB OF FI-LopNr IN FRAME Dialog-Frame /* Løpenummer */
or "RETURN":U of FI-LopNr
DO:
  find bufArtBas no-lock where
    bufArtBas.Vg    = ArtBas.Vg and
    bufArtBas.LopNr = input FI-LopNr no-error.
  if available bufArtBas then
    do:
      message "Vg/Løpenummer er allerede i bruk!"
        view-as alert-box.
      return no-apply.
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

find ArtBas no-lock where
  recid(ArtBas) = wArtBasRecid no-error.
if not available ArtBas then
  do:
    message "Ukjent artikkel!" 
      view-as alert-box message title "Melding".
    return no-apply.
  end.
if ArtBas.lopnr = 0 or ArtBas.LopNr = ? then.
else
  do:
    message "Artikkelen har allerede fått tildelt løpenummerl!" 
      view-as alert-box message title "Melding".
    return no-apply.
  end.

/* Flagger at systemet kjøres på en LapTop (Innkjøpssystem) */
if valid-handle(wLibHandle) then
  run SjekkLapTop in wLibHandle (output wLapTop).

/* Denne rutinen kan ikke kjøres på en LapTop */
if wLapTop then
  do:
    message "Løpenummer kan ikke tildeles artikelen i innkjøpssystemet."
            view-as alert-box message title "Melding".
    return.
  end.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  {lng.i} 

  apply "entry":U to FI-LopNr.
  
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

return wStatus.

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
  DISPLAY FI-LopNr 
      WITH FRAME Dialog-Frame.
  ENABLE B-Neste B-Forste FI-LopNr Btn_OK Btn_Cancel Btn_Help RECT-51 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LagreLopNr Dialog-Frame 
PROCEDURE LagreLopNr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
do with frame Dialog-Frame:
  IF INPUT FI-LopNr = 0 THEN
  DO:
      MESSAGE "Løpenummer må angis - løpenummer 0 er ikke tillatt."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY "AVBRYT".
  END.

  find bufArtBas no-lock where
    bufArtBas.Vg    = ArtBas.Vg and
    bufArtBas.LopNr = input FI-LopNr no-error.
  if available bufArtBas then
    do:
      message "Vg/Løpenummer er allerede i bruk!"
        view-as alert-box.
      return no-apply "AVBRYT".
    end.

  do:
    find current ArtBas NO-LOCK.
    IF ArtBas.LopNr <> ? THEN
    DO:
        ASSIGN
            FI-LopNr:SCREEN-VALUE = STRING(ArtBas.LopNr)
            .
        MESSAGE "Vg/Løpenummer er allerede tildelt artikkelen." SKIP
                "Løpenummer " ArtBas.LopNr " er tildelt."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.
    ELSE DO:
        assign
          iLopNr = input FI-LopNr.
    END.
    
    release ArtBas.
  end. /* Transaction */

  return "OK".
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

