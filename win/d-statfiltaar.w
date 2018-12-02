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
def input-output parameter wKriterier as char no-undo.
DEF INPUT        PARAMETER wStTypeId  AS CHAR NO-UNDO.

/* Local Variable Definitions ---                                       */
def var wRetStatus as char initial "AVBRYT" no-undo.
def var wTittel    as char                  no-undo.
def var wButik     as int                   no-undo.
def var wAlle      as char                  no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS B-AlleBut FI-Aar FI-Aar2 Btn_OK Btn_Cancel ~
B-SokButikk Btn_Help RECT-49 
&Scoped-Define DISPLAYED-OBJECTS FI-Aar FI-Aar2 FI-Butikk FILL-IN-3 ~
FI-FraTxt FI-TilTxt 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-AlleBut  NO-FOCUS
     LABEL "Alle butikker" 
     SIZE 15 BY .95.

DEFINE BUTTON B-SokButikk 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

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

DEFINE VARIABLE FI-Aar AS INTEGER FORMAT "9999" INITIAL 0 
     LABEL "År/mnd" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1.

DEFINE VARIABLE FI-Aar2 AS INTEGER FORMAT "9999" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1.

DEFINE VARIABLE FI-Butikk AS CHARACTER FORMAT "X(256)" 
     LABEL "Butikk" 
     VIEW-AS FILL-IN 
     SIZE 20.2 BY 1.

DEFINE VARIABLE FI-FraTxt AS CHARACTER FORMAT "X(256)":U INITIAL "Fra" 
      VIEW-AS TEXT 
     SIZE 14 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-TilTxt AS CHARACTER FORMAT "X(256)":U INITIAL "Til" 
      VIEW-AS TEXT 
     SIZE 14 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FILL-IN-3 AS CHARACTER FORMAT "X(256)":U INITIAL "Periodeavgrensing" 
      VIEW-AS TEXT 
     SIZE 28 BY .62
     FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-49
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 65 BY 4.38.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     B-AlleBut AT ROW 4.81 COL 40 NO-TAB-STOP 
     FI-Aar AT ROW 3.57 COL 10 COLON-ALIGNED
     FI-Aar2 AT ROW 3.57 COL 38 COLON-ALIGNED NO-LABEL
     FI-Butikk AT ROW 4.81 COL 10 COLON-ALIGNED
     Btn_OK AT ROW 6.71 COL 2
     Btn_Cancel AT ROW 6.71 COL 18
     B-SokButikk AT ROW 4.81 COL 33 NO-TAB-STOP 
     Btn_Help AT ROW 6.81 COL 52
     FILL-IN-3 AT ROW 1.48 COL 3 NO-LABEL
     FI-FraTxt AT ROW 2.81 COL 11 COLON-ALIGNED NO-LABEL
     FI-TilTxt AT ROW 2.81 COL 39 COLON-ALIGNED NO-LABEL
     RECT-49 AT ROW 2.1 COL 2
     SPACE(0.79) SKIP(1.80)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Periodefilter"
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
   L-To-R                                                               */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FI-Butikk IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-FraTxt IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-TilTxt IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-3 IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON GO OF FRAME Dialog-Frame /* Periodefilter */
DO:
  if (input FI-Aar > input FI-Aar2) then
    do:
      message "Ugyldig årsangivelse!" 
        view-as alert-box message title "Melding".
      return no-apply.
    end.  
  assign
    wKriterier = string(input FI-Aar) + "," + 
                 string(input FI-Aar2) + ",1,1" +
                 string(wButik).    
  assign
    wRetStatus = "OK".
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Periodefilter */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-AlleBut
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-AlleBut Dialog-Frame
ON CHOOSE OF B-AlleBut IN FRAME Dialog-Frame /* Alle butikker */
DO:
  assign
    wButik = 0  
    FI-Butikk = wAlle.
  display 
    FI-Butikk
  with frame Dialog-Frame.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SokButikk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SokButikk Dialog-Frame
ON CHOOSE OF B-SokButikk IN FRAME Dialog-Frame /* ... */
DO:
  
  run d-bbutiker.w (input-output wButik).
  if return-value = "AVBRYT" then
    return no-apply.
  if wButik = 0
    then FI-Butikk = wAlle.
  else do:
    find Butiker no-lock where
      Butiker.Butik = wButik no-error.
    if available Butiker then
      FI-Butikk = string(wButik) + " " + Butiker.ButNamn.
    else
      FI-Butikk = "".
  end.
  display 
    FI-Butikk
  with frame Dialog-Frame.
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


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

{syspara.i 1 100 1 wAlle}
assign
  FI-Butikk = wAlle.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  {lng.i}

  IF wStTypeId = "BUTSTAT" THEN
      ASSIGN
      B-SokButikk:HIDDEN = TRUE 
      B-AlleBut:HIDDEN   = TRUE 
          .

  IF wKriterier <> "" THEN
    DO:
      ASSIGN
        FI-Aar    = int(ENTRY(1,wKriterier))
        FI-Aar2   = int(ENTRY(2,wKriterier))
        FI-Butikk = IF ENTRY(5,wKriterier) = "0" 
                      THEN wAlle
                      ELSE ENTRY(5,wKriterier).
      display
        FI-Butikk
        FI-Aar
        FI-Aar2
      with frame Dialog-Frame.
    END.
  ELSE 
    display
      FI-Butikk
      year(today) - 10 @ FI-Aar
      year(today)      @ FI-Aar2
    with frame Dialog-Frame.
  
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.
return wRetStatus.

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
  DISPLAY FI-Aar FI-Aar2 FI-Butikk FILL-IN-3 FI-FraTxt FI-TilTxt 
      WITH FRAME Dialog-Frame.
  ENABLE B-AlleBut FI-Aar FI-Aar2 Btn_OK Btn_Cancel B-SokButikk Btn_Help 
         RECT-49 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

