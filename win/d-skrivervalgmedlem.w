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
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN
    DEFINE VARIABLE iPrinternr AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iAntall  AS INTEGER    NO-UNDO.
&ELSE
    DEFINE OUTPUT PARAMETER iPrinternr AS INTEGER  NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iAntall  AS INTEGER    NO-UNDO.
&ENDIF
/* Local Variable Definitions ---                                       */
DEFINE VARIABLE cStartEtiPrinter AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cLAYOUT  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cSKRIVER AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cListItemPairs AS CHARACTER  NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS RECT-55 Btn_OK CB-Printer Btn_Cancel ~
FI-Antall 
&Scoped-Define DISPLAYED-OBJECTS CB-Printer FI-Antall 

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

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE CB-Printer AS INTEGER FORMAT ">>9":U INITIAL 0 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1",0
     DROP-DOWN-LIST
     SIZE 31 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Antall AS INTEGER FORMAT ">9":U INITIAL 1 
     LABEL "Antall" 
     VIEW-AS FILL-IN 
     SIZE 4.4 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-55
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 36.8 BY 2.95.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     Btn_OK AT ROW 1.57 COL 40.2
     CB-Printer AT ROW 1.62 COL 2 COLON-ALIGNED NO-LABEL
     Btn_Cancel AT ROW 2.81 COL 40.2
     FI-Antall AT ROW 2.91 COL 27.8 COLON-ALIGNED
     RECT-55 AT ROW 1.38 COL 1.2
     SPACE(18.79) SKIP(0.09)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Printer/startetikett"
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

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON GO OF FRAME Dialog-Frame /* Printer/startetikett */
DO:
  ASSIGN iPrinternr = INT(CB-Printer:SCREEN-VALUE)
         iAntall  = INT(FI-Antall:SCREEN-VALUE).
/*   RUN SaveSettings. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Printer/startetikett */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel Dialog-Frame
ON CHOOSE OF Btn_Cancel IN FRAME Dialog-Frame /* Avbryt */
DO:
  ASSIGN iAntall = 0.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  IF INT(FI-Antall:SCREEN-VALUE) = 0 THEN DO:
      MESSAGE "Angi antall"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY "ENTRY" TO FI-Antall.
      RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Printer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Printer Dialog-Frame
ON VALUE-CHANGED OF CB-Printer IN FRAME Dialog-Frame
DO:
      APPLY "ENTRY" TO FI-Antall.
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
/*   RUN GetLastPrinter. */
  RUN InitCombo.
  ASSIGN FI-Antall = iAntall.
  {lng.i}
  RUN enable_UI.
  ASSIGN FI-Antall:SENSITIVE = iAntall = 0.
  APPLY "VALUE-CHANGED" TO CB-Printer.
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
  DISPLAY CB-Printer FI-Antall 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-55 Btn_OK CB-Printer Btn_Cancel FI-Antall 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetLastPrinter Dialog-Frame 
PROCEDURE GetLastPrinter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF VALID-HANDLE(wLibHandle) THEN
  DO WITH FRAME default-frame:
    RUN HentParametre IN wLibHandle ("ETIKETTER", "LAYOUT",  OUTPUT cLAYOUT).
    RUN HentParametre IN wLibHandle ("ETIKETTER", "SKRIVER", OUTPUT cSKRIVER).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitCombo Dialog-Frame 
PROCEDURE InitCombo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE BUFFER bSysPara FOR SysPara.
    FOR EACH SysPara WHERE SysPara.SysHId = 5 AND 
                           SysPara.SysGr = 21 AND 
                           ENTRY(1,SysPara.Parameter2,";") = "MEDLEM" NO-LOCK:
        FIND bSysPara WHERE bSysPara.SysHId = 5 AND 
                            bSysPara.SysGr  = 20 AND
                            bSysPara.ParaNr = SysPara.ParaNr NO-LOCK NO-ERROR.
        IF AVAIL bSysPara THEN DO:
            ASSIGN cListItemPairs = cListItemPairs + (IF cListItemPairs = "" THEN "" ELSE ",") +
                   bSysPara.Parameter2 + "," + TRIM(STRING(bSysPara.ParaNr)).
        END.
    END.
    ASSIGN CB-Printer:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME}= cListItemPairs.
/*     IF cSKRIVER <> ? AND CAN-DO(cListItemPairs,cSKRIVER) THEN */
/*         CB-Printer = INT(cSKRIVER).                           */
/*     ELSE                                                      */
        CB-Printer = INT(ENTRY(2,cListItemPairs)).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SaveSettings Dialog-Frame 
PROCEDURE SaveSettings :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF VALID-HANDLE(wLibHandle) THEN
  DO WITH FRAME {&FRAME-NAME}:
    RUN LagreParametre IN wLibHandle ("ETIKETTER", "LAYOUT", ENTRY(LOOKUP(CB-Printer:SCREEN-VALUE,cListItemPairs) - 1,CB-Printer:LIST-ITEM-PAIRS)).
    RUN LagreParametre IN wLibHandle ("ETIKETTER", "SKRIVER", CB-Printer:SCREEN-VALUE).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

