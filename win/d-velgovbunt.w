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
    DEFINE VARIABLE iBuntNr  AS INTEGER  INIT ?  NO-UNDO.
    DEFINE VARIABLE cBuntTxt AS CHARACTER  NO-UNDO.
&ELSE
    DEFINE INPUT-OUTPUT PARAMETER iBuntNr  AS INTEGER    NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER cBuntTxt AS CHARACTER  NO-UNDO.
&ENDIF



/* Local Variable Definitions ---                                       */

DEFINE VARIABLE lovBuntFinnes AS LOGICAL    NO-UNDO.
DEFINE VARIABLE cReturVerdi   AS CHARACTER  INIT "AVBRYT" NO-UNDO.
DEFINE VARIABLE lVisTG-Oppdater AS LOGICAL    NO-UNDO.
DEFINE VARIABLE cKoble    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cOppdater AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cUtskrift AS CHARACTER  NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_OK RS-ovType Btn_Cancel Btn_Help ~
CB-ovVelg FI-BuntTxt TG-Oppdater TG-Utskrift 
&Scoped-Define DISPLAYED-OBJECTS RS-ovType CB-ovVelg FI-BuntTxt TG-Oppdater ~
TG-Utskrift 

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

DEFINE VARIABLE CB-ovVelg AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 42 BY 1 NO-UNDO.

DEFINE VARIABLE FI-BuntTxt AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1 NO-UNDO.

DEFINE VARIABLE RS-ovType AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Koble til overføring", 1,
"Ny overføring", 2
     SIZE 27 BY 2.14 NO-UNDO.

DEFINE VARIABLE TG-Oppdater AS LOGICAL INITIAL no 
     LABEL "Oppdater overføring direkte" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE TG-Utskrift AS LOGICAL INITIAL no 
     LABEL "Utskrift overføringsrapport" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     Btn_OK AT ROW 1.52 COL 49
     RS-ovType AT ROW 1.71 COL 4 NO-LABEL
     Btn_Cancel AT ROW 2.76 COL 49
     Btn_Help AT ROW 4.76 COL 49
     CB-ovVelg AT ROW 5.05 COL 2 COLON-ALIGNED NO-LABEL
     FI-BuntTxt AT ROW 6.71 COL 2 COLON-ALIGNED NO-LABEL
     TG-Oppdater AT ROW 8.43 COL 4
     TG-Utskrift AT ROW 9.48 COL 4
     SPACE(30.19) SKIP(0.85)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Velg overføring"
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
   FRAME-NAME                                                           */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

ASSIGN 
       FI-BuntTxt:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON GO OF FRAME Dialog-Frame /* Velg overføring */
DO:
  ASSIGN cBuntTxt = IF FI-BuntTxt:SCREEN-VALUE = "" THEN cBuntTxt
          ELSE FI-BuntTxt:SCREEN-VALUE.
  IF TG-Oppdater:CHECKED THEN
      ASSIGN cBuntTxt = "J" + CHR(1) + cBuntTxt + 
                (IF TG-Utskrift:CHECKED THEN CHR(1) + "J" ELSE "").
  ELSE ASSIGN cBuntTxt = "N" + CHR(1) + cBuntTxt.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Velg overføring */
DO:
  APPLY "END-ERROR":U TO SELF.
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


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
    ASSIGN cReturVerdi = "OK"
           iBuntNr     = IF RS-ovType:SCREEN-VALUE = "1" THEN
                         INT(CB-ovVelg:SCREEN-VALUE) ELSE ?.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RS-ovType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RS-ovType Dialog-Frame
ON VALUE-CHANGED OF RS-ovType IN FRAME Dialog-Frame
DO:
  ASSIGN CB-ovVelg:SENSITIVE = SELF:SCREEN-VALUE = "1"
         FI-BuntTxt:HIDDEN = NOT (SELF:SCREEN-VALUE = "2" AND FI-BuntTxt <> "").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-Oppdater
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-Oppdater Dialog-Frame
ON VALUE-CHANGED OF TG-Oppdater IN FRAME Dialog-Frame /* Oppdater overføring direkte */
DO:
    IF SELF:CHECKED THEN DO:
        ASSIGN RS-ovType:SCREEN-VALUE = "2"
               RS-ovType:SENSITIVE = FALSE.
    END.
    ELSE DO:
        IF NOT CB-ovVelg:SCREEN-VALUE = ? THEN
            ASSIGN RS-ovType:SENSITIVE = TRUE.
        TG-Utskrift:CHECKED = FALSE.
    END.
    TG-Utskrift:SENSITIVE = SELF:CHECKED.
    APPLY "VALUE-CHANGED" TO RS-ovType.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-Utskrift
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-Utskrift Dialog-Frame
ON VALUE-CHANGED OF TG-Utskrift IN FRAME Dialog-Frame /* Utskrift overføringsrapport */
DO:
/*     IF SELF:CHECKED THEN DO:                   */
/*         ASSIGN RS-ovType:SCREEN-VALUE = "2"    */
/*                RS-ovType:SENSITIVE = FALSE.    */
/*     END.                                       */
/*     ELSE DO:                                   */
/*         IF NOT CB-ovVelg:SCREEN-VALUE = ? THEN */
/*             ASSIGN RS-ovType:SENSITIVE = TRUE. */
/*     END.                                       */
/*     APPLY "VALUE-CHANGED" TO RS-ovType.        */
    RETURN NO-APPLY.
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
   {syspara.i 11 4 1 cKoble}
   {syspara.i 11 4 2 cOppdater}
   {syspara.i 11 4 3 cUtskrift}
IF cBuntTxt = "" THEN DO:
    cBuntTxt = "J" + CHR(1) + "Inleverans beställning".
END.
  IF CAN-FIND(FIRST ovBunt WHERE ovBunt.DatoOppdatert = ?) THEN DO:
      RUN CreateListItemPairs.
  END.
  ELSE
      ASSIGN RS-ovType = 2.
  ASSIGN lVisTG-Oppdater = NUM-ENTRIES(cBuntTxt,CHR(1)) = 2 AND
                           ENTRY(1,cBuntTxt,CHR(1)) = "J"
         cBuntTxt   = IF NUM-ENTRIES(cBuntTxt,CHR(1)) = 2 THEN 
                       ENTRY(2,cBuntTxt,CHR(1))
                       ELSE cBuntTxt
         FI-BuntTxt = cBuntTxt.
  RUN enable_UI.
  ASSIGN FI-BuntTxt:HIDDEN = TRUE
         TG-Oppdater:HIDDEN = lVisTG-Oppdater = FALSE. 
  ASSIGN RS-ovType:SENSITIVE = RS-ovType <> 2
         CB-ovVelg:SENSITIVE = RS-ovType:SENSITIVE.
  IF CB-ovVelg:SENSITIVE THEN
      ASSIGN CB-ovVelg:SCREEN-VALUE = 
      IF CAN-DO(CB-ovVelg:LIST-ITEM-PAIRS,STRING(iBuntNr)) THEN
          STRING(iBuntNr) ELSE ENTRY(2,CB-ovVelg:LIST-ITEM-PAIRS).
  ASSIGN iBuntNr = ?.
  APPLY "VALUE-CHANGED" TO RS-ovType.
  IF RS-ovType <> 2 AND TRIM(cKoble) = "0" THEN
      ASSIGN RS-ovType:SCREEN-VALUE = "2".
  IF RS-ovType:SCREEN-VALUE = "2" AND TRIM(cOppdater) = "1" THEN
      ASSIGN TG-Oppdater:CHECKED = TRUE
             TG-Utskrift:CHECKED = TRIM(cUtskrift) = "1".
  APPLY "VALUE-CHANGED" TO TG-Oppdater.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.
RETURN cReturVerdi.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CreateListItemPairs Dialog-Frame 
PROCEDURE CreateListItemPairs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cListItemPairs AS CHARACTER  NO-UNDO.
  FOR EACH ovBunt WHERE ovBunt.DatoOppdatert = ? NO-LOCK BY OvBunt.Buntnr DESCENDING:
      ASSIGN cListItemPairs = cListItemPairs + 
                     (IF cListItemPairs = "" THEN "" ELSE ",") +
             STRING(OvBunt.BuntNr) + " " + REPLACE(OvBunt.Merknad,",","-") + "," +
                    STRING(OvBunt.BuntNr).
  END.
  ASSIGN CB-ovVelg:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = cListItemPairs.
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
  DISPLAY RS-ovType CB-ovVelg FI-BuntTxt TG-Oppdater TG-Utskrift 
      WITH FRAME Dialog-Frame.
  ENABLE Btn_OK RS-ovType Btn_Cancel Btn_Help CB-ovVelg FI-BuntTxt TG-Oppdater 
         TG-Utskrift 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

