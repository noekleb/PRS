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
DEF VAR wReturn-value AS CHAR NO-UNDO.
DEF VAR wMaksLengde   AS INT  NO-UNDO.
DEF VAR wTekst        AS CHAR NO-UNDO.
DEF VAR wAlfaTegn     AS LOG  NO-UNDO.
DEF VAR wLoop         AS INT  NO-UNDO.
DEF VAR iSisteNr      AS INT NO-UNDO.
DEF VAR iCl           AS INT NO-UNDO.

{runlib.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FI-Kortnummer Btn_OK Btn_Cancel Btn_Help 
&Scoped-Define DISPLAYED-OBJECTS FI-Kortnummer 

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

DEFINE VARIABLE FI-Kortnummer AS CHARACTER FORMAT "X(256)":U 
     LABEL "Kortnummer" 
     VIEW-AS FILL-IN 
     SIZE 37 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     FI-Kortnummer AT ROW 3.14 COL 13 COLON-ALIGNED
     Btn_OK AT ROW 6.71 COL 2
     Btn_Cancel AT ROW 6.71 COL 17.4
     Btn_Help AT ROW 6.71 COL 40
     SPACE(0.39) SKIP(0.05)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Registrering av kortnummer"
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

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON GO OF FRAME Dialog-Frame /* Registrering av kortnummer */
DO:
  IF (LENGTH(INPUT FI-KortNummer) > wMaksLengde AND
      LENGTH(INPUT FI-KortNummer) <> 10 AND 
      LENGTH(INPUT FI-KortNummer) <> 11)  
       THEN
  DO:
      MESSAGE "For mange siffer i kortnummer." SKIP
              "Maksimalt " wMaksLengde " siffer kan benyttes."
          VIEW-AS ALERT-BOX ERROR BUTTONS OK TITLE "Lagringsfeil".
      RETURN NO-APPLY.
  END.

  IF wAlfaTegn = FALSE THEN
  DO:
      do wLoop = 1 TO LENGTH(INPUT FI-KortNummer):
          IF NOT can-do("0,1,2,3,4,5,6,7,8,9",SUBSTRING(INPUT FI-KortNummer,wLoop,1)) THEN
          DO:
              MESSAGE "Kun siffer kan benyttes i kortnummeret."
                  VIEW-AS ALERT-BOX error BUTTONS OK TITLE "Lagringsfeil".
              RETURN NO-APPLY.
          END.
      END.
  END.
  
  IF CAN-FIND(FIRST MedlemsKort WHERE
              MedlemsKort.KortNr = INPUT FI-KortNummer) THEN
  DO:
      MESSAGE "Det finnes allerede et kort med dette kortnummeret."
          VIEW-AS ALERT-BOX error BUTTONS OK TITLE "Lagringsfeil".
      RETURN NO-APPLY "AVBRYT".
  END.
  
  ASSIGN
      wReturn-Value = STRING(INPUT FI-Kortnummer,"999999").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Registrering av kortnummer */
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
  {diahelp.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

{syspara.i 14 1 1 wMaksLengde INT}
{syspara.i 14 1 2 wTekst}
{syspara.i 5 1 1 iCl INT}

ASSIGN
    wAlfaTegn     = IF CAN-DO("Nei,No,False",wTekst)
                      THEN FALSE
                      ELSE TRUE
    wReturn-value = "AVBRYT".

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  {lng.i}

  {syspara.i 14 2 5 iSisteNr INT}
  LOOPEN:
  DO WHILE TRUE:
      iSisteNr = iSisteNr + 1.
      IF NOT CAN-FIND(MedlemsKort WHERE
                      MedlemsKort.KortNr = STRING(iCl) + STRING(iSisteNr,"999999")) THEN
          LEAVE LOOPEN.
  END. /* LOOPEN */
  DO TRANSACTION:
      FIND SysPara EXCLUSIVE-LOCK where
        SysPara.SysHId = 14 and
        SysPara.SysGr  = 2 and
        SysPara.ParaNr = 5 NO-ERROR.
      if AVAILABLE SysPara then DO:
          ASSIGN SysPara.Parameter1 = STRING(iSisteNr).
      END.
      IF AVAILABLE SysPara THEN
          RELEASE SysPara.
  END.
  FI-Kortnummer:SCREEN-VALUE = STRING(iSisteNr).

  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

RETURN wReturn-value.

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
  DISPLAY FI-Kortnummer 
      WITH FRAME Dialog-Frame.
  ENABLE FI-Kortnummer Btn_OK Btn_Cancel Btn_Help 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

