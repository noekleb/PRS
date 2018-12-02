&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
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
DEF VAR wReturn-Value AS CHAR INITIAL "AVBRYT" NO-UNDO.

{runlib.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Medlem MedlemsKort

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define QUERY-STRING-Dialog-Frame FOR EACH Medlem SHARE-LOCK, ~
      EACH MedlemsKort OF Medlem SHARE-LOCK
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH Medlem SHARE-LOCK, ~
      EACH MedlemsKort OF Medlem SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame Medlem MedlemsKort
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame Medlem
&Scoped-define SECOND-TABLE-IN-QUERY-Dialog-Frame MedlemsKort


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FI-MedlemsNr FI-KortNr FI-Etternavn ~
FI-Personnr Btn_OK Btn_Cancel Btn_Help 
&Scoped-Define DISPLAYED-OBJECTS FI-MedlemsNr FI-KortNr FI-Etternavn ~
FI-Personnr 

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

DEFINE VARIABLE FI-Etternavn AS CHARACTER FORMAT "X(256)":U 
     LABEL "Etternavn" 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE FI-KortNr LIKE MedlemsKort.KortNr
     VIEW-AS FILL-IN 
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE FI-MedlemsNr LIKE Medlem.MedlemsNr
     VIEW-AS FILL-IN 
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Personnr AS CHARACTER FORMAT "X(256)":U 
     LABEL "Personnr" 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
      Medlem, 
      MedlemsKort SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     FI-MedlemsNr AT ROW 2 COL 18 COLON-ALIGNED HELP
          ""
     FI-KortNr AT ROW 3 COL 18 COLON-ALIGNED HELP
          ""
     FI-Etternavn AT ROW 4.05 COL 18 COLON-ALIGNED
     FI-Personnr AT ROW 5.1 COL 18 COLON-ALIGNED WIDGET-ID 2
     Btn_OK AT ROW 7.1 COL 2
     Btn_Cancel AT ROW 7.1 COL 18
     Btn_Help AT ROW 7.1 COL 38
     SPACE(0.79) SKIP(0.08)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Medlemssøk"
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
   FRAME-NAME                                                           */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FI-KortNr IN FRAME Dialog-Frame
   LIKE = skotex.MedlemsKort.KortNr EXP-SIZE                            */
/* SETTINGS FOR FILL-IN FI-MedlemsNr IN FRAME Dialog-Frame
   LIKE = skotex.Medlem.MedlemsNr EXP-SIZE                              */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _TblList          = "skotex.Medlem,skotex.MedlemsKort OF skotex.Medlem"
     _Options          = "SHARE-LOCK"
     _Query            is OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON GO OF FRAME Dialog-Frame /* Medlemssøk */
DO:
  IF AVAILABLE Medlem THEN
      wReturn-value = STRING(Medlem.MedlemsNr).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Medlemssøk */
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


&Scoped-define SELF-NAME FI-Etternavn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Etternavn Dialog-Frame
ON TAB OF FI-Etternavn IN FRAME Dialog-Frame /* Etternavn */
OR "RETURN" OF FI-Etternavn
DO:
  IF INPUT FI-Etternavn <> "" THEN
  DO:
    FIND FIRST Medlem NO-LOCK WHERE
       Medlem.Etternavn BEGINS INPUT FI-Etternavn NO-ERROR.
    IF AVAILABLE Medlem THEN
        APPLY "Choose" TO Btn_Ok.
    ELSE DO:
      MESSAGE "Finner ingen medlemmer som har et etternavn som begynner med:" 
              INPUT FI-Etternavn
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY "AVBRYT".
    END.
  END.
  ELSE
      APPLY "Choose" TO Btn_Cancel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-KortNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-KortNr Dialog-Frame
ON TAB OF FI-KortNr IN FRAME Dialog-Frame /* Kortnummer */
OR "RETURN" OF FI-KortNr
DO:
  IF INPUT FI-KortNr <> "" THEN
  DO:
    FIND FIRST MedlemsKort NO-LOCK WHERE
       MedlemsKort.KortNr = INPUT FI-KortNr NO-ERROR.
    IF AVAILABLE MedlemsKort THEN
        FIND Medlem OF MedlemsKort NO-LOCK NO-ERROR.
    IF AVAILABLE Medlem AND AVAILABLE MedlemsKort THEN
        APPLY "Choose" TO Btn_Ok.
    ELSE DO:
      MESSAGE "Finner ingen medlemskort med dette søkebegrepet."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY "AVBRYT".
    END.
  END.
  ELSE
      APPLY "Choose" TO Btn_Cancel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-MedlemsNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-MedlemsNr Dialog-Frame
ON TAB OF FI-MedlemsNr IN FRAME Dialog-Frame /* Medlemsnummer */
OR "RETURN" OF FI-MedlemsNr
DO:
  IF INPUT FI-MedlemsNr > 0 THEN
  DO:
    FIND FIRST Medlem NO-LOCK WHERE
        Medlem.MedlemsNr >= INPUT FI-MedlemsNr NO-ERROR.
    IF NOT AVAILABLE Medlem THEN
    DO:
        MESSAGE "Finner ingen medlemmer med dette medlemmsnummer."
            VIEW-AS ALERT-BOX INFO BUTTONS OK TITLE "Medlemssøk".
        RETURN NO-APPLY "AVBRYT".
    END.
    ELSE
        APPLY "choose" TO Btn_Ok.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Personnr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Personnr Dialog-Frame
ON TAB OF FI-Personnr IN FRAME Dialog-Frame /* Personnr */
OR "RETURN" OF FI-Personnr
DO:
  IF INPUT FI-Personnr <> "" THEN
  DO:
    FIND FIRST Medlem NO-LOCK WHERE
       Medlem.Personnr BEGINS INPUT FI-Personnr NO-ERROR.
    IF AVAILABLE Medlem THEN
        APPLY "Choose" TO Btn_Ok.
    ELSE DO:
      MESSAGE "Finner ingen medlemmer som har et personner:" 
              INPUT FI-Personnr
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY "AVBRYT".
    END.
  END.
  ELSE
      APPLY "Choose" TO Btn_Cancel.
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

  {&OPEN-QUERY-Dialog-Frame}
  GET FIRST Dialog-Frame.
  DISPLAY FI-MedlemsNr FI-KortNr FI-Etternavn FI-Personnr 
      WITH FRAME Dialog-Frame.
  ENABLE FI-MedlemsNr FI-KortNr FI-Etternavn FI-Personnr Btn_OK Btn_Cancel 
         Btn_Help 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

