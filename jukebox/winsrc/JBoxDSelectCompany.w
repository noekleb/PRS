&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
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
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
&IF DEFINED(UIB_is_Running) NE 0 &THEN
  DEF VAR bSetCompany AS LOG NO-UNDO.
  DEF VAR oiCompanyId AS INT NO-UNDO.
&ELSE
  DEF INPUT PARAM bSetCompany        AS LOG NO-UNDO.
  DEF INPUT-OUTPUT PARAM oiCompanyId AS INT NO-UNDO.
&ENDIF
/* Local Variable Definitions ---                                       */

DEF VAR iCompanyId  AS INT    NO-UNDO.
DEF VAR hMainMenu   AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS cmbFirma Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS cmbFirma 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LocalTranslation Dialog-Frame 
FUNCTION LocalTranslation RETURNS LOGICAL
  ( INPUT ibAll AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "&Avbryt" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE cmbFirma AS CHARACTER FORMAT "X(256)":U 
     LABEL "Firma" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     DROP-DOWN-LIST
     SIZE 54.2 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     cmbFirma AT ROW 1.62 COL 18 COLON-ALIGNED
     Btn_OK AT ROW 3.52 COL 43.4
     Btn_Cancel AT ROW 3.52 COL 59
     SPACE(2.39) SKIP(0.19)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Velg firma"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


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
   FRAME-NAME                                                           */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON END-ERROR OF FRAME Dialog-Frame /* Velg firma */
DO:
  IF cmbFirma:SCREEN-VALUE = ? OR DYNAMIC-FUNCTION("getCompanyId") = 0 THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Velg firma */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel Dialog-Frame
ON CHOOSE OF Btn_Cancel IN FRAME Dialog-Frame /* Avbryt */
DO:
  IF cmbFirma:SCREEN-VALUE = ? OR DYNAMIC-FUNCTION("getCompanyId") = 0 THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  IF cmbFirma:SCREEN-VALUE = ? THEN RETURN NO-APPLY.

  ASSIGN cmbFirma.
  IF bSetCompany THEN
    DYNAMIC-FUNCTION("setCompanyId",INT(cmbFirma)).
  ELSE oiCompanyId = INT(cmbFirma).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

ON "SHIFT-CTRL-T" OF FRAME {&FRAME-NAME} ANYWHERE
  PUBLISH "BuildScreenObjects" (FRAME {&FRAME-NAME}:HANDLE).

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  hMainMenu = SOURCE-PROCEDURE.
  ASSIGN FRAME {&FRAME-NAME}:X = SESSION:WIDTH-PIXELS / 2 - FRAME {&FRAME-NAME}:WIDTH-PIXELS / 2 - 100 
         FRAME {&FRAME-NAME}:Y = SESSION:HEIGHT-PIXELS / 2 - FRAME {&FRAME-NAME}:HEIGHT-PIXELS - 200.
  IF DYNAMIC-FUNCTION("getFieldValues","JBoxUser","WHERE cJBoxUserId = '" + DYNAMIC-FUNCTION("getASUserId") + "'","bSuperUser") NE ? THEN DO:      
    RUN enable_UI.
    RUN InitWindow.
    WAIT-FOR GO OF FRAME {&FRAME-NAME}.
  END.
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
  DISPLAY cmbFirma 
      WITH FRAME Dialog-Frame.
  ENABLE cmbFirma Btn_OK Btn_Cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitWindow Dialog-Frame 
PROCEDURE InitWindow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cSuperUser AS CHAR NO-UNDO.
DEF VAR hMenuItem  AS HANDLE NO-UNDO.

ASSIGN iCompanyId  = oiCompanyId
       oiCompanyId = 0
       .
cSuperUser = DYNAMIC-FUNCTION("getFieldValues","JBoxUser","WHERE cJBoxUserId = '" + DYNAMIC-FUNCTION("getASUserId") + "'","bSuperUser").

IF cSuperUser NE ? THEN DO WITH FRAME {&FRAME-NAME}:

  IF VALID-HANDLE(hMainMenu) THEN DO:
    hMenuItem = DYNAMIC-FUNCTION("getCurrentMenuItem" IN hMainMenu) NO-ERROR.
    IF VALID-HANDLE(hMenuItem) THEN DO:
      FRAME {&FRAME-NAME}:TITLE = hMenuItem:LABEL.
      IF NUM-ENTRIES(hMenuItem:LABEL," ") > 1 THEN
        cmbFirma:LABEL = CAPS(SUBSTR(ENTRY(2,hMenuItem:LABEL," "),1,1)) + SUBSTR(ENTRY(2,hMenuItem:LABEL," "),2).
      ELSE 
        cmbFirma:LABEL = hMenuItem:LABEL.
      LocalTranslation(FALSE).
    END.
    ELSE LocalTranslation(TRUE).
  END.
  ELSE LocalTranslation(TRUE).
  DYNAMIC-FUNCTION("InitTranslation",FRAME {&FRAME-NAME}:HANDLE).
  cmbFirma:DELIMITER = "|".
  cmbFirma:LIST-ITEM-PAIRS = "|".
  cmbFirma:DELETE("").

  IF cSuperUser = "no":U THEN
    cmbFirma:LIST-ITEM-PAIRS = DYNAMIC-FUNCTION("getFieldList",
                                                "JBoxCompanyUser;,JBoxCompany;cCompanyName;iJBoxCompanyId",
                                                "WHERE cJBoxUserId = '" + DYNAMIC-FUNCTION("getASUserId") + "', FIRST JBoxCompany OF JBoxCompanyUser NO-LOCK BY cCompanyName").
  ELSE
    cmbFirma:LIST-ITEM-PAIRS = DYNAMIC-FUNCTION("getFieldList",
                                                "JBoxCompany;cCompanyName;iJBoxCompanyId",
                                                "BY cCompanyName").

  IF DYNAMIC-FUNCTION("getCompanyId") NE 0 THEN 
    cmbFirma:SCREEN-VALUE = STRING(DYNAMIC-FUNCTION("getCompanyId")).
  ELSE
    cmbFirma:SCREEN-VALUE = ENTRY(1,cmbFirma).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LocalTranslation Dialog-Frame 
FUNCTION LocalTranslation RETURNS LOGICAL
  ( INPUT ibAll AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose: Set english labels
    Notes:  
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:

  IF DYNAMIC-FUNCTION("getAttribute",SESSION,"CompanyIsRole") = "yes" THEN DO:
    IF DYNAMIC-FUNCTION("Scandinavian") THEN
      ASSIGN FRAME {&FRAME-NAME}:TITLE = "Velg rolle"
             cmbFirma:LABEL = "Rolle".
    ELSE
      ASSIGN FRAME {&FRAME-NAME}:TITLE = "Select role"
             cmbFirma:LABEL = "Role"
             Btn_Cancel:LABEL = "Cancel".
    RETURN YES.
  END.

  IF DYNAMIC-FUNCTION("Scandinavian") THEN RETURN FALSE.
  ELSE DO:
    Btn_Cancel:LABEL         = "Cancel".
    IF ibAll THEN
      ASSIGN cmbFirma:LABEL           = "Company"
             FRAME Dialog-Frame:TITLE = "Select company"
             .
  END.
  
  RETURN TRUE.  
END.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

