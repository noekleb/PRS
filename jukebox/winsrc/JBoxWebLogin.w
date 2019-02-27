&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
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

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
&IF DEFINED(UIB_is_Running) NE 0 &THEN
  DEF VAR icUserId       AS CHAR NO-UNDO.
  DEF VAR icPrefLanguage AS CHAR NO-UNDO.
  DEF VAR bOK            AS LOG  NO-UNDO.
&ELSE
  DEF INPUT  PARAM icUserId       AS CHAR NO-UNDO.
  DEF INPUT  PARAM icPrefLanguage AS CHAR NO-UNDO.
  DEF OUTPUT PARAM bOK            AS LOG NO-UNDO.
&ENDIF

/* Local Variable Definitions ---                                       */

DEF VAR tries      AS INT NO-UNDO.
DEF VAR passord1   AS CHAR FORMAT "x(15)" NO-UNDO.
DEF VAR passord2   AS CHAR FORMAT "x(15)" NO-UNDO.
DEF VAR enco-pass  AS CHAR FORMAT "x(15)" NO-UNDO.

DEF VAR cOldPsw           AS CHAR   NO-UNDO.
DEF VAR bErr              AS LOG    NO-UNDO.
DEF VAR ix                AS INT    NO-UNDO.
DEF VAR hPwdChange        AS HANDLE NO-UNDO.
DEF VAR bPasswordChanged  AS LOG    NO-UNDO.
DEF VAR hWaitForProc      AS HANDLE NO-UNDO.


PROCEDURE SendMessageA EXTERNAL "USER32.dll":
  DEFINE INPUT PARAMETER hHWND AS LONG.
  DEFINE INPUT PARAMETER iCmd  AS LONG.
  DEFINE INPUT PARAMETER iChar AS LONG.
  DEFINE INPUT PARAMETER ulParam AS LONG.
END PROCEDURE.

PROCEDURE PostPWChar:
  DEFINE INPUT PARAMETER hHWND AS INT.
  RUN SendMessageA(hHWND, 204, ASC("*"), 0).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS IMAGE-28 btnOK btnCancel 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD PasswordChanged C-Win 
FUNCTION PasswordChanged RETURNS LOGICAL
  ( INPUT ibPasswordChanged AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCancel 
     LABEL "&Cancel" 
     SIZE 12 BY 1.14.

DEFINE BUTTON btnOK AUTO-END-KEY 
     LABEL "OK" 
     SIZE 12 BY 1.14.

DEFINE IMAGE IMAGE-28
     FILENAME "bmp/cat00003.bmp":U
     SIZE 63 BY 8.33.

DEFINE VARIABLE cmbDateFormat AS CHARACTER FORMAT "X(256)":U INITIAL "dmy" 
     LABEL "Date format" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "dmy","mdy","ymd" 
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE cmbLanguage AS CHARACTER FORMAT "X(256)":U 
     LABEL "Language" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE cmbNumFormat AS CHARACTER FORMAT "X(256)":U INITIAL "European" 
     LABEL "Num.format" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "European","American" 
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cPassword AS CHARACTER FORMAT "X(256)":U 
     LABEL "Password" 
     VIEW-AS FILL-IN 
     SIZE 15.8 BY .91 NO-UNDO.

DEFINE VARIABLE fi-cUserId AS CHARACTER FORMAT "X(256)":U 
     LABEL "Userid" 
     VIEW-AS FILL-IN 
     SIZE 31 BY .91 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnOK AT ROW 8 COL 38
     btnCancel AT ROW 8 COL 51.4
     IMAGE-28 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 63 BY 8.33
         DEFAULT-BUTTON btnOK.

DEFINE FRAME frmInput
     fi-cUserId AT ROW 1.1 COL 11.4 COLON-ALIGNED
     fi-cPassword AT ROW 2.05 COL 3 BLANK 
     cmbLanguage AT ROW 3.05 COL 11.4 COLON-ALIGNED
     cmbDateFormat AT ROW 4.1 COL 11.4 COLON-ALIGNED
     cmbNumFormat AT ROW 5.14 COL 11.4 COLON-ALIGNED
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 1.52
         SIZE 44.2 BY 5.33.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "JBox login"
         COLUMN             = 104.4
         ROW                = 12.38
         HEIGHT             = 8.33
         WIDTH              = 63
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 80
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-ICON("ico/cntrlhry.ico":U) THEN
    MESSAGE "Unable to load icon: ico/cntrlhry.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{incl/demo.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME frmInput:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR FRAME frmInput
                                                                        */
/* SETTINGS FOR COMBO-BOX cmbLanguage IN FRAME frmInput
   NO-ENABLE                                                            */
ASSIGN 
       cmbLanguage:HIDDEN IN FRAME frmInput           = TRUE.

/* SETTINGS FOR FILL-IN fi-cPassword IN FRAME frmInput
   ALIGN-L                                                              */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME frmInput
/* Query rebuild information for FRAME frmInput
     _Query            is NOT OPENED
*/  /* FRAME frmInput */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* JBox login */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* JBox login */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel C-Win
ON CHOOSE OF btnCancel IN FRAME DEFAULT-FRAME /* Cancel */
DO:
  &IF DEFINED (adm-panel) <> 0 &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOK C-Win
ON CHOOSE OF btnOK IN FRAME DEFAULT-FRAME /* OK */
DO:
  RUN CheckUser (OUTPUT bOk).
  IF NOT bOk THEN DO:
    APPLY "ENTRY" TO fi-cUserId IN FRAME frmInput.
    RETURN NO-APPLY.
  END.
  ELSE IF bOk = ? THEN DO:
    THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = YES.  
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmInput
&Scoped-define SELF-NAME fi-cPassword
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cPassword C-Win
ON ANY-PRINTABLE OF fi-cPassword IN FRAME frmInput /* Password */
DO:
  RUN PostPWChar(fi-cPassword:HWND).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
  IF VALID-HANDLE(hWaitForProc) THEN DELETE PROCEDURE hWaitForProc.
  RUN disable_UI.
END.   

ON ENDKEY, END-ERROR OF {&WINDOW-NAME} ANYWHERE DO:
  QUIT.
END.

ON RETURN OF {&WINDOW-NAME} ANYWHERE DO:
  IF NOT bErr THEN APPLY 'CHOOSE' TO BtnOk IN FRAME {&FRAME-NAME}.
END.    

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.

  RUN InitWin.
  
  SESSION:SET-WAIT-STATE("").
  IF NOT THIS-PROCEDURE:PERSISTENT THEN 
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CheckUser C-Win 
PROCEDURE CheckUser :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF OUTPUT PARAM bOkUser AS LOG    NO-UNDO.
DEF VAR hAppservice      AS HANDLE NO-UNDO.
DEF VAR ocSessionId      AS CHAR   NO-UNDO.
DEF VAR oiCompanyId      AS INT    NO-UNDO.
DEF VAR ocASuserName     AS CHAR   NO-UNDO.
DEF VAR cUserStatus      AS CHAR   NO-UNDO.
DEF VAR bChangePwd       AS LOG    NO-UNDO.
DEF VAR cLangCode        AS CHAR   NO-UNDO.

DO WITH FRAME frmInput:
  ASSIGN SESSION:DATE-FORMAT    = cmbDateFormat:SCREEN-VALUE
         SESSION:NUMERIC-FORMAT = cmbNumFormat:SCREEN-VALUE
         .
  IF NOT bJukeBoxDemo THEN DO:
    ASSIGN fi-cUserId 
           fi-cPassword.
    hAppservice = DYNAMIC-FUNCTION("getAppserviceHandle").
    IF NOT cmbLanguage:SENSITIVE THEN 
      cLangCode = DYNAMIC-FUNCTION("getLanguages").
    ELSE
      cLangCode = cmbLanguage:SCREEN-VALUE.
        
    DYNAMIC-FUNCTION("setASUserId",fi-cUserId,"").
    bOkUser = DYNAMIC-FUNCTION("getUserConfirmation",fi-cPassword,"",0).
  

    IF NOT bOkUser THEN DO:
      IF cLangCode = "NO" THEN
        MESSAGE "Beklager, bruker/passord er ugyldig" VIEW-AS ALERT-BOX.
      ELSE 
        MESSAGE "Invalid userid/password" VIEW-AS ALERT-BOX.
  
      IF tries > 3 THEN QUIT.   /* only allow 3 tries*/
      tries = tries + 1.
      bOkUser = FALSE.
      RETURN.
    END.
    ELSE DO:
      IF VALID-HANDLE(hAppservice) THEN
        RUN jbserv_validateuser.p ON hAppservice (fi-cUserId,CHR(3),
                                           SESSION:DATE-FORMAT,
                                           SESSION:NUMERIC-FORMAT,
                                           OUTPUT ocSessionId,
                                           OUTPUT ocASuserName,
                                           OUTPUT oiCompanyId,
                                           OUTPUT bOkUser).
      ELSE 
        RUN jbserv_validateuser.p         (fi-cUserId,CHR(3),
                                           SESSION:DATE-FORMAT,
                                           SESSION:NUMERIC-FORMAT,
                                           OUTPUT ocSessionId,
                                           OUTPUT ocASuserName,
                                           OUTPUT oiCompanyId,
                                           OUTPUT bOkUser).

      IF NOT bOkUser THEN DO:
        IF DYNAMIC-FUNCTION("getLanguageCode") = "NO" THEN
          MESSAGE "Bruker må aktiveres for å kunne logge inn" VIEW-AS ALERT-BOX.
        ELSE 
          MESSAGE "User must be activated to proceed login" VIEW-AS ALERT-BOX.
        QUIT.  
      END. 

      DYNAMIC-FUNCTION("setSessionId",ocSessionId).
      DYNAMIC-FUNCTION("setASUserId",fi-cUserId,ocASuserName).
      DYNAMIC-FUNCTION("setLanguageCode",cLangCode).
      IF oiCompanyId NE 0 THEN
        DYNAMIC-FUNCTION("setCompanyId",oiCompanyId).
      ELSE 
        DYNAMIC-FUNCTION("getFunctionRestrictions").

      IF DYNAMIC-FUNCTION("runProc","jbserv_checkuserstatus.p","",?) THEN DO:
        cUserStatus = DYNAMIC-FUNCTION("getTransactionMessage").
        IF cUserStatus NE "" THEN DO:
          IF ENTRY(2,cUserStatus,"|") NE "" THEN
            bChangePwd = DYNAMIC-FUNCTION("DoMessage",0,4,
                                          IF DYNAMIC-FUNCTION("Scandinavian") THEN 
                                            "Passordet ditt utløper om " + ENTRY(2,cUserStatus,"|") + " dager." + CHR(10) + "Endre nå?"
                                          ELSE
                                            "Your password expires in " + ENTRY(2,cUserStatus,"|") + " days." + CHR(10) + "Change it now?"
                                         ,"","") = 6.
          ELSE bChangePwd = LOGICAL(ENTRY(1,cUserStatus,"|")).
  
          IF bChangePwd THEN DO:
            RUN JBoxChangePwd.w PERSIST SET hPwdChange.
            DYNAMIC-FUNCTION("setQuitOnCancel" IN hPwdChange,YES).
            RUN MoveToTop IN hPwdChange.
            SUBSCRIBE TO "InvalidateHandle" IN hPwdChange.
            bOkUser = ?.
            RETURN.
          END.
        END.
      END. 
    END.
  
    APPLY "CLOSE" TO THIS-PROCEDURE.
  END.
  ELSE DO WITH FRAME frmInput:
    bOkUser = IF cmbLanguage:SCREEN-VALUE = "EN" THEN TRUE ELSE ?.
    APPLY "CLOSE" TO THIS-PROCEDURE.
  END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  ENABLE IMAGE-28 btnOK btnCancel 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  DISPLAY fi-cUserId fi-cPassword cmbLanguage cmbDateFormat cmbNumFormat 
      WITH FRAME frmInput IN WINDOW C-Win.
  ENABLE fi-cUserId fi-cPassword cmbDateFormat cmbNumFormat 
      WITH FRAME frmInput IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frmInput}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitWin C-Win 
PROCEDURE InitWin :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME frmInput:
  IF NOT bJukeBoxDemo THEN DO:
    IF NUM-ENTRIES(DYNAMIC-FUNCTION("getLanguages"),"|") > 1 THEN 
      ASSIGN cmbLanguage:HIDDEN       = FALSE
             cmbLanguage:DELIMITER    = "|"
             cmbLanguage:SENSITIVE    = TRUE
             cmbLanguage:LIST-ITEMS   = DYNAMIC-FUNCTION("getLanguages")
             cmbLanguage:SCREEN-VALUE = IF icPrefLanguage = "" THEN 
                                          DYNAMIC-FUNCTION("getBaseLanguageCode")
                                        ELSE icPrefLanguage
             .
    {&WINDOW-NAME}:TITLE = DYNAMIC-FUNCTION("getAppTitle") + " login".  

  END.
  ELSE 
    ASSIGN cmbLanguage:HIDDEN        = FALSE
           cmbLanguage:DELIMITER    = "|"
           cmbLanguage:SENSITIVE    = TRUE
           cmbLanguage:LIST-ITEMS    = "EN|NO"
           fi-cUserId:SCREEN-VALUE   = "<whatever>"
           cmbLanguage:SCREEN-VALUE  = IF icPrefLanguage = "" THEN "EN"
                                       ELSE icPrefLanguage
           .

  ASSIGN cmbDateFormat:SCREEN-VALUE = SESSION:DATE-FORMAT
         cmbNumFormat:SCREEN-VALUE  = SESSION:NUMERIC-FORMAT.

  IF icUserId NE ? THEN fi-cUserId:SCREEN-VALUE = icUserId.
  IF icUserId NE "" AND NOT icUserId BEGINS "<" THEN DO:
    APPLY "ENTRY" TO fi-cPassword.
  END.
  ELSE APPLY "ENTRY" TO fi-cUserid.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InvalidateHandle C-Win 
PROCEDURE InvalidateHandle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihProc AS HANDLE NO-UNDO.
IF ihProc = hPwdChange THEN DO:
  bOk = bPasswordChanged.  
  APPLY "close" TO THIS-PROCEDURE.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION PasswordChanged C-Win 
FUNCTION PasswordChanged RETURNS LOGICAL
  ( INPUT ibPasswordChanged AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
bPasswordChanged = ibPasswordChanged.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

