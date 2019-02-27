&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
/* Procedure Description
"Basic Window Template

Use this template to create a new window. Alter this default template or create new ones to accomodate your needs for different default sizes and/or attributes."
*/
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*********************************************************************
* Copyright (C) 2001 by Progress Software Corporation ("PSC"),       *
* 14 Oak Park, Bedford, MA 01730, and other contributors as listed   *
* below.  All Rights Reserved.                                       *
*                                                                    *
* The Initial Developer of the Original Code is PSC.  The Original   *
* Code is Progress IDE code released to open source December 1, 2000.*
*                                                                    *
* The contents of this file are subject to the Possenet Public       *
* License Version 1.0 (the "License"); you may not use this file     *
* except in compliance with the License.  A copy of the License is   *
* available as of the date of this notice at                         *
* http://www.possenet.org/license.html                               *
*                                                                    *
* Software distributed under the License is distributed on an "AS IS"*
* basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. You*
* should refer to the License for the specific language governing    *
* rights and limitations under the License.                          *
*                                                                    *
* Contributors:                                                      *
*                                                                    *
*********************************************************************/
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
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEF VAR bOK               AS LOG    NO-UNDO.
DEF VAR ix                AS INT    NO-UNDO.
DEF VAR bQuitOnCancel     AS LOG    NO-UNDO.
DEF VAR bPwdSet           AS LOG    NO-UNDO.
DEF VAR hParent           AS HANDLE NO-UNDO.
                          
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
&Scoped-Define ENABLED-OBJECTS fi-cPwd0 fi-cPwd1 fi-cPwd2 btnOK btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS fi-cPwd0 fi-cPwd1 fi-cPwd2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LocalTranslation C-Win 
FUNCTION LocalTranslation RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD PwdMixOk C-Win 
FUNCTION PwdMixOk RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setPassword C-Win 
FUNCTION setPassword RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setQuitOnCancel C-Win 
FUNCTION setQuitOnCancel RETURNS LOGICAL
  ( INPUT ibQuitOnCancel AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnOK 
     LABEL "OK" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn_Cancel 
     LABEL "&Avbryt" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE fi-cPwd0 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Eksisterende passord" 
     VIEW-AS FILL-IN 
     SIZE 23.2 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cPwd1 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Nytt passord (minst 8 tegn, inkludert tall , store og små bokstaver)" 
     VIEW-AS FILL-IN 
     SIZE 23.2 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cPwd2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Bekreft nytt passord" 
     VIEW-AS FILL-IN 
     SIZE 23.2 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     fi-cPwd0 AT ROW 1.29 COL 64.8 COLON-ALIGNED
     fi-cPwd1 AT ROW 2.91 COL 4.4
     fi-cPwd2 AT ROW 3.95 COL 64.8 COLON-ALIGNED
     btnOK AT ROW 5.52 COL 60.4
     btn_Cancel AT ROW 5.52 COL 76
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 91.6 BY 5.91
         DEFAULT-BUTTON btnOK.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window Template
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Bytt passord"
         HEIGHT             = 5.86
         WIDTH              = 91.6
         MAX-HEIGHT         = 57.14
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 57.14
         VIRTUAL-WIDTH      = 320
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

{incl/devmode.i}
{incl/custdevmode.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN fi-cPwd1 IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Bytt passord */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Bytt passord */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOK C-Win
ON CHOOSE OF btnOK IN FRAME DEFAULT-FRAME /* OK */
DO:
  ASSIGN fi-cPwd0.
  IF NOT DYNAMIC-FUNCTION("getUserConfirmation",fi-cPwd0,"",0) THEN DO:
    IF DYNAMIC-FUNCTION("Scandinavian") THEN
      DYNAMIC-FUNCTION("DoMessage",0,0,"Feil angivelse av eksisterende passord","","").
    ELSE
      DYNAMIC-FUNCTION("DoMessage",0,0,"Existing password was not entered correctly","","").
    RETURN NO-APPLY.
  END.
  IF setPassword() THEN DO:
    THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = YES.
    IF DYNAMIC-FUNCTION("Scandinavian") THEN
      DYNAMIC-FUNCTION("DoMessage",0,0,"Passordet er nå endret","","").
    ELSE
      DYNAMIC-FUNCTION("DoMessage",0,0,"Your password was changed","","").
    DYNAMIC-FUNCTION("PasswordChanged" IN hParent,YES) NO-ERROR.
    bPwdSet = YES.
    APPLY "close" TO THIS-PROCEDURE.
  END.
  ELSE RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_Cancel C-Win
ON CHOOSE OF btn_Cancel IN FRAME DEFAULT-FRAME /* Avbryt */
DO:
  APPLY "close" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cPwd0
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cPwd0 C-Win
ON ANY-PRINTABLE OF fi-cPwd0 IN FRAME DEFAULT-FRAME /* Eksisterende passord */
DO:
  RUN PostPWChar(fi-cPwd0:HWND).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cPwd1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cPwd1 C-Win
ON ANY-PRINTABLE OF fi-cPwd1 IN FRAME DEFAULT-FRAME /* Nytt passord (minst 8 tegn, inkludert tall , store og små bokstaver) */
DO:
  RUN PostPWChar(fi-cPwd1:HWND).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cPwd2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cPwd2 C-Win
ON ANY-PRINTABLE OF fi-cPwd2 IN FRAME DEFAULT-FRAME /* Bekreft nytt passord */
DO:
  RUN PostPWChar(fi-cPwd2:HWND).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
  IF NOT DYNAMIC-FUNCTION("DoCleanUpObjects",THIS-PROCEDURE:CURRENT-WINDOW) THEN RETURN NO-APPLY.

  PUBLISH "InvalidateHandle" (THIS-PROCEDURE).
  RUN disable_UI.

  IF bQuitOnCancel AND NOT bPwdSet THEN QUIT.
END.

{incl/wintrigg.i}

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  hParent = SOURCE-PROCEDURE.
  RUN enable_UI.
  LocalTranslation().
  DYNAMIC-FUNCTION("initTranslation",THIS-PROCEDURE:CURRENT-WINDOW).
  
  THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.

  ASSIGN fi-cPwd1:MODIFIED = YES
         fi-cPwd2:MODIFIED = YES.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  DISPLAY fi-cPwd0 fi-cPwd1 fi-cPwd2 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE fi-cPwd0 fi-cPwd1 fi-cPwd2 btnOK btn_Cancel 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MoveToTop C-Win 
PROCEDURE MoveToTop :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
{&WINDOW-NAME}:WINDOW-STATE = 3.
{&WINDOW-NAME}:MOVE-TO-TOP().
APPLY "entry" TO fi-cPwd0 IN FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LocalTranslation C-Win 
FUNCTION LocalTranslation RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose: Set english labels
    Notes:  
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  IF DYNAMIC-FUNCTION("Scandinavian") THEN RETURN FALSE.
  ELSE 
    ASSIGN Btn_Cancel:LABEL         = "Cancel" 
           {&WINDOW-NAME}:TITLE     = "Change password"
           fi-cPwd0:LABEL           = "Current password"
           fi-cPwd1:LABEL           = "New password (min 8 characters)"
           fi-cPwd2:LABEL           = "Confirm new password"
           .
  
  RETURN TRUE.  
END.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION PwdMixOk C-Win 
FUNCTION PwdMixOk RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR ix            AS INT NO-UNDO.
DEF VAR iAsc          AS INT NO-UNDO.          
DEF VAR bLargeCharOk  AS LOG NO-UNDO.
DEF VAR bSmallCharOk  AS LOG NO-UNDO.
DEF VAR bNumberOk     AS LOG NO-UNDO.

DO ix = 1 TO LENGTH(fi-cPwd1):
  iAsc = ASC(SUBSTR(fi-cPwd1,ix,1)).

  IF iAsc GE 48 AND iAsc LE 57 THEN
    bNumberOk = YES.
  
  IF iAsc GE 65 AND iAsc LE 90 THEN
    bLargeCharOk = YES.
  IF DYNAMIC-FUNCTION("getLanguageCode") = "NO" AND
    (iAsc = 198 OR iAsc = 216 OR iAsc = 194) THEN
    bLargeCharOk = YES.

  IF iAsc GE 97 AND iAsc LE 122 THEN
    bSmallCharOk = YES.
  IF DYNAMIC-FUNCTION("getLanguageCode") = "NO" AND
    (iAsc = 230 OR iAsc = 248 OR iAsc = 229) THEN
    bSmallCharOk = YES.

END.

RETURN bNumberOk AND bLargeCharOk AND bSmallCharOk.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setPassword C-Win 
FUNCTION setPassword RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cEncoPass    AS CHAR NO-UNDO.
DEF VAR cSessContext AS CHAR NO-UNDO.
DEF VAR cDbList      AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  IF fi-cPwd1:MODIFIED AND fi-cPwd2:MODIFIED THEN DO:
    ASSIGN fi-cPwd1 fi-cPwd2.
    IF fi-cPwd1 = "" THEN DO:
      DYNAMIC-FUNCTION("DoMessage",0,0,
                       IF DYNAMIC-FUNCTION("Scandinavian") THEN
                         "Angi passord"
                       ELSE "Enter password"
                       ,"","").
      APPLY "entry" TO fi-cPwd1.
      RETURN FALSE.
    END.
    IF NOT COMPARE(fi-cPwd1,"=",fi-cPwd2,"CASE-SENSITIVE") THEN DO:
      DYNAMIC-FUNCTION("DoMessage",0,0,
                       IF DYNAMIC-FUNCTION("Scandinavian") THEN
                         "Feil i bekreftelse av passord"
                       ELSE
                         "Confirmed password doesn't mach new password"
                       ,"","").
      APPLY "entry" TO fi-cPwd1.
      RETURN FALSE.
    END.
    IF fi-cPwd1 = fi-cPwd0 THEN DO:
      DYNAMIC-FUNCTION("DoMessage",0,0,
                       IF DYNAMIC-FUNCTION("Scandinavian") THEN
                         "Passord må endres"
                       ELSE
                         "Password must be changed"
                       ,"","").
      APPLY "entry" TO fi-cPwd1.
      RETURN FALSE.
    END.
    IF LENGTH(fi-cPwd1) < 8 THEN DO:
      DYNAMIC-FUNCTION("DoMessage",0,0,
                       IF DYNAMIC-FUNCTION("Scandinavian") THEN
                         "Passord må være minst 8 tegn"
                       ELSE 
                         "Password must be minimum 8 characters"
                       ,"","").
      APPLY "entry" TO fi-cPwd1.
      RETURN FALSE.
    END.
    IF NOT PwdMixOk() THEN DO:
      DYNAMIC-FUNCTION("DoMessage",0,0,
                       IF DYNAMIC-FUNCTION("getLanguageCode") = "NO" THEN
                         "Passord må inneholde både store og små bokstaver og minst ett tall"
                       ELSE 
                         "Password must contain a mix of large and small characters and minimum one digit"
                       ,"","").
      APPLY "entry" TO fi-cPwd1.
      RETURN FALSE.
    END.
    ELSE DO:
      cEncoPass = ENCODE(fi-cPwd1).
      cSessContext = DYNAMIC-FUNCTION("getFieldValues","JBoxLoginSession",
                                      "WHERE cSessionId = '" + DYNAMIC-FUNCTION("getSessionId") + "'",
                                      "cContext").
      DO ix = 1 TO NUM-ENTRIES(cSessContext):
        IF ENTRY(ix,cSessContext) BEGINS "dblist" THEN
          cDbList = REPLACE(ENTRY(2,ENTRY(ix,cSessContext),";"),"|",",").
      END.
      IF cDbList NE "" THEN DO:
        DO ix = 1 TO NUM-ENTRIES(cDbList):
          DYNAMIC-FUNCTION("DoDelete",ENTRY(ix,cDbList) + "._User","Avail",
                "_UserId",
                DYNAMIC-FUNCTION("getASuserId"),
                FALSE).
          DYNAMIC-FUNCTION("DoCreate",ENTRY(ix,cDbList) + "._User","",
                "_UserId,_password",
                DYNAMIC-FUNCTION("getASuserId") + "|" + cEncoPass,
                FALSE).
        END.
      END.
      ELSE DO:
        DYNAMIC-FUNCTION("DoDelete","_User","Avail",
              "_UserId",
              DYNAMIC-FUNCTION("getASuserId"),
              FALSE).
        DYNAMIC-FUNCTION("DoCreate","_User","",
              "_UserId,_password",
              DYNAMIC-FUNCTION("getASuserId") + "|" + cEncoPass,
              FALSE).
      END.
      DYNAMIC-FUNCTION("DoUpdate","JBoxUser","Ignore",
            "cJBoxUserId",DYNAMIC-FUNCTION("getASuserId"),
            "bChangePwd","no",
            FALSE).
    END.
  END.
  IF NOT DYNAMIC-FUNCTION("DoCommit",TRUE) THEN DO:
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
    RETURN FALSE.
  END.
  ELSE DO:
    IF DYNAMIC-FUNCTION("isFieldNameInTable","JBoxUser","dLastPwdChange") THEN
      DYNAMIC-FUNCTION("DoUpdate","JBoxUser","ignore",
            "cJBoxUserId",
            DYNAMIC-FUNCTION("getASuserId"),
            "dLastPwdChange",
            STRING(TODAY),
            YES).
    RETURN TRUE.  
  END. 
END.
RETURN FALSE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setQuitOnCancel C-Win 
FUNCTION setQuitOnCancel RETURNS LOGICAL
  ( INPUT ibQuitOnCancel AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
bQuitOnCancel = ibQuitOnCancel.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

