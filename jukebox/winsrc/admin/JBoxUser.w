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
&IF "{1}" = "Developer_Studio_is_Running" &THEN
  &SCOPED-DEFINE UIB_is_Running 1 
&ENDIF   
/* Uncomment to enable use of .Net components: */
/* &SCOPED-DEFINE AdvGuiWin */ 

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEF VAR bOK               AS LOG NO-UNDO.
DEF VAR ix                AS INT NO-UNDO.
                          
DEF VAR hToolbar          AS HANDLE NO-UNDO.
DEF VAR hBrowse           AS HANDLE NO-UNDO.
DEF VAR hFieldMap         AS HANDLE NO-UNDO.
DEF VAR hBrowseCompany    AS HANDLE NO-UNDO.
DEF VAR hBrowseUserGrp    AS HANDLE NO-UNDO.
DEF VAR hToolbarCompany   AS HANDLE NO-UNDO.
DEF VAR hAdminOverlay     AS HANDLE NO-UNDO.
DEF VAR hSearchField      AS HANDLE NO-UNDO.
DEF VAR hBrwColSuperUser  AS HANDLE NO-UNDO.
DEF VAR hBrwColCompAdmin  AS HANDLE NO-UNDO.
DEF VAR cDbList           AS CHAR   NO-UNDO.
DEF VAR hDbToggle         AS HANDLE NO-UNDO EXTENT 10.
DEF VAR bEnglish          AS LOG    NO-UNDO.
DEF VAR iFontWingdings    AS INT    NO-UNDO.
iFontWingdings = DYNAMIC-FUNCTION("setAppFont","Wingdings, size=11 Script=symbol","").

DEF VAR bUserGroupTbl     AS LOG    NO-UNDO.
DEF VAR bCompUserTbl      AS LOG    NO-UNDO.
DEF VAR hUserActivity     AS HANDLE NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS rectBrowse rectToolBar rectBrwCompanyUser ~
RectBrowseSearch brwUserGroup cJBoxUserId cUserName cTitle cEmail cWrkPhone ~
cCellPhone fi-cPwd1 fi-cPwd2 iPwdInterval dLastPwdChange bSuperUser bActive ~
bProcessUser bChangePwd 
&Scoped-Define DISPLAYED-OBJECTS cJBoxUserId cUserName cTitle cEmail ~
cWrkPhone cCellPhone fi-cPwd1 fi-cPwd2 iPwdInterval dLastPwdChange ~
bSuperUser bActive bProcessUser bChangePwd 

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


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE cCellPhone AS CHARACTER FORMAT "x(15)" 
     LABEL "Mobil" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.

DEFINE VARIABLE cEmail AS CHARACTER FORMAT "x(50)" 
     LABEL "Email" 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1.

DEFINE VARIABLE cJBoxUserId AS CHARACTER FORMAT "X(40)" 
     LABEL "Brukerid" 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1 NO-UNDO.

DEFINE VARIABLE cTitle AS CHARACTER FORMAT "x(25)" 
     LABEL "Tittel" 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1.

DEFINE VARIABLE cUserName AS CHARACTER FORMAT "X(50)" 
     LABEL "Navn" 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1 NO-UNDO.

DEFINE VARIABLE cWrkPhone AS CHARACTER FORMAT "x(15)" 
     LABEL "Tlf.arb" 
     VIEW-AS FILL-IN 
     SIZE 15.4 BY 1.

DEFINE VARIABLE dLastPwdChange AS DATE FORMAT "99/99/9999" 
     LABEL "Sist endret passord" 
     VIEW-AS FILL-IN 
     SIZE 15.2 BY 1.

DEFINE VARIABLE fi-cPwd1 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Passord" 
     VIEW-AS FILL-IN 
     SIZE 15.4 BY 1 TOOLTIP "Passord må bare angis ved registrering - kan også endres" NO-UNDO.

DEFINE VARIABLE fi-cPwd2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Bekreft" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE iPwdInterval AS INTEGER FORMAT ">>>>9" INITIAL 0 
     LABEL "Passord-intervall" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 TOOLTIP "Antall dager mellom tvungent skifte av passord".

DEFINE RECTANGLE brwUserGroup
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 24 BY 6.91.

DEFINE RECTANGLE rectBrowse
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 40 BY 17.62.

DEFINE RECTANGLE RectBrowseSearch
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 17.6 BY .95.

DEFINE RECTANGLE rectBrwCompanyUser
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 40.4 BY 6.91.

DEFINE RECTANGLE rectToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 28 BY .95.

DEFINE VARIABLE bActive AS LOGICAL INITIAL yes 
     LABEL "Aktiv" 
     VIEW-AS TOGGLE-BOX
     SIZE 10 BY .81.

DEFINE VARIABLE bChangePwd AS LOGICAL INITIAL no 
     LABEL "Passord må endres" 
     VIEW-AS TOGGLE-BOX
     SIZE 21.8 BY .95 TOOLTIP "Passord må endres ved neste innlogging" NO-UNDO.

DEFINE VARIABLE bProcessUser AS LOGICAL INITIAL no 
     LABEL "Prosess-bruker" 
     VIEW-AS TOGGLE-BOX
     SIZE 18.2 BY 1.

DEFINE VARIABLE bSuperUser AS LOGICAL INITIAL yes 
     LABEL "Superbruker" 
     VIEW-AS TOGGLE-BOX
     SIZE 16.6 BY .81 TOOLTIP "Superbruker kan administrere alle firma" NO-UNDO.

DEFINE BUTTON btnSplitBarX 
     IMAGE-UP FILE "bmp/tableft.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Button 1" 
     SIZE 2 BY 17.57.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     cJBoxUserId AT ROW 2.76 COL 61 COLON-ALIGNED
     cUserName AT ROW 3.81 COL 61 COLON-ALIGNED
     cTitle AT ROW 4.86 COL 61 COLON-ALIGNED
     cEmail AT ROW 5.91 COL 61 COLON-ALIGNED
     cWrkPhone AT ROW 6.95 COL 61 COLON-ALIGNED
     cCellPhone AT ROW 6.95 COL 90.8 COLON-ALIGNED
     fi-cPwd1 AT ROW 8 COL 61 COLON-ALIGNED
     fi-cPwd2 AT ROW 8 COL 90.8 COLON-ALIGNED
     iPwdInterval AT ROW 9 COL 61 COLON-ALIGNED HELP
          "Antall dager mellom tvungent skifte av passord"
     dLastPwdChange AT ROW 9 COL 90.8 COLON-ALIGNED
     bSuperUser AT ROW 10.19 COL 63.2
     bActive AT ROW 10.24 COL 93
     bProcessUser AT ROW 11.19 COL 92.8 WIDGET-ID 4
     bChangePwd AT ROW 11.24 COL 63.2 WIDGET-ID 6
     rectBrowse AT ROW 2.67 COL 2
     rectToolBar AT ROW 1.33 COL 20.8
     rectBrwCompanyUser AT ROW 13.38 COL 44.6
     RectBrowseSearch AT ROW 1.29 COL 2.4
     brwUserGroup AT ROW 13.38 COL 86
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 110.4 BY 19.48.

DEFINE FRAME frmSplitBarX
     btnSplitBarX AT ROW 1.05 COL 13.4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 29.8 ROW 2.67
         SIZE 16.2 BY 17.62.


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
         TITLE              = "Vedlikehold brukere"
         HEIGHT             = 19.48
         WIDTH              = 110.4
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
/* REPARENT FRAME */
ASSIGN FRAME frmSplitBarX:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
ASSIGN 
       cJBoxUserId:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FRAME frmSplitBarX
                                                                        */
ASSIGN 
       btnSplitBarX:MOVABLE IN FRAME frmSplitBarX          = TRUE.

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

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME frmSplitBarX
/* Query rebuild information for FRAME frmSplitBarX
     _Query            is NOT OPENED
*/  /* FRAME frmSplitBarX */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Vedlikehold brukere */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Vedlikehold brukere */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Vedlikehold brukere */
DO:
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").
  DEF VAR hColumn AS HANDLE NO-UNDO.
  hColumn = hBrowseCompany:GET-BROWSE-COLUMN(1).
  APPLY "end-resize" TO hColumn.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bSuperUser
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bSuperUser C-Win
ON VALUE-CHANGED OF bSuperUser IN FRAME DEFAULT-FRAME /* Superbruker */
DO:
  ASSIGN bSuperUser.
  IF bSuperUser THEN
    HIDE {&List-1}.
  ELSE VIEW {&List-1}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmSplitBarX
&Scoped-define SELF-NAME btnSplitBarX
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSplitBarX C-Win
ON END-MOVE OF btnSplitBarX IN FRAME frmSplitBarX /* Button 1 */
DO:
  DYNAMIC-FUNCTION("setSplitBarX",THIS-PROCEDURE:CURRENT-WINDOW,btnSplitBarX:HANDLE IN FRAME frmSplitBarX,NO).
  DEF VAR hColumn AS HANDLE NO-UNDO.
  hColumn = hBrowseCompany:GET-BROWSE-COLUMN(1).
  APPLY "end-resize" TO hColumn.
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
  IF NOT DYNAMIC-FUNCTION("DoCleanUpObjects",THIS-PROCEDURE:CURRENT-WINDOW) THEN RETURN NO-APPLY.

  DYNAMIC-FUNCTION("setCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
  PUBLISH "InvalidateHandle" (THIS-PROCEDURE).
  RUN disable_UI.
END.

{incl/wintrigg.i}

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:


  iFontWingdings = DYNAMIC-FUNCTION("setAppFont","Wingdings, size=11 Script=symbol","").
  &IF DEFINED(UIB_is_Running) NE 0 &THEN
    RUN InitializeObject.
    RUN MoveToTop.
  &ENDIF

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AnyPrintableKey C-Win 
PROCEDURE AnyPrintableKey :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN SUPER.

DO WITH FRAME {&FRAME-NAME}:
  IF DYNAMIC-FUNCTION("getCurrentWidget") = fi-cPwd1:HANDLE THEN
    RUN PostPWChar(fi-cPwd1:HWND).
  IF DYNAMIC-FUNCTION("getCurrentWidget") = fi-cPwd2:HANDLE THEN
    RUN PostPWChar(fi-cPwd2:HWND).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplayRecord C-Win 
PROCEDURE DisplayRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN SUPER.

IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowse THEN DO:

  ASSIGN cJBoxUserId:READ-ONLY IN FRAME {&FRAME-NAME} = TRUE
         fi-cPwd1:SCREEN-VALUE = "*****************"
         fi-cPwd2:SCREEN-VALUE = "*****************"
         .
  
  IF NUM-ENTRIES(cDbList) > 1 THEN
    DO ix = 1 TO NUM-ENTRIES(cDbList):
      hDbToggle[ix]:SCREEN-VALUE = STRING(hFieldMap:AVAIL AND hFieldMap:BUFFER-FIELD(ENTRY(ix,cDbList)):BUFFER-VALUE).
    END.
  
  DYNAMIC-FUNCTION("CheckModified",FRAME {&FRAME-NAME}:HANDLE,"clear").

  IF VALID-HANDLE(hUserActivity) THEN
    DYNAMIC-FUNCTION("setJBoxUserId" IN hUserActivity,
                     IF hFieldMap:AVAIL THEN hFieldMap:BUFFER-FIELD("cJBoxUserId"):BUFFER-VALUE ELSE "").
END.
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
  DISPLAY cJBoxUserId cUserName cTitle cEmail cWrkPhone cCellPhone fi-cPwd1 
          fi-cPwd2 iPwdInterval dLastPwdChange bSuperUser bActive bProcessUser 
          bChangePwd 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE rectBrowse rectToolBar rectBrwCompanyUser RectBrowseSearch 
         brwUserGroup cJBoxUserId cUserName cTitle cEmail cWrkPhone cCellPhone 
         fi-cPwd1 fi-cPwd2 iPwdInterval dLastPwdChange bSuperUser bActive 
         bProcessUser bChangePwd 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  ENABLE btnSplitBarX 
      WITH FRAME frmSplitBarX IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frmSplitBarX}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExtraDeleteRecord C-Win 
PROCEDURE ExtraDeleteRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF OUTPUT PARAM obOk AS LOG NO-UNDO INIT YES.


DO ix = 1 TO NUM-ENTRIES(cDbList):
  obOk = DYNAMIC-FUNCTION("DoDelete","_user","",
                   "_userid",
                   hFieldMap:BUFFER-FIELD("cJBoxUserId"):BUFFER-VALUE,
                   ix = NUM-ENTRIES(cDbList)). /* Commit on last database */
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExtraSaveRecord C-Win 
PROCEDURE ExtraSaveRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT  PARAM icTBstate AS CHAR NO-UNDO. /* Toolbar status */
DEF OUTPUT PARAM obOk      AS LOG NO-UNDO INIT TRUE.

DEF VAR cEncoPass         AS CHAR NO-UNDO.
DEF VAR bModDbAccess      AS LOG  NO-UNDO.
DEF VAR cServerTransParam AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  IF NUM-ENTRIES(cDbList) > 1 THEN
    DO ix = 1 TO NUM-ENTRIES(cDbList):
      IF hDbToggle[ix]:MODIFIED THEN bModDbAccess = YES.
    END.

  IF fi-cPwd1:MODIFIED OR fi-cPwd2:MODIFIED OR icTBstate = "new" THEN DO:
    ASSIGN fi-cPwd1 fi-cPwd2.
    IF fi-cPwd1 = "" THEN DO:
      MESSAGE IF bEnglish THEN "Password must be assigned" ELSE "Passord må oppgis"
              VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO fi-cPwd1.
      obOk = FALSE.
      RETURN.
    END.
    IF fi-cPwd1 NE fi-cPwd2 THEN DO:
      MESSAGE IF bEnglish THEN "Invalid password confirmation" ELSE "Feil i bekreftelse av passord"
              VIEW-AS ALERT-BOX ERROR.
      obOk = FALSE.
      APPLY "entry" TO fi-cPwd1.
      RETURN.
    END.
    ELSE DO:
      cEncoPass = ENCODE(fi-cPwd1).

      DO ix = 1 TO NUM-ENTRIES(cDbList):
        DYNAMIC-FUNCTION("DoDelete",ENTRY(ix,cDbList) + "._User","Avail",
              "_UserId",
              cJBoxUserId:SCREEN-VALUE,
              FALSE).
        IF (NOT VALID-HANDLE(hDbToggle[ix]) AND ix = 1) OR (VALID-HANDLE(hDbToggle[ix]) AND hDbToggle[ix]:CHECKED) THEN
          DYNAMIC-FUNCTION("DoCreate",ENTRY(ix,cDbList) + "._User","",
                "_UserId,_password",
                cJBoxUserId:SCREEN-VALUE + "|" + cEncoPass,
                FALSE).
      END.
    END.
  END.
  ELSE IF bModDbAccess THEN DO:
    DO ix = 1 TO NUM-ENTRIES(cDbList):
      cServerTransParam = cServerTransParam + (IF cServerTransParam NE "" THEN "," ELSE "") + ENTRY(ix,cDbList) + ";" + hDbToggle[ix]:SCREEN-VALUE.
    END.
    DYNAMIC-FUNCTION("setServerTransInputParam",cServerTransParam).
  END.
END.

obOk = DYNAMIC-FUNCTION("DoCommit",TRUE).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FlatViewRecord C-Win 
PROCEDURE FlatViewRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hFlatView AS HANDLE NO-UNDO.
DEF VAR hFlatBrw  AS HANDLE NO-UNDO.

DEF VAR cFollowLink AS CHAR NO-UNDO.
DEF VAR iReturn     AS INT NO-UNDO.


IF bCompUserTbl AND bUserGroupTbl THEN DO:
    
  RUN JBoxDSimpleSelectList.w (IF bEnglish THEN
                                 "Show company links|Company|Show user group links|Groups"
                               ELSE
                                 "Vis firma-knytninger|Company|Vis brukergruppe-knytninger|Groups"
                              ,?,OUTPUT cFollowLink).
  IF cFollowLink = "" THEN RETURN.
  
  IF cFollowLink = "Company" THEN 
    DYNAMIC-FUNCTION("DeleteObjectLink",hBrowse,hBrowseUserGrp).
  ELSE
    DYNAMIC-FUNCTION("DeleteObjectLink",hBrowse,hBrowseCompany).

  iReturn = DYNAMIC-FUNCTION("DoMessage",0,3,
                             IF bEnglish THEN
                               "Show all users (No: Only users where links exist)"
                             ELSE
                               "Vis alle brukere (Nei: Bare brukere med knytning)","","").
  IF iReturn = 2 THEN RETURN.
  ELSE DYNAMIC-FUNCTION("setAttribute",hBrowse,"flatViewJoinType",
                        IF iReturn = 6 THEN "OUTER-JOIN" ELSE "").

  RUN SUPER.
  
  IF cFollowLink = "Company" THEN 
    DYNAMIC-FUNCTION("CreateParentLink",hBrowseUserGrp,hBrowse,"cJBoxUserId").
  ELSE
    DYNAMIC-FUNCTION("CreateParentLink",hBrowseCompany,hBrowse,"cJBoxUserId").
  
    
  hFlatView = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hToolbar,"flatviewhandle")) NO-ERROR.
  IF NOT VALID-HANDLE(hFlatView) THEN RETURN.
  
  /* SUBSCRIBE PROCEDURE THIS-PROCEDURE TO "FlatViewDblClick" IN hFlatView. */
  
  hFlatBrw  = DYNAMIC-FUNCTION("getBrowseHandle" IN hFlatView).
  {incl/dynfilterlookups.i hFlatBrw}
  
END.
ELSE RUN SUPER.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeObject C-Win 
PROCEDURE InitializeObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR bActiveField    AS LOG  NO-UNDO.
DEF VAR bTitleField     AS LOG  NO-UNDO.
DEF VAR bPwdIntFields   AS LOG  NO-UNDO.
DEF VAR cDbAccessTbHdl  AS CHAR NO-UNDO.
DEF VAR cDbAccessFldDef AS CHAR NO-UNDO.
DEF VAR bProcessUserFld AS LOG  NO-UNDO.
DEF VAR bChgPwdFld      AS LOG  NO-UNDO.
  
RUN enable_UI.
 
DYNAMIC-FUNCTION("CreateStatusBar",THIS-PROCEDURE:CURRENT-WINDOW,"",0,YES,?) NO-ERROR.
                                                                                     
DO WITH FRAME {&FRAME-NAME}:

  bActiveField    = DYNAMIC-FUNCTION("IsFieldNameInTable","JBoxUser","bActive").
  bTitleField     = DYNAMIC-FUNCTION("IsFieldNameInTable","JBoxUser","cTitle").
  bPwdIntFields   = DYNAMIC-FUNCTION("IsFieldNameInTable","JBoxUser","iPwdInterval").
  bProcessUserFld = DYNAMIC-FUNCTION("IsFieldNameInTable","JBoxUser","bProcessUser").
  bChgPwdFld      = DYNAMIC-FUNCTION("IsFieldNameInTable","JBoxUser","bChangePwd").
  bUserGroupTbl   = DYNAMIC-FUNCTION("getFieldValues","FIRST JBoxUserGroup","WHERE true","iJBoxUserGroupId") NE ?.
  bCompUserTbl    = DYNAMIC-FUNCTION("getFieldValues","FIRST JBoxCompany","WHERE true","iJBoxCompanyId") NE ?.
  bEnglish        = NOT DYNAMIC-FUNCTION("Scandinavian").
  

  IF DYNAMIC-FUNCTION("runProc","jbadmin_getdblist.p","parameter_string",?) THEN DO:
    cDbList = DYNAMIC-FUNCTION("getTransactionMessage").
    IF NUM-ENTRIES(cDbList) > 1 THEN
      DO ix = 1 TO NUM-ENTRIES(cDbList):

        CREATE TOGGLE-BOX hDbToggle[ix] 
          ASSIGN FRAME            = FRAME {&FRAME-NAME}:HANDLE
                 LABEL            = ENTRY(ix,cDbList)
                 NAME             = ENTRY(ix,cDbList)
                 X                = bSuperUser:X
                 Y                = bSuperUser:Y + 22 * ix
                 WIDTH-PIXELS     = 100
                 HEIGHT-PIXELS    = 21
                 VISIBLE          = YES
                 SENSITIVE        = TRUE
                 TAB-STOP         = YES
                 .

        ASSIGN rectBrwCompanyUser:HEIGHT-PIXELS = rectBrwCompanyUser:HEIGHT-PIXELS - 21
               rectBrwCompanyUser:Y             = rectBrwCompanyUser:Y + 21
               brwUserGroup:HEIGHT-PIXELS       = brwUserGroup:HEIGHT-PIXELS - 21
               brwUserGroup:Y                   = brwUserGroup:Y + 21
               cDbAccessFldDef                  = cDbAccessFldDef + ";"
                                                + "+" + ENTRY(ix,cDbList) + "|LOGICAL|yes/no|DbAccess(ROWID" + ENTRY(ix,cDbList) + ")|" + ENTRY(ix,cDbList)
               cDbAccessTbHdl                   = cDbAccessTbHdl + "," + STRING(hDbToggle[ix])
                                                .
      END.
  END.

  IF NOT bActiveField THEN
    bActive:HIDDEN = YES.
  IF NOT bTitleField THEN
    cTitle:HIDDEN = YES.
  IF NOT bPwdIntFields THEN
    ASSIGN iPwdInterval:HIDDEN = YES
           dLastPwdChange:HIDDEN = YES.
  IF NOT bProcessUserFld THEN
    bProcessUser:HIDDEN = YES.
  IF NOT bChgPwdFld THEN
    bChangePwd:HIDDEN = YES.
  ELSE IF bEnglish THEN bChangePwd:TOOLTIP = "Password must be changed on next login".  

  hBrowse = DYNAMIC-FUNCTION("NewBrowse",          /* Create a browse object */
                    rectBrowse:HANDLE,              /* Rectangle to define coordinates for browse */
                    100,                            /* Rows to batch */
                    "",                             /* Browse properties, ie MULTIPLE,NUM-LOCKED-COLUMNS, etc */
                    "JBoxUser"                   /* Buffers and fields: <buffer1>;<field1>;<field2>..,<buffer2>;.. No fields gives all*/
                    + ";cJBoxUserId"
                    + ";cUserName"
                    + ";bSuperUser" + (IF bEnglish THEN "|Super user" ELSE "")
                    + cDbAccessFldDef
                    + ";cWrkPhone" + (IF NOT bEnglish THEN "|Tlf" ELSE "")
                    + ";cCellPhone" + (IF NOT bEnglish THEN "|Mobil" ELSE "")
                    + ";cEmail" + (IF NOT bEnglish THEN "|Epost" ELSE "")
                    + (IF bTitleField THEN ";cTitle" + (IF bEnglish THEN "|Title" ELSE "") ELSE "")
                    + (IF bActiveField THEN ";bActive" + (IF bEnglish THEN "|Active" ELSE "") ELSE "")
                    + (IF bPwdIntFields THEN ";iPwdInterval" + (IF bEnglish THEN "|Pwd.interval" ELSE "") + ";dLastPwdChange" + (IF bEnglish THEN "|Last chng pwd" ELSE "") ELSE "")
                    + (IF bProcessUserFld THEN ";bProcessUser" + (IF bEnglish THEN "|Process-user" ELSE "") ELSE "")
                    + (IF bChgPwdFld THEN ";bChangePwd" + (IF NOT bEnglish THEN "|Passord må byttes" ELSE "") ELSE "")
                    + ";dCreated"
                    + ";cCreatedBy"
                    + ";dModified"
                    + ";cModifiedBy"
                    ,"WHERE false", 
                    "sort|cJBoxUserId").                            /* Misc - for something I might need in next version.. */
  hBrowse:NAME = "brwUser". /* This name is neccessary because the browser is due to special treatment during resize */
  hBrowse:GET-BROWSE-COLUMN(1):WIDTH-PIXELS = 60.
  hBrwColSuperUser = hBrowse:GET-BROWSE-COLUMN(3).
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcFieldProc","jbadmin_jboxuser_browsecalc.p").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"flatViewJoinType","OUTER-JOIN").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"getRecordCount","yes").

  hSearchField = DYNAMIC-FUNCTION("NewBrowseSearchField",RectBrowseSearch:HANDLE,hBrowse,1).
  DYNAMIC-FUNCTION("createObjectLink",hBrowse,hSearchField).

  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap",      /* A fieldmap object holds extra info for display and input fields (fill-ins) 
                                                       and their corresponding buffer columns return handle equals the buffer handle */
                    hBrowse:QUERY,
                    FRAME {&FRAME-NAME}:HANDLE,     /* Frame for the input/display fields (might not be the same frame as the browse) */
                    "cJBoxUserId,cUserName,bSuperUser,cWrkPhone,cCellPhone,cEmail" 
                  + (IF bTitleField THEN ",cTitle" ELSE "")
                  + (IF bActiveField THEN ",bActive" ELSE "")
                  + (IF bPwdIntFields THEN ",iPwdInterval" ELSE "")
                  + (IF bProcessUserFld THEN ",bProcessUser" ELSE "")
                  + (IF bChgPwdFld THEN ",bChangePwd" ELSE "")
                   ,"",
                    IF bPwdIntFields THEN "dLastPwdChange" ELSE "",
                    "",
                    "fi-cPwd1,fi-cPwd2"
                  + (IF NUM-ENTRIES(cDbList) > 1 THEN "," + cDbList ELSE "")
                    ).           /* input fields not in buffer */
  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hFieldMap).

  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"postUpdateProc","jbadmin_jboxuser_post_update.p").

  hToolbar = DYNAMIC-FUNCTION("NewToolBar",
                    rectToolBar:HANDLE,             /* Rectangle to define coordinates for toolbar */
                    "Fil" + (IF bEnglish THEN "e" ELSE ""),  
                    (IF bEnglish THEN "New,Edit,Undo,Delete,Save,Filter,Flatview,Excel"
                     ELSE "new;Ny,Edit;Endre,undo;Angre,delete;Slett,save;Lagre,filter,flatview,excel;Eksporter til E&xcel")
                  + (IF bCompUserTbl THEN ",UserCompany;" + (IF bEnglish THEN "Assign to Company" ELSE "Knytt til firma") ELSE "")
                  + (IF bUserGroupTbl THEN ",MemberOf;" + (IF bEnglish THEN "Member of" ELSE "Medlem av") ELSE "")
                  + ",UserSettings;" + (IF bEnglish THEN "Settings" ELSE "Innstillinger")
                  + ",UserActivity;" + (IF bEnglish THEN "Activity" ELSE "Aktivitet")
                   ,"maxborder").                    /* Misc - for something I might need in next version.. */

  IF bCompUserTbl THEN DO:

    DYNAMIC-FUNCTION("setAttribute",hBrowse,"extraFilterFields",
                     "JBoxCompany.cCompanyName|" + IF bEnglish THEN "Company name" ELSE "Firmanavn").
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"prescanQueryJBoxCompany","EACH JBoxCompanyUser OF JBoxCompany NO-LOCK,FIRST JBoxUser OF JBoxCompanyUser NO-LOCK").

    DYNAMIC-FUNCTION("setAttribute",hBrowse,"filterlookupfields_cCompanyName","JBoxCompany;cCompanyName").
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"filterlookupquery_cCompanyName","where true").
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"filterlookupreturnfield_cCompanyName","cCompanyName").

/*     IF NOT bUserGroupTbl THEN                                                                        */
/*       rectBrwCompanyUser:WIDTH-PIXELS = rectBrwCompanyUser:WIDTH-PIXELS + brwUserGroup:WIDTH-PIXELS. */
    hBrowseCompany = DYNAMIC-FUNCTION("NewBrowse",    /* Create a browse object */
                      rectBrwCompanyUser:HANDLE,      /* Rectangle to define coordinates for browse */
                      100,                            /* Rows to batch */
                      "",                             /* Browse properties, ie MULTIPLE,NUM-LOCKED-COLUMNS, etc */
                      "JBoxCompanyUser"                /* Buffers and fields: <buffer1>;<field1>;<field2>..,<buffer2>;.. No fields gives all*/
                      + ";bSuperUserCompany" + (IF bEnglish THEN "|Company admin" ELSE "")
                      + ";!cJBoxUserId"
                      + ",JBoxCompany"
                      + ";cCompanyName" + (IF NOT bEnglish THEN "|Firmanavn" ELSE ""),
                      "WHERE false, FIRST JBoxCompany OF JBoxCompanyUser", 
                      "").                             /* Misc - for something I might need in next version.. */
    hBrowseCompany:NAME = "brwCompanyUser".            /* Name is neccessary for resize settings */
    DYNAMIC-FUNCTION("CreateParentLink",hBrowseCompany,hBrowse,"cJBoxUserId").
  
    DYNAMIC-FUNCTION("setAttribute",hBrowseCompany,"viewRecordCount","no").

    hBrwColCompAdmin = hBrowseCompany:GET-BROWSE-COLUMN(1).
  
    hAdminOverlay = DYNAMIC-FUNCTION("NewBrowseToggle",
                      hBrowse,          
                      "bSuperUserCompany",     
                      "bSuperUserCompany",
                      "").                                         
    DYNAMIC-FUNCTION("CreateOverlayLink",hBrowseCompany,hAdminOverlay,"bSuperUserCompany").
    DYNAMIC-FUNCTION("setAttribute",hAdminOverlay,"visible","yes").
  END.
/*   ELSE hBrowseCompany:HIDDEN = YES. */

  IF bUserGroupTbl THEN DO:
    DYNAMIC-FUNCTION("appendAttribute",hBrowse,"extraFilterFields",
                     "JBoxUserGroup.cUserGroupName|" + (IF bEnglish THEN "User group" ELSE "Brukergruppe"),",").
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"prescanQueryJBoxUserGroup","EACH JBoxUserGroupMembers OF JBoxUserGroup NO-LOCK,FIRST JBoxUser OF JBoxUserGroupMembers NO-LOCK").

    DYNAMIC-FUNCTION("setAttribute",hBrowse,"filterlookupfields_cUserGroupName","JBoxUserGroup;cUserGroupName").
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"filterlookupquery_cUserGroupName","where true").
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"filterlookupreturnfield_cUserGroupName","cUserGroupName").

    hBrowseUserGrp = DYNAMIC-FUNCTION("NewBrowse"
                      ,brwUserGroup:HANDLE
                      ,100
                      ,""
                      ,"JBoxUserGroupMembers"
                       + ";!cJBoxUserId"
                     + ",JBoxUserGroup"
                       + ";cUserGroupName|" + (IF bEnglish THEN "Member of" ELSE "Medlem av")
                      ,"WHERE false,FIRST JBoxUserGroup NO-LOCK OF JBoxUserGroupMembers"
                      ,"").
    DYNAMIC-FUNCTION("CreateParentLink",hBrowseUserGrp,hBrowse,"cJBoxUserId").
    DYNAMIC-FUNCTION("setAttribute",hBrowseUserGrp,"viewRecordCount","no").
  END.

  DYNAMIC-FUNCTION("CreateObjectLink",hToolbar,hBrowse).
  DYNAMIC-FUNCTION("CreateObjectLink",hToolbar,hFieldMap).

  RUN InvokeMethod(hBrowse,"OpenQuery").

END.

DYNAMIC-FUNCTION("setSplitBarX",THIS-PROCEDURE:CURRENT-WINDOW,btnSplitBarX:HANDLE IN FRAME frmSplitBarX,NO).
DYNAMIC-FUNCTION("setFollowSplitBarX",THIS-PROCEDURE:CURRENT-WINDOW,btnSplitBarX:HANDLE IN FRAME frmSplitBarX,
                  STRING(hBrowse) + "," +
                  STRING(rectBrowse:HANDLE) + "," +
                  STRING(brwUserGroup:HANDLE) + "," +
                  (IF VALID-HANDLE(hBrowseCompany) THEN STRING(hBrowseCompany) + "," ELSE "") +
                  STRING(rectBrwCompanyUser:HANDLE) + "," +
                  STRING(hAdminOverlay) + "," +
                  (IF VALID-HANDLE(hBrowseUserGrp) THEN STRING(hBrowseUserGrp) + "," ELSE "") +

                  STRING(cWrkPhone:HANDLE) + "," +
                  STRING(cEmail:HANDLE) + "," +
                  STRING(cJBoxUserId:HANDLE) + "," +
                  STRING(cUserName:HANDLE) + "," +
                  STRING(fi-cPwd1:HANDLE) + "," +
                  STRING(bSuperUser:HANDLE) + "," +
                  STRING(cTitle:HANDLE) + "," + 
                  STRING(iPwdInterval:HANDLE) + "," +
                  STRING(bChangePwd:HANDLE) + 
                  cDbAccessTbHdl
                  ).

DYNAMIC-FUNCTION("setNoMoveX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,"brwUserGroup").
DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, 
                 hBrowse:NAME + ",rectBrowse,rectBrwCompanyUser" 
              + (IF VALID-HANDLE(hBrowseCompany) THEN "," + hBrowseCompany:NAME ELSE "")).
DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectToolBar").
DYNAMIC-FUNCTION("setAddResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "cJBoxUserId,cUserName,cEmail,cTitle").
DYNAMIC-FUNCTION("setAddMoveX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,
                 "cCellPhone,dLastPwdChange,fi-cPwd2,bActive,bProcessUser").
DYNAMIC-FUNCTION("setOrgWinSize", THIS-PROCEDURE:CURRENT-WINDOW,500,400,0,250).

LocalTranslation().
DYNAMIC-FUNCTION("initTranslation",THIS-PROCEDURE:CURRENT-WINDOW).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MemberOfRecord C-Win 
PROCEDURE MemberOfRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cGroupRowIdList AS CHAR NO-UNDO.
DEF VAR cGroupIdList    AS CHAR NO-UNDO.

THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.

cGroupRowIdList = DYNAMIC-FUNCTION("getRowIdList","JBoxUserGroupMembers,JBoxUserGroup","",
                                   "WHERE cJBoxUserId = '" + hFieldMap:BUFFER-FIELD("cJBoxUserId"):BUFFER-VALUE 
                                 + "',FIRST JBoxUserGroup OF JBoxUserGroupMembers NO-LOCK"). 
RUN JBoxSelector.w (THIS-PROCEDURE,0,
                    "JBoxUserGroup;cUserGroupName|Gruppe;!iJBoxUserGroupId",
                    "where true",
                    INPUT-OUTPUT cGroupRowIdList,
                    "iJBoxUserGroupId",
                    INPUT-OUTPUT cGroupIdList,
                    "","",
                    OUTPUT bOK).
THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

IF bOk THEN DO:
  IF NOT DYNAMIC-FUNCTION("runproc","jbadmin_edituser_to_groups.p",
                          hFieldMap:BUFFER-FIELD("cJBoxUserId"):BUFFER-VALUE + "|" + cGroupIdList,?) THEN
    DYNAMIC-FUNCTION("DoMessage",0,1,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil","").
  ELSE APPLY "value-changed" TO hBrowse.
END.
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
THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.
{&WINDOW-NAME}:WINDOW-STATE = 3.
{&WINDOW-NAME}:MOVE-TO-TOP().
APPLY "end-move" TO btnSplitBarX IN FRAME frmSplitBarX.

APPLY "entry" TO hBrowse.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NewRecord C-Win 
PROCEDURE NewRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DYNAMIC-FUNCTION("setAttribute",hBrowseCompany,"basequery","where false").
DYNAMIC-FUNCTION("setCurrentObject",hBrowseCompany).
RUN OpenQuery.
DYNAMIC-FUNCTION("setCurrentObject",hToolbar).
DYNAMIC-FUNCTION("setAttribute",hToolbar,"disabledevents","UserCompany").
RUN SUPER.  
DYNAMIC-FUNCTION("setAttribute",hToolbar,"disabledevents","").
cJBoxUserId:READ-ONLY IN FRAME {&FRAME-NAME} = FALSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RowDisplayBrowse C-Win 
PROCEDURE RowDisplayBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowse THEN DO:
  hBrwColSuperUser:FONT = iFontWingdings.
  hBrwColSuperUser:FORMAT = CHR(254) + "/"  + CHR(168).
END.
ELSE IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowseCompany THEN DO:
  hBrwColCompAdmin:FONT = iFontWingdings.
  hBrwColCompAdmin:FORMAT = CHR(254) + "/"  + CHR(168).
END.
RUN SUPER.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SaveRecord C-Win 
PROCEDURE SaveRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF DYNAMIC-FUNCTION("getToolbarState",hToolbar) = "new" THEN
  DO WITH FRAME {&FRAME-NAME}:
    DO ix = 1 TO NUM-ENTRIES(cDbList):
      DYNAMIC-FUNCTION("DoDelete",ENTRY(ix,cDbList) + "._User","Avail",
            "_UserId",
            cJBoxUserId:SCREEN-VALUE,
            FALSE).
    END.
  END.

RUN SUPER.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartSearch C-Win 
PROCEDURE StartSearch :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN SUPER.
APPLY "window-resized" TO {&WINDOW-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UserActivityRecord C-Win 
PROCEDURE UserActivityRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
PUBLISH "StartChildWindow" ("JBoxLoginSession.w","",THIS-PROCEDURE,NO,OUTPUT hUserActivity).

DYNAMIC-FUNCTION("setJBoxUserId" IN hUserActivity,hFieldMap:BUFFER-FIELD("cJBoxUserId"):BUFFER-VALUE).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UserCompanyRecord C-Win 
PROCEDURE UserCompanyRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cCompanyRowIdList AS CHAR NO-UNDO.
DEF VAR cCompanyIdList    AS CHAR NO-UNDO.

THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
IF hBrowseCompany:QUERY:IS-OPEN THEN DO:
  hBrowseCompany:QUERY:GET-FIRST().
  REPEAT WHILE NOT hBrowseCompany:QUERY:QUERY-OFF-END:
    cCompanyRowIdList = cCompanyRowIdList + hBrowseCompany:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("RowIdent2"):BUFFER-VALUE + ",".
    hBrowseCompany:QUERY:GET-NEXT().
  END.
  cCompanyRowIdList = TRIM(cCompanyRowIdList,",").
END.
RUN JBoxSelector.w (THIS-PROCEDURE,0,
                    "JBoxCompany;iJBoxCompanyId;cCompanyName",
                    "where true",
                    INPUT-OUTPUT cCompanyRowIdList,
                    "iJBoxCompanyId",
                    INPUT-OUTPUT cCompanyIdList,
                    "","",
                    OUTPUT bOK).
THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

IF bOk THEN DO:
  IF NOT DYNAMIC-FUNCTION("runproc","jbserv_editcompanyuser.p",
                          hFieldMap:BUFFER-FIELD("cJBoxUserId"):BUFFER-VALUE + (IF cCompanyIdList NE "" THEN "|" + cCompanyIdList ELSE ""),?) THEN
    DYNAMIC-FUNCTION("DoMessage",0,1,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil","").
  ELSE APPLY "value-changed" TO hBrowse.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UserSettingsRecord C-Win 
PROCEDURE UserSettingsRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hUserSettings AS HANDLE NO-UNDO.

PUBLISH "StartChildWindow" ("JBoxUserSetting.w","",THIS-PROCEDURE,YES,OUTPUT hUserSettings).

DYNAMIC-FUNCTION("setJBoxUserId" IN hUserSettings,hFieldMap:BUFFER-FIELD("cJBoxUserId"):BUFFER-VALUE).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LocalTranslation C-Win 
FUNCTION LocalTranslation RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF NUM-ENTRIES(cDbList) = 1 THEN
  DYNAMIC-FUNCTION("BrwOrQryToFMapTranslation",hBrowse) NO-ERROR.
  
IF bEnglish THEN DO WITH FRAME {&FRAME-NAME}:
  ASSIGN fi-cPwd1:LABEL = "Password"
         fi-cPwd2:LABEL = "Confirm"
         .
END.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

