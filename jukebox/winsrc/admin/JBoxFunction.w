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
   
&SCOPED-DEFINE AdvGuiWin

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEF VAR bOK                   AS LOG    NO-UNDO.
DEF VAR ix                    AS INT    NO-UNDO.
DEF VAR hClientProg           AS HANDLE NO-UNDO.                          
DEF VAR hToolbar              AS HANDLE NO-UNDO.
DEF VAR hBrowse               AS HANDLE NO-UNDO.
DEF VAR hFieldMap             AS HANDLE NO-UNDO.
DEF VAR hBrowseFunctionAccess AS HANDLE NO-UNDO.
DEF VAR hBuffFuncAccess       AS HANDLE NO-UNDO.
DEF VAR hToolbarCompany       AS HANDLE NO-UNDO.
DEF VAR hAccessOverlay        AS HANDLE NO-UNDO.
DEF VAR hSearchField          AS HANDLE NO-UNDO.
DEF VAR hBrwColMenu           AS HANDLE NO-UNDO.
DEF VAR hBrwColCompAdmin      AS HANDLE NO-UNDO.
DEF VAR iFontWingdings        AS INT    NO-UNDO.
iFontWingdings = DYNAMIC-FUNCTION("setAppFont","Wingdings, size=11 Script=symbol","") NO-ERROR.

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
&Scoped-Define ENABLED-OBJECTS btnClientFileName BrwFunction rectToolBar ~
BrwAccess RectBrowseSearch bMenu cClientFileName btnServerFileName ~
btnObject cObjectName btnAction cAction cServerFileName cDescription ~
bAllowCompanyAdmins 
&Scoped-Define DISPLAYED-OBJECTS bMenu cClientFileName cObjectName cAction ~
cServerFileName cDescription bAllowCompanyAdmins 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setFieldState C-Win 
FUNCTION setFieldState RETURNS LOGICAL
  ( INPUT icAction AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnAction 
     LABEL "..." 
     SIZE 4 BY 1.14.

DEFINE BUTTON btnClientFileName 
     IMAGE-UP FILE "bmp/open16e.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Klient filnavn" 
     SIZE 4 BY 1.

DEFINE BUTTON btnObject 
     LABEL "..." 
     SIZE 4 BY 1.14.

DEFINE BUTTON btnServerFileName 
     IMAGE-UP FILE "bmp/open16e.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Server filnavn" 
     SIZE 4 BY 1.

DEFINE VARIABLE cAction AS CHARACTER FORMAT "x(256)" 
     LABEL "Aksjon" 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1.

DEFINE VARIABLE cClientFileName AS CHARACTER FORMAT "x(40)" 
     LABEL "Klient prog.navn" 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1.

DEFINE VARIABLE cDescription AS CHARACTER FORMAT "x(50)" 
     LABEL "Beskrivelse" 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1.

DEFINE VARIABLE cObjectName AS CHARACTER FORMAT "x(30)" 
     LABEL "Objekt" 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1.

DEFINE VARIABLE cServerFileName AS CHARACTER FORMAT "x(40)" 
     LABEL "Server prog.navn" 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1.

DEFINE RECTANGLE BrwAccess
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 53.8 BY 5.43.

DEFINE RECTANGLE BrwFunction
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 40 BY 13.33.

DEFINE RECTANGLE RectBrowseSearch
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 16.8 BY .95.

DEFINE RECTANGLE rectToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 28 BY .95.

DEFINE VARIABLE bAllowCompanyAdmins AS LOGICAL INITIAL no 
     LABEL "Tillatt for firma adm" 
     VIEW-AS TOGGLE-BOX
     SIZE 32 BY 1.

DEFINE VARIABLE bMenu AS LOGICAL INITIAL no 
     LABEL "Meny" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE BUTTON btnSplitBarX 
     IMAGE-UP FILE "bmp/tableft.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Button 1" 
     SIZE 2 BY 13.1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnClientFileName AT ROW 4.81 COL 93.8
     bMenu AT ROW 3.86 COL 65
     cClientFileName AT ROW 4.81 COL 62.8 COLON-ALIGNED
     btnServerFileName AT ROW 8.1 COL 93.8
     btnObject AT ROW 5.81 COL 93.8 NO-TAB-STOP 
     cObjectName AT ROW 5.91 COL 62.8 COLON-ALIGNED
     btnAction AT ROW 6.91 COL 93.8 NO-TAB-STOP 
     cAction AT ROW 7 COL 62.8 COLON-ALIGNED
     cServerFileName AT ROW 8.1 COL 62.8 COLON-ALIGNED
     cDescription AT ROW 9.19 COL 62.8 COLON-ALIGNED
     bAllowCompanyAdmins AT ROW 10.33 COL 64.8
     BrwFunction AT ROW 3.62 COL 2
     rectToolBar AT ROW 1.24 COL 1.8
     BrwAccess AT ROW 11.48 COL 45.2
     RectBrowseSearch AT ROW 2.48 COL 2.2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 99 BY 16.

DEFINE FRAME frmSplitBarX
     btnSplitBarX AT ROW 1.05 COL 13.4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 29.8 ROW 3.62
         SIZE 16.2 BY 13.24.


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
         TITLE              = "Vedlikehold funksjonstilgang"
         HEIGHT             = 16
         WIDTH              = 99.2
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
ON END-ERROR OF C-Win /* Vedlikehold funksjonstilgang */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Vedlikehold funksjonstilgang */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Vedlikehold funksjonstilgang */
DO:
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").
  DEF VAR hColumn AS HANDLE NO-UNDO.
  hColumn = hBrowseFunctionAccess:GET-BROWSE-COLUMN(1).
  APPLY "end-resize" TO hColumn.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAction
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAction C-Win
ON CHOOSE OF btnAction IN FRAME DEFAULT-FRAME /* ... */
DO:
  DEF VAR cLookupValue  AS CHAR NO-UNDO.
  DEF VAR cRowIdList    AS CHAR NO-UNDO.
  DEF VAR cReturnValues AS CHAR NO-UNDO.

  IF bMenu:CHECKED THEN DO:
    cLookupValue = "cMenuLabel".

    RUN JBoxDLookup.w ("JBoxMenu"
                     + ";cMenuLabel|Menu label"
                    ,"WHERE cMenuType = 'sub-menu'"
                    ,INPUT-OUTPUT cLookupValue).

    IF cLookupValue NE "" THEN DO:
      cAction:SCREEN-VALUE = cLookupValue.
      APPLY "any-printable" TO cAction.
    END.
  END.
  ELSE DO:
    THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
    RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "temp-table"
                      + ";cSourceProc|CHARACTER|x(30)||Program"
                      + ";cObjectName|CHARACTER|x(20)||Object"
                      + ";cAction|CHARACTER|x(20)||Action"
                      + ";cMethod|CHARACTER|x(25)||Method"
                      + ";cName|CHARACTER|x(20)||Event"
                      + ";cLabel|CHARACTER|x25)||Text"
                      + ";!hSourceProc|HANDLE"
                     ,"WHERE false"
                     ,INPUT-OUTPUT cRowIdList
                     ,"cAction"                                                  
                     ,INPUT-OUTPUT cReturnValues
                     ,"",""
                     ,OUTPUT bOK).
    THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

    IF bOk AND cReturnValues NE "" THEN DO:
      cAction:SCREEN-VALUE = REPLACE(cReturnValues,"|",",").
      APPLY "any-printable" TO cAction.
    END.
    THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnClientFileName
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnClientFileName C-Win
ON CHOOSE OF btnClientFileName IN FRAME DEFAULT-FRAME /* Klient filnavn */
DO:
  DEF VAR cFileName AS CHAR NO-UNDO.
  SYSTEM-DIALOG GET-FILE cFileName 
                FILTERS "Source files" "*.w" 
                MUST-EXIST
                UPDATE bOk.
  IF bOK THEN DO:  
    cClientFileName:SCREEN-VALUE = ENTRY(NUM-ENTRIES(cFileName,"\"),cFileName,"\").
    IF SUBSTR(cClientFileName:SCREEN-VALUE,LENGTH(cClientFileName:SCREEN-VALUE) - 1) = ".r" THEN
      cClientFileName:SCREEN-VALUE = SUBSTR(cClientFileName:SCREEN-VALUE,LENGTH(cClientFileName:SCREEN-VALUE) - 1) + ".w".
    APPLY "any-printable" TO cClientFileName.
  END.
  THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnObject
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnObject C-Win
ON CHOOSE OF btnObject IN FRAME DEFAULT-FRAME /* ... */
DO:
  DEF VAR cReturnValues AS CHAR NO-UNDO.

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxLookup.w (THIS-PROCEDURE,50,
                    "temp-table"
                    + ";cSourceProc|CHARACTER|x(30)||Program"
                    + ";cObjectName|CHARACTER|x(30)||Object"
                    + ";cObjectType|CHARACTER|x(30)||Object type"
                    + ";!hSourceProc|HANDLE"
                   ,"WHERE false"
                   ,""                                                  
                   ,"cSourceProc,cObjectname",
                    OUTPUT cReturnValues,
                    OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk AND cReturnValues NE "" THEN DO:
    ASSIGN cClientFileName:SCREEN-VALUE = ENTRY(1,cReturnValues,"|") 
           cObjectName:SCREEN-VALUE     = ENTRY(2,cReturnValues,"|")
           .
    APPLY "any-printable" TO cObjectName.
  END.
  THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnServerFileName
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnServerFileName C-Win
ON CHOOSE OF btnServerFileName IN FRAME DEFAULT-FRAME /* Server filnavn */
DO:
  DEF VAR cFileName AS CHAR NO-UNDO.
  SYSTEM-DIALOG GET-FILE cFileName 
                FILTERS "Source files" "*.p" 
                MUST-EXIST
                UPDATE bOk.
  IF bOK THEN DO:  
    cServerFileName:SCREEN-VALUE = ENTRY(NUM-ENTRIES(cFileName,"\"),cFileName,"\").
    IF SUBSTR(cServerFileName:SCREEN-VALUE,LENGTH(cServerFileName:SCREEN-VALUE) - 1) = ".r" THEN
      cServerFileName:SCREEN-VALUE = SUBSTR(cServerFileName:SCREEN-VALUE,LENGTH(cServerFileName:SCREEN-VALUE) - 1) + ".p".
    APPLY "any-printable" TO cServerFileName.
  END.
  THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
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
  hColumn = hBrowseFunctionAccess:GET-BROWSE-COLUMN(1).
  APPLY "end-resize" TO hColumn.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME cAction
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cAction C-Win
ON F3 OF cAction IN FRAME DEFAULT-FRAME /* Aksjon */
DO:
  APPLY "choose" TO btnAction.
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

  DYNAMIC-FUNCTION("setCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
  
  IF VALID-HANDLE(hClientProg) THEN
      APPLY "close" TO hClientProg.

  PUBLISH "InvalidateHandle" (THIS-PROCEDURE).
  RUN disable_UI.
END.

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
    IF DYNAMIC-FUNCTION("getAttribute",THIS-PROCEDURE,"advGuiWin") = "YES" THEN
      RUN WaitForForm NO-ERROR.
  &ENDIF

  IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
    &IF DEFINED(UIB_is_Running) = 0 &THEN
    RUN InitializeObject.
    RUN MoveToTop.
    APPLY "WINDOW-RESIZED" TO {&WINDOW-NAME}.
    SESSION:SET-WAIT-STATE("").
    IF DYNAMIC-FUNCTION("getAttribute",THIS-PROCEDURE,"advGuiWin") = "YES" THEN
      RUN WaitForForm NO-ERROR.
    ELSE 
    &ENDIF
      WAIT-FOR CLOSE OF THIS-PROCEDURE.
  END.  
END.

{incl/wintrigg.i}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplayRecord C-Win 
PROCEDURE DisplayRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowse THEN DO:
  hBuffFuncAccess:EMPTY-TEMP-TABLE().
  IF DYNAMIC-FUNCTION("runproc","jbadmin_getfunction_access.p",
                   IF hBrowse:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("iJBoxFunctionId"):BUFFER-VALUE)
                   ELSE "0"
                  ,hBuffFuncAccess:TABLE-HANDLE) THEN
    DYNAMIC-FUNCTION("getRunProcReturnTable",hBuffFuncAccess).

  IF NOT hFieldMap:AVAIL OR hFieldMap:AVAIL AND hFieldMap:BUFFER-FIELD("bMenu"):BUFFER-VALUE THEN
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"disabledevents","StartContainer").
  ELSE
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"disabledevents","").
END.

RUN SUPER.
setFieldState("Display").

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
  DISPLAY bMenu cClientFileName cObjectName cAction cServerFileName cDescription 
          bAllowCompanyAdmins 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btnClientFileName BrwFunction rectToolBar BrwAccess RectBrowseSearch 
         bMenu cClientFileName btnServerFileName btnObject cObjectName 
         btnAction cAction cServerFileName cDescription bAllowCompanyAdmins 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  ENABLE btnSplitBarX 
      WITH FRAME frmSplitBarX IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frmSplitBarX}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FunctionGroupAccessRecord C-Win 
PROCEDURE FunctionGroupAccessRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cGroupRowIdList AS CHAR NO-UNDO.
DEF VAR cGroupIdList    AS CHAR NO-UNDO.

THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.

cGroupRowIdList = DYNAMIC-FUNCTION("getRowIdList","JBoxFunctionAccess,JBoxUserGroup","",
                                   "WHERE iJBoxFunctionId = " + STRING(hFieldMap:BUFFER-FIELD("iJBoxFunctionId"):BUFFER-VALUE) + ",FIRST JBoxUserGroup OF JBoxFunctionAccess NO-LOCK"). 

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
  IF NOT DYNAMIC-FUNCTION("runproc","jbadmin_editfunction_to_groups.p",
                          STRING(hFieldMap:BUFFER-FIELD("iJBoxFunctionId"):BUFFER-VALUE) + ";" + cGroupIdList,?) THEN
    DYNAMIC-FUNCTION("DoMessage",0,1,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil","").
  ELSE APPLY "value-changed" TO hBrowse.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FunctionUserAccessRecord C-Win 
PROCEDURE FunctionUserAccessRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cUserRowIdList AS CHAR NO-UNDO.
DEF VAR cUserIdList    AS CHAR NO-UNDO.

THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.

cUserRowIdList = DYNAMIC-FUNCTION("getRowIdList","JBoxFunctionAccess,JBoxUser","",
                                   "WHERE iJBoxFunctionId = " + STRING(hFieldMap:BUFFER-FIELD("iJBoxFunctionId"):BUFFER-VALUE) + ",FIRST JBoxUser OF JBoxFunctionAccess NO-LOCK"). 

RUN JBoxSelector.w (THIS-PROCEDURE,0,
                    "JBoxUser;cJBoxUserId;cUserName|Navn",
                    "where true",
                    INPUT-OUTPUT cUserRowIdList,
                    "cJBoxUserId",
                    INPUT-OUTPUT cUserIdList,
                    "","",
                    OUTPUT bOK).

THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

IF bOk THEN DO:
  IF NOT DYNAMIC-FUNCTION("runproc","jbadmin_editfunction_to_users.p",
                          STRING(hFieldMap:BUFFER-FIELD("iJBoxFunctionId"):BUFFER-VALUE) + ";" + cUserIdList,?) THEN
    DYNAMIC-FUNCTION("DoMessage",0,1,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil","").
  ELSE APPLY "value-changed" TO hBrowse.
END.

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
DEF VAR bAllowCompAdminInstalled AS LOG NO-UNDO.
DEF VAR bScand                   AS LOG NO-UNDO.

RUN enable_UI.

DYNAMIC-FUNCTION("CreateStatusBar",THIS-PROCEDURE:CURRENT-WINDOW,"",0,YES,?) NO-ERROR.

DO WITH FRAME {&FRAME-NAME}:

  ASSIGN bAllowCompAdminInstalled = DYNAMIC-FUNCTION("isFieldNameInTable","JBoxFunction","bAllowCompanyAdmins")
         bScand = DYNAMIC-FUNCTION("Scandinavian")
         .

  IF NOT bScand THEN
    ASSIGN bMenu:LABEL               = "Menu"
           cClientFileName:LABEL     = "Client prog.name"
           cObjectName:LABEL         = "Object"
           cAction:LABEL             = "Action"
           cServerFileName:LABEL     = "Server prog.name"
           cDescription:LABEL        = "Description"
           bAllowCompanyAdmins:LABEL = "Allowed for company admins"
           .

  IF NOT bAllowCompAdminInstalled THEN
    bAllowCompanyAdmins:HIDDEN = YES.

  hBrowse = DYNAMIC-FUNCTION("NewBrowse",   
                    BrwFunction:HANDLE,     
                    100,                   
                    "",                    
                    "JBoxFunction"         
                    + ";bMenu" + (IF NOT bScand THEN "|Menu" ELSE "")
                    + ";cAction" + (IF NOT bScand THEN "|Action" ELSE "") 
                    + ";cObjectName" + (IF NOT bScand THEN "|Object" ELSE "")
                    + ";cClientFileName" + (IF NOT bScand THEN "|Client prog.name" ELSE "")
                    + ";cServerFileName" + (IF NOT bScand THEN "|Server prog.name" ELSE "")
                    + ";cDescription" + (IF NOT bScand THEN "|Description" ELSE "")
                    + (IF bAllowCompAdminInstalled THEN 
                        ";bAllowCompanyAdmins" + (IF NOT bScand THEN "|Allowed comp.admins" else "")
                       ELSE "")
                    + ";dCreated" + (IF NOT bScand THEN "|Created" ELSE "")
                    + ";cCreatedBy" + (IF NOT bScand THEN "|By" ELSE "")
                    + ";dModified" + (IF NOT bScand THEN "|Modified" ELSE "")
                    + ";cModifiedBy" + (IF NOT bScand THEN "|By" ELSE "")
                    + ";!iJBoxFunctionId"
                    ,"WHERE false", 
                    "").                         
  hBrwColMenu = hBrowse:GET-BROWSE-COLUMN(1).

  hSearchField = DYNAMIC-FUNCTION("NewBrowseSearchField",RectBrowseSearch:HANDLE,hBrowse,1).
  DYNAMIC-FUNCTION("createObjectLink",hBrowse,hSearchField).

  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap",
                    hBrowse:QUERY,
                    FRAME {&FRAME-NAME}:HANDLE,
                    "cClientFileName,bMenu,cAction,cObjectName,cDescription,cServerFileName"
                  + (IF bAllowCompAdminInstalled THEN ",bAllowCompanyAdmins" ELSE "")
                   ,"",
                    "","",                      
                    "btnClientFileName,btnAction,btnServerFileName,btnObject"). 
  
  hToolbar = DYNAMIC-FUNCTION("NewToolBar",
                    rectToolBar:HANDLE,
                    IF bScand THEN "Fil" ELSE "File",
                    "new,undo,delete,save,filter,excel"
                  + ",StartContainer;Start program"
                  + ",FunctionGroupAccess;" + (IF bScand THEN "Gruppe-tilgang" ELSE "Group access")
                  + ",FunctionUserAccess;" + (IF bScand THEN "Bruker-tilgang" ELSE "User access")
                   ,"maxborder").

  hBrowseFunctionAccess = DYNAMIC-FUNCTION("NewBrowse", 
                    BrwAccess:HANDLE,
                    100,
                    "",
                    "temp-table"
                    + ";cUserOrGroup|CHARACTER|x(40)||" + (IF NOT bScand THEN "Allowed for User/Group" ELSE "Tillatt for bruker/brukergr")
/*                     + ";cAccess|CHARACTER|x(12)||Rettighet"  */
                    + ";!iJBoxFunctionId|INTEGER|>>>>>>9"
                   ,"WHERE false"
                   ,"").

  ASSIGN hBrwColCompAdmin = hBrowseFunctionAccess:GET-BROWSE-COLUMN(1)
         hBuffFuncAccess  = hBrowseFunctionAccess:QUERY:GET-BUFFER-HANDLE(1).

  DYNAMIC-FUNCTION("CreateObjectLink",hToolbar,hBrowse).
  DYNAMIC-FUNCTION("CreateObjectLink",hToolbar,hFieldMap).
  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hFieldMap).
  DYNAMIC-FUNCTION("CreateParentLink",hBrowseFunctionAccess,hBrowse,"iJBoxFunctionId").

END.

DYNAMIC-FUNCTION("setSplitBarX",THIS-PROCEDURE:CURRENT-WINDOW,btnSplitBarX:HANDLE IN FRAME frmSplitBarX,NO).
DYNAMIC-FUNCTION("setFollowSplitBarX",THIS-PROCEDURE:CURRENT-WINDOW,btnSplitBarX:HANDLE IN FRAME frmSplitBarX,
                  STRING(hBrowse) + "," +
                  STRING(BrwFunction:HANDLE) + "," +
                  STRING(hBrowseFunctionAccess) + "," +
                  STRING(BrwAccess:HANDLE) + "," +

                  STRING(bMenu:HANDLE) + "," +
                  STRING(cClientFileName:HANDLE) + "," +
                  STRING(cAction:HANDLE) + "," +
                  STRING(cObjectName:HANDLE) + "," +
                  STRING(cDescription:HANDLE) + "," +
                  STRING(cServerFileName:HANDLE) + "," +
                  STRING(bAllowCompanyAdmins:HANDLE) 
                  ).

DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, hBrowse:NAME + ",brwFunction").
DYNAMIC-FUNCTION("setAddResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "cClientFileName,cAction,cServerFileName,cObjectName,cDescription").

DYNAMIC-FUNCTION("setOrgWinSize", THIS-PROCEDURE:CURRENT-WINDOW,350,250,0,0).

DYNAMIC-FUNCTION("initTranslation",THIS-PROCEDURE:CURRENT-WINDOW).

RUN InvokeMethod(hBrowse,"OpenQuery").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LeaveOfField C-Win 
PROCEDURE LeaveOfField :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icFieldName AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  CASE icFieldName:
    WHEN "cClientFileName" THEN setFieldState("").
  END CASE.
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
APPLY "end-move" TO btnSplitBarX IN FRAME frmSplitBarX.
{&WINDOW-NAME}:HIDDEN = NO.
{&WINDOW-NAME}:WINDOW-STATE = 3.
{&WINDOW-NAME}:MOVE-TO-TOP().
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
DEF VAR cType AS CHAR NO-UNDO.

IF NOT DYNAMIC-FUNCTION("getActionPermission",THIS-PROCEDURE:FILE-NAME,"","new") THEN RETURN.

DYNAMIC-FUNCTION("setAttribute",hBrowseFunctionAccess,"basequery","where false").
DYNAMIC-FUNCTION("setCurrentObject",hBrowseFunctionAccess).
RUN OpenQuery.
DYNAMIC-FUNCTION("setCurrentObject",hToolbar).
RUN SUPER.  

RUN JBoxDSimpleSelectList.w ("Function (action)|function|Menu|menu|Other (server-routine)|other",?,OUTPUT cType). 

IF cType = "function" THEN
  RUN StartContainerRecord.
ELSE IF cType = "menu" THEN DO WITH FRAME {&FRAME-NAME}:
  bMenu:CHECKED = YES.
  APPLY "value-changed" TO bMenu.
  APPLY "choose" TO btnAction.
END.

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
  hBrwColMenu:FONT = iFontWingdings.
  hBrwColMenu:FORMAT = CHR(254) + "/"  + CHR(168).
END.
RUN SUPER.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setLookupAttributes C-Win 
PROCEDURE setLookupAttributes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihBrowse AS HANDLE NO-UNDO.
DEF INPUT PARAM ihTBrect  AS HANDLE NO-UNDO.
DEF INPUT PARAM ihOther   AS HANDLE NO-UNDO.

DEF VAR httEvent    AS HANDLE NO-UNDO.
DEF VAR httObject   AS HANDLE NO-UNDO.
DEF VAR hqEvent     AS HANDLE NO-UNDO.
DEF VAR hbEvent     AS HANDLE NO-UNDO.
DEF VAR hbObject    AS HANDLE NO-UNDO.
DEF VAR hBuffer     AS HANDLE NO-UNDO.
DEF VAR hSourceProc AS HANDLE NO-UNDO.
         
hBuffer   = ihBrowse:QUERY:GET-BUFFER-HANDLE(1).

IF ihBrowse:QUERY:GET-BUFFER-HANDLE(1):NAME NE "JBoxMenu" THEN DO WITH FRAME {&FRAME-NAME}:

  ASSIGN httEvent  = WIDGET-HANDLE(DYNAMIC-FUNCTION("getObjectTableHandle","ttEvent"))
         httObject = WIDGET-HANDLE(DYNAMIC-FUNCTION("getObjectTableHandle","ttObject"))
         .
  
  IF NOT VALID-HANDLE(httEvent) OR NOT VALID-HANDLE(httObject) THEN RETURN.
  
  ASSIGN hbEvent  = httEvent:DEFAULT-BUFFER-HANDLE
         hbObject = httObject:DEFAULT-BUFFER-HANDLE.
         
/*  run toexcelviafile.p (hbObject,0).*/
  
  CREATE QUERY hqEvent.
  hqEvent:SET-BUFFERS(hbEvent,hbObject).
  
/*  hqEvent:QUERY-PREPARE("FOR EACH ttEvent WHERE ttEvent.cLabel NE '' AND cMethod NE ''"*/
  hqEvent:QUERY-PREPARE("FOR EACH ttEvent WHERE cMethod NE ''"
                      + ",FIRST ttObject OF ttEvent WHERE ttObject.hWindow NE WIDGET-HANDLE('" + STRING(THIS-PROCEDURE:CURRENT-WINDOW) + "')"
                      + " AND CAN-DO('toolbar,browse,popupMenu',cObjectType)"
                      + (IF cObjectName NE "" THEN
                          " AND cObjectName = '" + cObjectName:SCREEN-VALUE + "'"
                         ELSE "")
                        ).
  hqEvent:QUERY-OPEN().
  hqEvent:GET-FIRST().
  REPEAT WHILE NOT hqEvent:QUERY-OFF-END:
    bOk = hBuffer:FIND-FIRST("WHERE cObjectName = '" + hbObject:BUFFER-FIELD("cObjectName"):BUFFER-VALUE + "'" 
                           + "  AND hSourceProc = WIDGET-HANDLE('" + STRING(hbObject:BUFFER-FIELD("hSourceProc"):BUFFER-VALUE) + "')"
                             ) NO-ERROR.
    IF NOT bOk THEN DO:
      hSourceProc = hbObject:BUFFER-FIELD("hSourceProc"):BUFFER-VALUE.
      IF VALID-HANDLE(hSourceProc) AND (IF cClientFileName:SCREEN-VALUE NE "" THEN hSourceProc:FILE-NAME = cClientFileName:SCREEN-VALUE ELSE TRUE) THEN DO:        
        hBuffer:BUFFER-CREATE().
        hBuffer:BUFFER-COPY(hbObject).
        hBuffer:BUFFER-COPY(hbEvent).
        hBuffer:BUFFER-FIELD("cSourceProc"):BUFFER-VALUE = hSourceProc:FILE-NAME.
      END.
    END.
  
    hqEvent:GET-NEXT().
  END.
   
   
  DYNAMIC-FUNCTION("setCurrentObject",ihBrowse).
    
  RUN OpenQuery.
END.

ELSE DO WITH FRAME {&FRAME-NAME}:
  IF cClientFileName:SCREEN-VALUE NE "" THEN DO:
    bOk = hBuffer:FIND-FIRST("WHERE cLaunch = '" + cClientFileName:SCREEN-VALUE + "'") NO-ERROR.
    IF bOk THEN
      ihBrowse:QUERY:REPOSITION-TO-ROWID(hBuffer:ROWID).
  END.
  ihBrowse:WINDOW:TITLE = "Velg program som inneholder funksjon".
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSelectorAttributes C-Win 
PROCEDURE setSelectorAttributes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihSourceBrw   AS HANDLE NO-UNDO.
DEF INPUT PARAM ihTargetBrw   AS HANDLE NO-UNDO.

DEF VAR httEvent      AS HANDLE NO-UNDO.
DEF VAR httObject     AS HANDLE NO-UNDO.
DEF VAR hqEvent       AS HANDLE NO-UNDO.
DEF VAR hbEvent       AS HANDLE NO-UNDO.
DEF VAR hbObject      AS HANDLE NO-UNDO.
DEF VAR hSrcBuffer    AS HANDLE NO-UNDO.
DEF VAR hTrgBuffer    AS HANDLE NO-UNDO.
DEF VAR hSourceProc   AS HANDLE NO-UNDO.
DEF VAR cSelectedRows AS CHAR   NO-UNDO.
DEF VAR ix            AS INT    NO-UNDO.

IF ihSourceBrw:QUERY:GET-BUFFER-HANDLE(1):NAME = "JBoxUser" THEN DO:
  ihSourceBrw:GET-BROWSE-COLUMN(1):WIDTH-PIXELS = 100. 
  ihTargetBrw:GET-BROWSE-COLUMN(1):WIDTH-PIXELS = 100. 
END.         
ELSE IF ihSourceBrw:QUERY:GET-BUFFER-HANDLE(1):NAME NE "JBoxUserGroup" THEN DO WITH FRAME {&FRAME-NAME}:

  ASSIGN httEvent   = WIDGET-HANDLE(DYNAMIC-FUNCTION("getObjectTableHandle","ttEvent"))
         httObject  = WIDGET-HANDLE(DYNAMIC-FUNCTION("getObjectTableHandle","ttObject"))
         hSrcBuffer = ihSourceBrw:QUERY:GET-BUFFER-HANDLE(1)
         hTrgBuffer = ihTargetBrw:QUERY:GET-BUFFER-HANDLE(1)
         ihSourceBrw:GET-BROWSE-COLUMN(1):WIDTH-PIXELS = 50
         ihSourceBrw:GET-BROWSE-COLUMN(2):WIDTH-PIXELS = 50
         ihTargetBrw:GET-BROWSE-COLUMN(1):WIDTH-PIXELS = 50
         ihTargetBrw:GET-BROWSE-COLUMN(2):WIDTH-PIXELS = 50
         .
  
  IF NOT VALID-HANDLE(httEvent) OR NOT VALID-HANDLE(httObject) THEN RETURN.
  
  ASSIGN hbEvent  = httEvent:DEFAULT-BUFFER-HANDLE
         hbObject = httObject:DEFAULT-BUFFER-HANDLE.
  
  CREATE QUERY hqEvent.
  hqEvent:SET-BUFFERS(hbEvent,hbObject).
  
  hqEvent:QUERY-PREPARE("FOR EACH ttEvent WHERE ttEvent.cMethod NE '' AND NOT CAN-DO('undo',ttEvent.cAction)"
                      + (IF VALID-HANDLE(hClientProg) THEN 
                          ",FIRST ttObject OF ttEvent WHERE ttObject.hWindow = WIDGET-HANDLE('" + STRING(hClientProg:CURRENT-WINDOW) + "')"
                         ELSE
                          ",FIRST ttObject OF ttEvent WHERE ttObject.hWindow NE WIDGET-HANDLE('" + STRING(THIS-PROCEDURE:CURRENT-WINDOW) + "')")
                      + (IF cObjectName:SCREEN-VALUE NE "" THEN 
                          " AND cObjectName = '" + cObjectName:SCREEN-VALUE + "'"
                         ELSE "")
                        ).
  hqEvent:QUERY-OPEN().
  hqEvent:GET-FIRST().
  REPEAT WHILE NOT hqEvent:QUERY-OFF-END:
    bOk = hSrcBuffer:FIND-FIRST("WHERE cObjectName = '" + hbObject:BUFFER-FIELD("cObjectName"):BUFFER-VALUE + "'" 
                               + " AND cAction = '" + hbEvent:BUFFER-FIELD("cAction"):BUFFER-VALUE + "'"
                               + "  AND hSourceProc = WIDGET-HANDLE('" + STRING(hbObject:BUFFER-FIELD("hSourceProc"):BUFFER-VALUE) + "')"
                               ) NO-ERROR.
    IF NOT bOk THEN DO:
      hSourceProc = hbObject:BUFFER-FIELD("hSourceProc"):BUFFER-VALUE.
      IF VALID-HANDLE(hSourceProc) THEN DO:        
        ix = ix + 1.
        hSrcBuffer:BUFFER-CREATE().
        hSrcBuffer:BUFFER-COPY(hbObject).
        hSrcBuffer:BUFFER-COPY(hbEvent).
        hSrcBuffer:BUFFER-FIELD("cSourceProc"):BUFFER-VALUE = hSourceProc:FILE-NAME.
        hSrcBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE = "rowid" + STRING(ix).

        IF CAN-DO(cAction:SCREEN-VALUE,hbEvent:BUFFER-FIELD("cAction"):BUFFER-VALUE) AND 
           (IF cObjectName:SCREEN-VALUE NE "" THEN
              CAN-DO(cObjectName:SCREEN-VALUE,hbObject:BUFFER-FIELD("cObjectName"):BUFFER-VALUE)
            ELSE TRUE) AND
           (IF cClientFileName:SCREEN-VALUE NE "" THEN
              CAN-DO(cClientFileName:SCREEN-VALUE,hSourceProc:FILE-NAME)
            ELSE TRUE)
           THEN DO:
          hTrgBuffer:BUFFER-CREATE().
          hTrgBuffer:BUFFER-COPY(hbObject).
          hTrgBuffer:BUFFER-COPY(hbEvent).
          hTrgBuffer:BUFFER-FIELD("cSourceProc"):BUFFER-VALUE = hSourceProc:FILE-NAME.
          hTrgBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE = "rowid" + STRING(ix).
          cSelectedRows = cSelectedRows + "rowid" + STRING(ix) + ",".
        END.
      END.
    END.
  
    hqEvent:GET-NEXT().
  END.
  
  DYNAMIC-FUNCTION("setSelectedRowids" IN SOURCE-PROCEDURE,TRIM(cSelectedRows,",")).

  DYNAMIC-FUNCTION("setCurrentObject",ihSourceBrw).
  RUN OpenQuerySource IN SOURCE-PROCEDURE.

  DYNAMIC-FUNCTION("setCurrentObject",ihTargetBrw).
  RUN OpenQueryTarget IN SOURCE-PROCEDURE.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSimpleSelectAttributes C-Win 
PROCEDURE setSimpleSelectAttributes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihSelecList AS HANDLE NO-UNDO.

ihSelecList:FRAME:TITLE = "Select access-control type".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartContainerRecord C-Win 
PROCEDURE StartContainerRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cLookupValue  AS CHAR NO-UNDO.
DEF VAR bNew          AS LOG  NO-UNDO.

IF cClientFileName:SCREEN-VALUE IN FRAME {&FRAME-NAME} NE "" THEN
  cLookupValue = cClientFileName:SCREEN-VALUE IN FRAME {&FRAME-NAME}.
ELSE DO:    
  ASSIGN cLookupValue = "cLaunch"
         bNew         = PROGRAM-NAME(2) BEGINS "NewRecord".
  
  RUN JBoxDLookup.w ("JBoxMenu"
                   + ";cMenuLabel|Tittel"
                   + ";cLaunch|Program"
                  ,"WHERE cMenuType = 'menu-item' AND cLaunchType = 'start-window'"
                  ,INPUT-OUTPUT cLookupValue).
END.

IF cLookupValue NE "" THEN DO:
  IF VALID-HANDLE(hClientProg) THEN APPLY "close" TO hClientProg.

  IF bNew THEN cClientFileName:SCREEN-VALUE IN FRAME {&FRAME-NAME} = cLookupValue.

  PUBLISH "StartChildWindow" (cLookupValue,"Sett tilgangskontroll for " + cLookupValue,THIS-PROCEDURE,YES,OUTPUT hClientProg).
/*  RUN VALUE(cLookupValue) PERSIST SET hClientProg.               */
/*  IF CAN-DO(hClientProg:INTERNAL-ENTRIES,"InitializeObject") THEN*/
/*    RUN InitializeObject IN hClientProg.                         */
END.

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
IF hBrowseFunctionAccess:QUERY:IS-OPEN THEN DO:
  hBrowseFunctionAccess:QUERY:GET-FIRST().
  REPEAT WHILE NOT hBrowseFunctionAccess:QUERY:QUERY-OFF-END:
    cCompanyRowIdList = cCompanyRowIdList + hBrowseFunctionAccess:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("RowIdent2"):BUFFER-VALUE + ",".
    hBrowseFunctionAccess:QUERY:GET-NEXT().
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
                          hFieldMap:BUFFER-FIELD("cJBoxUserId"):BUFFER-VALUE + "|" + cCompanyIdList,?) THEN
    DYNAMIC-FUNCTION("DoMessage",0,1,DYNAMIC-FUNCTION("getTransactionMessage"),"Feil","").
  ELSE APPLY "value-changed" TO hBrowse.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValueChangedField C-Win 
PROCEDURE ValueChangedField :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icFieldName AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  CASE icFieldName:
    WHEN "bMenu" THEN setFieldState("").
  END CASE.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setFieldState C-Win 
FUNCTION setFieldState RETURNS LOGICAL
  ( INPUT icAction AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR bSensitive  AS LOG NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  bSensitive = IF icAction = "Display" AND NOT hFieldMap:AVAIL THEN NO
               ELSE NOT bMenu:CHECKED.

  ASSIGN cClientFileName:SENSITIVE    = bSensitive
         btnClientFileName:SENSITIVE  = bSensitive
         cServerFileName:SENSITIVE    = bSensitive
         btnServerFileName:SENSITIVE  = bSensitive
         cObjectName:SENSITIVE        = bSensitive
         btnObject:SENSITIVE          = bSensitive
         .
END.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

