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

DEF VAR bOk    AS LOG NO-UNDO.
DEF VAR ix     AS INT NO-UNDO.
DEF VAR iDummy AS INT NO-UNDO.

DEF TEMP-TABLE ttWindow 
    FIELD cWinName     AS CHAR 
    FIELD cMenuTitle   AS CHAR 
    FIELD hWindow      AS HANDLE
    FIELD hWinMenuItem AS HANDLE
    .

DEF VAR hWinMenu AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setDisabledMenus C-Win 
FUNCTION setDisabledMenus RETURNS LOGICAL
  ( INPUT ihMenu     AS HANDLE,
    INPUT icDisabled AS CHAR   )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU m_Fil 
       MENU-ITEM m_Packages     LABEL "Packages"      
       MENU-ITEM m_applications LABEL "Applications"  
       MENU-ITEM m_All_files_and_packages LABEL "All files and packages"
       RULE
       MENU-ITEM m_Exit         LABEL "Exit"          .

DEFINE SUB-MENU m_System 
       MENU-ITEM m_Users        LABEL "Users"         
       MENU-ITEM m_Companies    LABEL "Companies"     
       MENU-ITEM m_Select_company LABEL "Select company".

DEFINE MENU MENU-BAR-C-Win MENUBAR
       SUB-MENU  m_Fil          LABEL "File"          
       SUB-MENU  m_System       LABEL "System"        .


/* Definitions of the field level widgets                               */

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80.4 BY 2.52.


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
         TITLE              = "Jukebox static menu template"
         HEIGHT             = 2.52
         WIDTH              = 80.4
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

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU MENU-BAR-C-Win:HANDLE.

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
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Jukebox static menu template */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Jukebox static menu template */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Jukebox static menu template */
DO:
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_All_files_and_packages
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_All_files_and_packages C-Win
ON CHOOSE OF MENU-ITEM m_All_files_and_packages /* All files and packages */
DO:
  RUN StartWindow ("JBoxDataBrw.w",SELF).
  ttWindow.hWindow:CURRENT-WINDOW:TITLE = "Files for packages".
  DYNAMIC-FUNCTION("setUseLocalData" IN ttWindow.hWindow,TRUE).
  RUN InitializeObject IN ttWindow.hWindow
      ("JBoxDocument"
       + ";cFileName|File name"
       + ";cFileType"
       + ";dFileCreateDate"
       + ";+cFileCreateTime|CHARACTER|x(5)|jbserv_int_to_hhmm_time.p(iFileCreateTime)|Time"
       + ";!iFileCreateTime"
       + ";dFileModDate"
       + ";+cFileModTime|CHARACTER|x(5)|jbserv_int_to_hhmm_time.p(iFileModTime)|Time"
       + ";!iFileModTime"
       + ";iDocSize|Size"
       + ";cFullPathName|Org.path"
       + ";dCreated|Loaded"
       + ";cCreatedBy|By"
     + ",JboxDocRel"
       + ";!iJBoxDocRelId"
       + ";!iJBoxDocumentId"
       + ";!cContext"
       + ";!cEntityId"
     + ",JBoxPackage"
       + ";cPackageName|Package name"
       + ";cVersion"
       + ";cRelativePath"
       + ";bComplete"
       + ";bDownLoad"
       + ";bProPath"
       + ";bRestart"
       + ";dCreated|Created"
       + ";cCreatedBy|By"
       + ";dModified|Modified"
       + ";cModifiedBy|By"
       + ";!iJBoxPackageId"
       ,
       "WHERE true "
     + ",EACH JBoxDocRel OF JBoxDocument NO-LOCK"
     + ",FIRST JBoxPackage WHERE iJBoxPackageId = INT(JBoxDocRel.cEntityId) NO-LOCK",
       "cFileName",
       TRUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_applications
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_applications C-Win
ON CHOOSE OF MENU-ITEM m_applications /* Applications */
DO:
  RUN StartWindow ("JBoxApplication.w",SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Companies
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Companies C-Win
ON CHOOSE OF MENU-ITEM m_Companies /* Companies */
DO:
  RUN StartWindow("JBoxCompany.w",SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Exit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Exit C-Win
ON CHOOSE OF MENU-ITEM m_Exit /* Exit */
DO:
  APPLY "close" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Packages
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Packages C-Win
ON CHOOSE OF MENU-ITEM m_Packages /* Packages */
DO:
  RUN StartWindow("JBoxPackage.w",SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Select_company
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Select_company C-Win
ON CHOOSE OF MENU-ITEM m_Select_company /* Select company */
DO:
  DEF VAR iDummy AS INT NO-UNDO.
  RUN JBoxDSelectCompany.w (TRUE, INPUT-OUTPUT iDummy).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Users
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Users C-Win
ON CHOOSE OF MENU-ITEM m_Users /* Users */
DO:
  RUN StartWindow("JBoxUser.w",SELF).
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
  DYNAMIC-FUNCTION("setCleanUpResize", THIS-PROCEDURE:CURRENT-WINDOW).
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

  RUN enable_UI.
  RUN InitWindow.
  THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BringToTop C-Win 
PROCEDURE BringToTop :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihWindow    AS HANDLE NO-UNDO.
DEF INPUT PARAM ihWinMenu   AS HANDLE NO-UNDO.
DEF INPUT PARAM ihMenuItem  AS HANDLE NO-UNDO.

DEF VAR hMenuItm AS HANDLE NO-UNDO.

FIND FIRST ttWindow 
     WHERE ttWindow.hWindow = ihWindow NO-ERROR.
IF AVAIL ttWindow THEN DO:
  ttWindow.hWindow:CURRENT-WINDOW:MOVE-TO-TOP().
  ttWindow.hWindow:CURRENT-WINDOW:WINDOW-STATE = 3.
  hMenuItm = ihWinMenu:FIRST-CHILD.
  REPEAT WHILE VALID-HANDLE(hMenuItm):
    hMenuItm:CHECKED = IF hMenuItm = ttWindow.hWinMenuItem THEN TRUE ELSE FALSE.
    hMenuItm = hMenuItm:NEXT-SIBLING.
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
  VIEW FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitWindow C-Win 
PROCEDURE InitWindow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cUserLevel AS CHAR NO-UNDO.
DEF VAR iCompanyId AS INT  NO-UNDO.

SUBSCRIBE TO "InvalidateHandle" ANYWHERE.
SUBSCRIBE TO "setSensitiveWin" ANYWHERE.

DYNAMIC-FUNCTION("setOrgWinSize", THIS-PROCEDURE:CURRENT-WINDOW,300,150,0,0).

DYNAMIC-FUNCTION("NewObject",SESSION,SESSION,"session").

iCompanyId = DYNAMIC-FUNCTION("getCompanyId").
IF iCompanyId = 0 THEN
  RUN JBoxDSelectCompany.w (TRUE,INPUT-OUTPUT iDummy).
ELSE 
  DYNAMIC-FUNCTION("setCompanyHeader",THIS-PROCEDURE:CURRENT-WINDOW).

IF LOGICAL(DYNAMIC-FUNCTION("getFieldValues","JBoxUser","WHERE cJBoxUserId = '" + DYNAMIC-FUNCTION("getASuserId") + "'","bSuperUser")) THEN
  DYNAMIC-FUNCTION("setAttribute",SESSION,"userlevel","super").
/* ELSE DO:                                                                                                                                                   */
/*   setDisabledMenus(THIS-PROCEDURE:CURRENT-WINDOW:MENUBAR:FIRST-CHILD,DYNAMIC-FUNCTION("getFieldList","genCode;cCodeValue","WHERE cCodeType = 'SU-menu'")). */
/*   cUserLevel = DYNAMIC-FUNCTION("getFieldValues","JBoxUser","WHERE cJBoxUserId = '" + DYNAMIC-FUNCTION("getASuserId") + "'","iSubjektId").                 */
/*   IF cUserLevel = "1" THEN                                                                                                                                 */
/*     DYNAMIC-FUNCTION("setSecDisabledActions","new,copy,delete,undo,save,kk","").                                                                           */
/* END.                                                                                                                                                       */
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
DEF INPUT PARAM ihWindow AS HANDLE NO-UNDO.

FIND FIRST ttWindow 
     WHERE ttWindow.hWindow = ihWindow NO-ERROR.
IF AVAIL ttWindow THEN DO:
  DELETE OBJECT ttWindow.hWinMenuItem.
  DELETE ttWindow.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSensitiveWin C-Win 
PROCEDURE setSensitiveWin :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ibSensitive AS LOG NO-UNDO.

FOR EACH ttWindow:
  ttWindow.hWindow:CURRENT-WINDOW:SENSITIVE = ibSensitive.
END.
THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = ibSensitive.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartWindow C-Win 
PROCEDURE StartWindow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icWinName      AS CHAR NO-UNDO.
DEF INPUT PARAM ihSelectedMenu AS HANDLE NO-UNDO.
DEF VAR hMenuItm AS HANDLE NO-UNDO.
DEF VAR bNewWin  AS LOG    NO-UNDO.

ASSIGN bNewWin   = NUM-ENTRIES(icWinName) > 1
       icWinName = ENTRY(1,icWinName)
       .

IF NOT bNewWin THEN
  FIND FIRST ttWindow 
       WHERE ttWindow.cWinName = icWinName NO-ERROR.
IF NOT AVAIL ttWindow OR bNewWin THEN DO:
  CREATE ttWindow.
  RUN VALUE(icWinName) PERSIST SET ttWindow.hWindow.
  ASSIGN ttWindow.cWinName     = icWinName
         ttWindow.cMenuTitle   = ihSelectedMenu:LABEL
         .
  
  IF NOT VALID-HANDLE(hWinMenu) THEN
    CREATE SUB-MENU hWinMenu
           ASSIGN PARENT = THIS-PROCEDURE:CURRENT-WINDOW:MENUBAR
                  LABEL = IF DYNAMIC-FUNCTION("Scandinavian") THEN "Vindu" ELSE "Window"
                  NAME  = "m_Vindu"
                  .
  CREATE MENU-ITEM ttWindow.hWinMenuItem
         ASSIGN TOGGLE-BOX = TRUE
                PARENT     = hWinMenu
                LABEL      = ihSelectedMenu:LABEL
                NAME       = "m_" + ihSelectedMenu:LABEL
                TRIGGERS:
                  ON VALUE-CHANGED PERSISTENT RUN BringToTop IN THIS-PROCEDURE (ttWindow.hWindow,hWinMenu,ttWindow.hWinMenuItem).
                END TRIGGERS
                .
END.
IF CAN-DO(ttWindow.hWindow:INTERNAL-ENTRIES,"InitializeObject") THEN RUN InitializeObject IN ttWindow.hWindow NO-ERROR.
ttWindow.hWindow:CURRENT-WINDOW:TITLE = ihSelectedMenu:LABEL.
ttWindow.hWindow:CURRENT-WINDOW:MOVE-TO-TOP().
hMenuItm = hWinMenu:FIRST-CHILD.
REPEAT WHILE VALID-HANDLE(hMenuItm):
  hMenuItm:CHECKED = IF hMenuItm = ttWindow.hWinMenuItem THEN TRUE ELSE FALSE.
  hMenuItm = hMenuItm:NEXT-SIBLING.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setDisabledMenus C-Win 
FUNCTION setDisabledMenus RETURNS LOGICAL
  ( INPUT ihMenu     AS HANDLE,
    INPUT icDisabled AS CHAR   ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hMenuItm AS HANDLE NO-UNDO.

REPEAT WHILE VALID-HANDLE(ihMenu):
  IF LOOKUP(ihMenu:LABEL,icDisabled,"|") > 0 THEN
    ihMenu:SENSITIVE = FALSE.
  IF CAN-QUERY(ihMenu,"first-child") THEN
    setDisabledMenus(ihMenu:FIRST-CHILD,icDisabled).
  ihMenu = ihMenu:NEXT-SIBLING.
END.
RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

