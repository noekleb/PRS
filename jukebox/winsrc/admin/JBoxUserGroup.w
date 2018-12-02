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

DEF VAR bOK               AS LOG NO-UNDO.
DEF VAR ix                AS INT NO-UNDO.
                          
DEF VAR hToolbar          AS HANDLE NO-UNDO.
DEF VAR hBrowse           AS HANDLE NO-UNDO.
DEF VAR hFieldMap         AS HANDLE NO-UNDO.
DEF VAR hBrowseMembers    AS HANDLE NO-UNDO.
DEF VAR hToolbarCompany   AS HANDLE NO-UNDO.
DEF VAR hAdminOverlay     AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rectBrowse rectToolBar ~
rectBrwUserGroupMembers iJBoxCompanyId cUserGroupName 
&Scoped-Define DISPLAYED-OBJECTS iJBoxCompanyId cUserGroupName 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE iJBoxCompanyId AS CHARACTER FORMAT "X(256)":U 
     LABEL "Company" 
     VIEW-AS COMBO-BOX INNER-LINES 30
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 35.86 BY 1 NO-UNDO.

DEFINE VARIABLE cUserGroupName AS CHARACTER FORMAT "x(40)" 
     LABEL "Group" 
     VIEW-AS FILL-IN 
     SIZE 36 BY 1.

DEFINE RECTANGLE rectBrowse
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 40 BY 14.54.

DEFINE RECTANGLE rectBrwUserGroupMembers
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 49 BY 11.85.

DEFINE RECTANGLE rectToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 28 BY .96.

DEFINE BUTTON btnSplitBarX 
     IMAGE-UP FILE "bmp/tableft.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Button 1" 
     SIZE 2 BY 14.15.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     iJBoxCompanyId AT ROW 2.46 COL 54.14 COLON-ALIGNED
     cUserGroupName AT ROW 3.58 COL 54 COLON-ALIGNED
     rectBrowse AT ROW 2.42 COL 2
     rectToolBar AT ROW 1.23 COL 1.86
     rectBrwUserGroupMembers AT ROW 5.04 COL 44.57
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 93 BY 16.

DEFINE FRAME frmSplitBarX
     btnSplitBarX AT ROW 1 COL 13.43
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 29.8 ROW 2.57
         SCROLLABLE SIZE 16.14 BY 14.31.


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
         TITLE              = "Vedlikehold brukergrupper"
         HEIGHT             = 16
         WIDTH              = 93
         MAX-HEIGHT         = 57.15
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 57.15
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
       FRAME frmSplitBarX:HEIGHT           = 14.29
       FRAME frmSplitBarX:WIDTH            = 16.2.

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
ON END-ERROR OF C-Win /* Vedlikehold brukergrupper */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Vedlikehold brukergrupper */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Vedlikehold brukergrupper */
DO:
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").
  DEF VAR hColumn AS HANDLE NO-UNDO.
  hColumn = hBrowseMembers:GET-BROWSE-COLUMN(1).
  APPLY "end-resize" TO hColumn.
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
  hColumn = hBrowseMembers:GET-BROWSE-COLUMN(1).
  APPLY "end-resize" TO hColumn.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME iJBoxCompanyId
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL iJBoxCompanyId C-Win
ON VALUE-CHANGED OF iJBoxCompanyId IN FRAME DEFAULT-FRAME /* Company */
DO:

  IF NOT CAN-DO("new",DYNAMIC-FUNCTION("getToolbarState",hToolbar)) THEN 
    RUN OpenQuery.
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

  &IF DEFINED(UIB_is_Running) NE 0 &THEN
    RUN InitializeObject.
    RUN MoveToTop.
  &ENDIF
/*   APPLY "end-move" TO btnSplitBarX IN FRAME frmSplitBarX.  */

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
  DISPLAY iJBoxCompanyId cUserGroupName 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE rectBrowse rectToolBar rectBrwUserGroupMembers iJBoxCompanyId 
         cUserGroupName 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  ENABLE btnSplitBarX 
      WITH FRAME frmSplitBarX IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frmSplitBarX}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GroupMembersRecord C-Win 
PROCEDURE GroupMembersRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cUserRowIdList AS CHAR NO-UNDO.
DEF VAR cUserIdList    AS CHAR NO-UNDO.

THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.

cUserRowIdList = DYNAMIC-FUNCTION("getRowIdList","JBoxUserGroupMembers,JBoxUser","",
                                   "WHERE iJBoxUserGroupId = " + STRING(hFieldMap:BUFFER-FIELD("iJBoxUserGroupId"):BUFFER-VALUE)
                                 + ",FIRST JBoxUser OF JBoxUserGroupMembers NO-LOCK"). 
RUN JBoxSelector.w (THIS-PROCEDURE,0,
                    "JBoxUser;cJBoxUserId;cUserName;bActive"
                  + ";+CompanyList|CHARACTER|x(100)|jbadmin_user_company_brwcalc.p(cJBoxUserId)|Companies"
                   ,"where true",
                    INPUT-OUTPUT cUserRowIdList,
                    "cJBoxUserId",
                    INPUT-OUTPUT cUserIdList,
                    "","",
                    OUTPUT bOK).
THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

IF bOk THEN DO:
  IF NOT DYNAMIC-FUNCTION("runproc","jbadmin_editgroupusers.p",
                          STRING(hFieldMap:BUFFER-FIELD("iJBoxUserGroupId"):BUFFER-VALUE) + "|" + cUserIdList,?) THEN
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
RUN enable_UI.

DEF VAR bUseCompany AS LOG NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:

  bUseCompany = DYNAMIC-FUNCTION("getIsFieldInTable","JBoxUserGroup.iJBoxCompanyId").
  IF NOT bUseCompany THEN 
    iJBoxCompanyId:HIDDEN = YES.  
  ELSE 
    ASSIGN iJBoxCompanyId:DELIMITER       = "|"
           iJBoxCompanyId:LIST-ITEM-PAIRS = RIGHT-TRIM("Global (0)|0|"
                                          +  DYNAMIC-FUNCTION("getFieldList",
                                            "JBoxCompany;cCompanyName;iJBoxCompanyId",
                                            "WHERE true BY cCompanyName"),"|") 
          .


  hBrowse = DYNAMIC-FUNCTION("NewBrowse",          
                    rectBrowse:HANDLE,             
                    100,                           
                    "",                            
                    "JBoxUserGroup"
                    + ";cUserGroupName"
                    + ";!iJBoxUserGroupId"
                    + (IF bUseCompany THEN ";iJBoxCompanyId,JBoxCompany;cCompanyName" ELSE "") 
                   ,"WHERE true"
                  + (IF bUseCompany THEN ",FIRST JBoxCompany NO-LOCK WHERE JBoxCompany.iJBoxCompanyId = JBoxUserGroup.iJBoxCompanyId OUTER-JOIN" ELSE "")
                   ,"").                           

  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap",      
                    hBrowse:QUERY,
                    FRAME {&FRAME-NAME}:HANDLE,     
                    (IF bUseCompany THEN "iJBoxCompanyId," ELSE "") + "cUserGroupName",               
                    "",                             
                    "","",                          
                    "").                            

  hToolbar = DYNAMIC-FUNCTION("NewToolBar",
                    rectToolBar:HANDLE,             
                    "File",                          
                    "new,undo,delete,save,filter,flatview,excel"
                  + ",GroupMembers;Members",
                    "maxborder").    

  hBrowseMembers = DYNAMIC-FUNCTION("NewBrowse",   
                    rectBrwUserGroupMembers:HANDLE,
                    100,                           
                    "",                            
                    "JBoxUserGroupMembers"         
                    + ";!iJBoxUserGroupId;cJBoxUserId|Userid"
                    + ",JBoxUser"
                    + ";cUserName|Name"
                    + ";bActive"
                    + ";+CompanyList|CHARACTER|x(100)|jbadmin_user_company_brwcalc.p(cJBoxUserId)|Companies"
                   ,"WHERE false, FIRST JBoxUser OF JBoxUserGroupMembers", 
                    "").                           
  hBrowseMembers:NAME = "brwCompanyUser".          

  DYNAMIC-FUNCTION("NewMenuBand",hBrowseMembers
                  ,"MultiSortBrowse;Sort on multiple columns"
                 + ",Excel"
                  ,""). 

  /* Link objects: */

  DYNAMIC-FUNCTION("CreateObjectLink",hToolbar,hBrowse).
  DYNAMIC-FUNCTION("CreateObjectLink",hToolbar,hFieldMap).
  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hFieldMap).
  DYNAMIC-FUNCTION("CreateParentLink",hBrowseMembers,hBrowse,"iJBoxUserGroupId").

  APPLY "value-changed" TO hBrowse.

END.

DYNAMIC-FUNCTION("setSplitBarX",THIS-PROCEDURE:CURRENT-WINDOW,btnSplitBarX:HANDLE IN FRAME frmSplitBarX,NO).
DYNAMIC-FUNCTION("setFollowSplitBarX",THIS-PROCEDURE:CURRENT-WINDOW,btnSplitBarX:HANDLE IN FRAME frmSplitBarX,
                  STRING(hBrowse) + "," +
                  STRING(rectBrowse:HANDLE) + "," +
                  STRING(hBrowseMembers) + "," +
                  STRING(rectBrwUserGroupMembers:HANDLE) + "," +

                  STRING(iJBoxCompanyId:HANDLE) + "," +
                  STRING(cUserGroupName:HANDLE)
                  ).

DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, hBrowse:NAME + ",rectBrowse").
DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectToolBar").
DYNAMIC-FUNCTION("setAddResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "cJBoxUserId,cUserName").
DYNAMIC-FUNCTION("setOrgWinSize", THIS-PROCEDURE:CURRENT-WINDOW,200,200,0,250).

DYNAMIC-FUNCTION("initTranslation",THIS-PROCEDURE:CURRENT-WINDOW).

THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.

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
THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = NO.
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
DYNAMIC-FUNCTION("setAttribute",hBrowseMembers,"basequery","where false").
DYNAMIC-FUNCTION("setCurrentObject",hBrowseMembers).
RUN OpenQuery.
DYNAMIC-FUNCTION("setCurrentObject",hToolbar).
DYNAMIC-FUNCTION("setAttribute",hToolbar,"disabledevents","GroupMembers").
RUN SUPER.  
DYNAMIC-FUNCTION("setAttribute",hToolbar,"disabledevents","").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

