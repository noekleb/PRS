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

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEF VAR bOK             AS LOG NO-UNDO.
DEF VAR ix              AS INT NO-UNDO.

DEF VAR hToolbar        AS HANDLE NO-UNDO.
DEF VAR hBrowse         AS HANDLE NO-UNDO.
DEF VAR hFieldMap       AS HANDLE NO-UNDO.
DEF VAR hDocList        AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnCompanyLogo rectBrowse rectToolBar ~
cCompanyName cAddress1 cAddress2 cPostalCode cCity cTlfWrk cFax cEmail cURL ~
cVat bVat cBankAccount cmbCodemaster cmbParent cCompanyLogo 
&Scoped-Define DISPLAYED-OBJECTS cCompanyName cAddress1 cAddress2 ~
cPostalCode cCity cTlfWrk cFax cEmail cURL cVat bVat cBankAccount ~
cmbCodemaster cmbParent cCompanyLogo 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ValidateImage C-Win 
FUNCTION ValidateImage RETURNS LOGICAL
  ( INPUT icFileName    AS CHAR,
    INPUT ihTargetField AS HANDLE)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ViewCompanyRelations C-Win 
FUNCTION ViewCompanyRelations RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCompanyLogo 
     IMAGE-UP FILE "bmp/open16e.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4 BY 1.

DEFINE VARIABLE cmbCodemaster AS CHARACTER FORMAT "X(256)":U 
     LABEL "Kodetabeller" 
     VIEW-AS COMBO-BOX INNER-LINES 30
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 39 BY 1 TOOLTIP "Firma som kodertabeller skal hentes fra" NO-UNDO.

DEFINE VARIABLE cmbParent AS CHARACTER FORMAT "X(256)":U 
     LABEL "Tilhører" 
     VIEW-AS COMBO-BOX INNER-LINES 30
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 39 BY 1 TOOLTIP "Firma som kodertabeller skal hentes fra" NO-UNDO.

DEFINE VARIABLE cAddress1 AS CHARACTER FORMAT "x(50)" 
     LABEL "Adresse" 
     VIEW-AS FILL-IN 
     SIZE 39.2 BY 1.

DEFINE VARIABLE cAddress2 AS CHARACTER FORMAT "x(40)" 
     VIEW-AS FILL-IN 
     SIZE 39.2 BY 1.

DEFINE VARIABLE cBankAccount AS CHARACTER FORMAT "x(255)" 
     LABEL "Konto" 
     VIEW-AS FILL-IN 
     SIZE 48.8 BY 1.

DEFINE VARIABLE cCity AS CHARACTER FORMAT "x(40)" 
     LABEL "Sted" 
     VIEW-AS FILL-IN 
     SIZE 23.2 BY 1.

DEFINE VARIABLE cCompanyLogo AS CHARACTER FORMAT "x(256)" 
     LABEL "Logo" 
     VIEW-AS FILL-IN 
     SIZE 44 BY 1.

DEFINE VARIABLE cCompanyName AS CHARACTER FORMAT "X(40)" 
     LABEL "Navn" 
     VIEW-AS FILL-IN 
     SIZE 39.2 BY 1.

DEFINE VARIABLE cEmail AS CHARACTER FORMAT "x(60)" 
     LABEL "Epost" 
     VIEW-AS FILL-IN 
     SIZE 39.2 BY 1.

DEFINE VARIABLE cFax AS CHARACTER FORMAT "x(15)" 
     LABEL "Fax" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE cPostalCode AS CHARACTER FORMAT "x(8)" 
     LABEL "Postnr" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1.

DEFINE VARIABLE cTlfWrk AS CHARACTER FORMAT "x(15)" 
     LABEL "Tlf" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE cURL AS CHARACTER FORMAT "x(40)" 
     LABEL "Webside" 
     VIEW-AS FILL-IN 
     SIZE 39.2 BY 1.

DEFINE VARIABLE cVat AS CHARACTER FORMAT "x(12)" 
     LABEL "Org.nr" 
     VIEW-AS FILL-IN 
     SIZE 17.6 BY 1.

DEFINE RECTANGLE rectBrowse
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 43 BY 14.52.

DEFINE RECTANGLE rectToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 28 BY .95.

DEFINE VARIABLE bVat AS LOGICAL INITIAL no 
     LABEL "Momsregistrert" 
     VIEW-AS TOGGLE-BOX
     SIZE 19.4 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnCompanyLogo AT ROW 14.24 COL 103.6
     cCompanyName AT ROW 2.57 COL 57.2 COLON-ALIGNED
     cAddress1 AT ROW 3.62 COL 57.2 COLON-ALIGNED
     cAddress2 AT ROW 4.67 COL 57.2 COLON-ALIGNED NO-LABEL
     cPostalCode AT ROW 5.71 COL 57.2 COLON-ALIGNED
     cCity AT ROW 5.71 COL 73.2 COLON-ALIGNED
     cTlfWrk AT ROW 6.76 COL 57.2 COLON-ALIGNED
     cFax AT ROW 6.76 COL 79.4 COLON-ALIGNED
     cEmail AT ROW 7.81 COL 57.2 COLON-ALIGNED
     cURL AT ROW 8.86 COL 57.2 COLON-ALIGNED
     cVat AT ROW 9.91 COL 57.2 COLON-ALIGNED
     bVat AT ROW 10 COL 77.8
     cBankAccount AT ROW 10.95 COL 57.2 COLON-ALIGNED
     cmbCodemaster AT ROW 12.05 COL 57.2 COLON-ALIGNED
     cmbParent AT ROW 13.14 COL 57.2 COLON-ALIGNED
     cCompanyLogo AT ROW 14.24 COL 57.2 COLON-ALIGNED
     rectBrowse AT ROW 2.43 COL 2
     rectToolBar AT ROW 1.24 COL 1.8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 107.4 BY 16.


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
         TITLE              = "Vedlikehold firma"
         HEIGHT             = 16
         WIDTH              = 107.4
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
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
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
ON END-ERROR OF C-Win /* Vedlikehold firma */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Vedlikehold firma */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Vedlikehold firma */
DO:
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCompanyLogo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCompanyLogo C-Win
ON CHOOSE OF btnCompanyLogo IN FRAME DEFAULT-FRAME
DO:
  DEF VAR cFileName AS CHAR NO-UNDO.
  SYSTEM-DIALOG GET-FILE cFileName 
                FILTERS "Bmp files" "*.bmp"
                MUST-EXIST
                UPDATE bOk.
  ValidateImage(cFileName,cCompanyLogo:HANDLE).
  THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
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
  IF VALID-HANDLE(hDocList) THEN APPLY "close" TO hDocList.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplayRecord C-Win 
PROCEDURE DisplayRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN SUPER.

ViewCompanyRelations().

IF VALID-HANDLE(hDocList) THEN DO:
  IF hFieldMap:AVAIL THEN
    DYNAMIC-FUNCTION("setDocContext" IN hDocList,"JBoxCompany",
                                                 STRING(hFieldMap:BUFFER-FIELD("iJBoxCompanyId"):BUFFER-VALUE),
                                                 "").
  ELSE APPLY "close" TO hDocList.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DocListRecord C-Win 
PROCEDURE DocListRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF NOT VALID-HANDLE(hDocList) THEN DO:
  RUN JBoxViewDocList.w PERSIST SET hDocList.
  RUN InitializeObject IN hDocList.
END.
DYNAMIC-FUNCTION("setDocContext" IN hDocList,STRING(hFieldMap:BUFFER-FIELD("iJBoxCompanyId"):BUFFER-VALUE),
                                             "JBoxCompany",   
                                             "").
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
  DISPLAY cCompanyName cAddress1 cAddress2 cPostalCode cCity cTlfWrk cFax cEmail 
          cURL cVat bVat cBankAccount cmbCodemaster cmbParent cCompanyLogo 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btnCompanyLogo rectBrowse rectToolBar cCompanyName cAddress1 cAddress2 
         cPostalCode cCity cTlfWrk cFax cEmail cURL cVat bVat cBankAccount 
         cmbCodemaster cmbParent cCompanyLogo 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
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
DEF VAR bCompanyLogoInstalled AS LOG NO-UNDO.
DEF VAR bScand                AS LOG NO-UNDO.
  
RUN enable_UI.

DYNAMIC-FUNCTION("CreateStatusBar",THIS-PROCEDURE:CURRENT-WINDOW,"",0,YES,?) NO-ERROR.

DO WITH FRAME {&FRAME-NAME}:
  ASSIGN bCompanyLogoInstalled   = DYNAMIC-FUNCTION("isFieldNameInTable","JBoxCompany","cCompanyLogo")
         bScand                  = DYNAMIC-FUNCTION("Scandinavian")
         cmbCodemaster:DELIMITER = "|"
         cmbCodemaster:LIST-ITEM-PAIRS = "||" + DYNAMIC-FUNCTION("getFieldList","JBoxCompany;cCompanyName;iJBoxCompanyId","WHERE true")
         cmbParent:DELIMITER = "|"
         cmbParent:LIST-ITEM-PAIRS = cmbCodemaster:LIST-ITEM-PAIRS
         .

  IF NOT bCompanyLogoInstalled THEN
    ASSIGN cCompanyLogo:HIDDEN = YES
           btnCompanyLogo:HIDDEN = YES.
  ELSE DO:
    IF bScand THEN cCompanyLogo:HELP = "Hvis blank hentes logo fra eier-firma evt firma som holder kodetabeller".
    ELSE cCompanyLogo:HELP = "Uses value from owner or code-master company if blank".
  END.

  hBrowse = DYNAMIC-FUNCTION("NewBrowse",
                    rectBrowse:HANDLE,
                    100,
                    "",
                    "JBoxCompany"
                   ,"WHERE true" 
                   ,"").                     

  DYNAMIC-FUNCTION("setSortString",hBrowse,"iJBoxCompanyId").

  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap",
                    hBrowse:QUERY,
                    FRAME {&FRAME-NAME}:HANDLE,   
                    "cCompanyName,cAddress1,cAddress2,cPostalCode,cCity,cTlfWrk,cFax,cEmail,cURL,cBankAccount,cVat,bVat"
                  + (IF bCompanyLogoInstalled THEN ",cCompanyLogo" ELSE "")
                   ,"",                 
                    "","", 
                    "cmbCodemaster,cmbParent").

  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"bufferextrafields","codemaster,parent").
  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"customUpdateValProc","=jbadmin_company_update.p").

  DYNAMIC-FUNCTION("NewToolBar",
                    rectToolBar:HANDLE,
                    "Fil",            
                    "new;Ny,undo;Angre,delete;Slett,save;Lagre,excel;Eksporter til E&xcel"
                  + ",DocList;Dokumenter"
                   ,"maxborder").
  

  /* Link objects: */

  DYNAMIC-FUNCTION("LinkAllObjects",                /* Link all created objects. Linktype is type of "to" object,
                                                      f.ex link from browse to combo-box is combo-box link */
                    THIS-PROCEDURE:CURRENT-WINDOW,  /* Link only objects created for current window */
                    TRUE,                           /* Replace any existing links */
                    ""). /* Except these objects */


  APPLY "value-changed" TO hBrowse.

END.

DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectToolBar").
DYNAMIC-FUNCTION("setOrgWinSize", THIS-PROCEDURE:CURRENT-WINDOW,200,300,0,0).

DYNAMIC-FUNCTION("initTranslation",THIS-PROCEDURE:CURRENT-WINDOW).

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
{&WINDOW-NAME}:HIDDEN = NO.
{&WINDOW-NAME}:WINDOW-STATE = 3.
{&WINDOW-NAME}:MOVE-TO-TOP().
APPLY "entry" TO hBrowse.
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
DO WITH FRAME {&FRAME-NAME}:
  IF NOT ValidateImage(cCompanyLogo:SCREEN-VALUE,?) THEN RETURN.

  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"bufferextravalues",
                   (IF cmbCodemaster:SCREEN-VALUE NE ? THEN cmbCodemaster:SCREEN-VALUE ELSE "") + "|" + 
                   (IF cmbParent:SCREEN-VALUE NE ? THEN cmbParent:SCREEN-VALUE ELSE "")).
  RUN SUPER.

  ASSIGN cmbCodemaster:LIST-ITEM-PAIRS  = "||" + DYNAMIC-FUNCTION("getFieldList","JBoxCompany;cCompanyName;iJBoxCompanyId","WHERE true")
         cmbParent:LIST-ITEM-PAIRS = cmbCodemaster:LIST-ITEM-PAIRS
         .
  ViewCompanyRelations().
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ValidateImage C-Win 
FUNCTION ValidateImage RETURNS LOGICAL
  ( INPUT icFileName    AS CHAR,
    INPUT ihTargetField AS HANDLE) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF icFileName = "" THEN RETURN YES.

IF icFileName MATCHES "*bmp" THEN DO WITH FRAME {&FRAME-NAME}:
  IF VALID-HANDLE(ihTargetField) THEN DO:
    ihTargetField:SCREEN-VALUE = ENTRY(NUM-ENTRIES(icFileName,"\"),icFileName,"\").
    IF SEARCH(ihTargetField:SCREEN-VALUE) = ? THEN DO ix = 1 TO 4:
      ihTargetField:SCREEN-VALUE = ENTRY(NUM-ENTRIES(icFileName,"\") - ix,icFileName,"\") + "\" + ihTargetField:SCREEN-VALUE.
      IF SEARCH(ihTargetField:SCREEN-VALUE) NE ? THEN LEAVE.
    END.
    IF SEARCH(ihTargetField:SCREEN-VALUE) = ? THEN DO:
      MESSAGE "Invalid image: " icFileName SKIP "(must be in PROPATH)"
               VIEW-AS ALERT-BOX ERROR.
      ihTargetField:SCREEN-VALUE = "".
      RETURN NO.
    END.    

    APPLY "any-printable" TO ihTargetField.
    THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
  END.
  ELSE IF SEARCH(icFileName) = ? THEN DO:
    DYNAMIC-FUNCTION("DoMessage",0,0,"Image " + icFilename + " not found","","").
    RETURN NO.  
  END.

  RETURN YES.
END.
ELSE 
  DYNAMIC-FUNCTION("DoMessage",0,0,"Invalid image file type: Must be bmp","","").
  
RETURN NO.
  
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ViewCompanyRelations C-Win 
FUNCTION ViewCompanyRelations RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cValue AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  IF hFieldMap:AVAIL THEN DO:
    cValue = DYNAMIC-FUNCTION("getFieldValues","JBoxCompanyToCompany"
                                              ,"WHERE iJBoxCompanyId = " + STRING(hFieldMap:BUFFER-FIELD("iJBoxCompanyId"):BUFFER-VALUE)
                                              + " AND cCompanyRole = 'codemaster'"
                                              ,"iJBoxToCompanyId").
    IF cValue = ? THEN
      cmbCodemaster:SCREEN-VALUE = " ".
    ELSE cmbCodemaster:SCREEN-VALUE = cValue.

    cValue = DYNAMIC-FUNCTION("getFieldValues","JBoxCompanyToCompany"
                                              ,"WHERE iJBoxCompanyId = " + STRING(hFieldMap:BUFFER-FIELD("iJBoxCompanyId"):BUFFER-VALUE)
                                               + " AND cCompanyRole = 'parent'"
                                              ,"iJBoxToCompanyId").
    IF cValue = ? THEN
      cmbParent:SCREEN-VALUE = " ".
    ELSE cmbParent:SCREEN-VALUE = cValue.
  END.
  ELSE 
    ASSIGN cmbCodemaster:SCREEN-VALUE = " "
           cmbParent:SCREEN-VALUE = " ".

  ASSIGN cmbCodemaster:MODIFIED = NO
         cmbParent:MODIFIED = NO.
END.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

