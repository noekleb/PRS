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
DEF VAR hTBcodeType     AS HANDLE NO-UNDO.
DEF VAR hBrowse         AS HANDLE NO-UNDO.
DEF VAR hFieldMap       AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-74 RECT-75 rectBrowse rectTBcodeType ~
rectToolBar cmbCodeType cTypeDescription cmbCompany cCodeValue cDescription ~
cLanguage cMisc1 cMisc2 
&Scoped-Define DISPLAYED-OBJECTS cmbCodeType cTypeDescription cmbCompany ~
cCodeValue cDescription cLanguage cMisc1 cMisc2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE cmbCodeType AS CHARACTER FORMAT "X(256)":U 
     LABEL "Codetype" 
     VIEW-AS COMBO-BOX INNER-LINES 30
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 32.8 BY 1 NO-UNDO.

DEFINE VARIABLE cmbCompany AS CHARACTER FORMAT "X(256)":U 
     LABEL "Company" 
     VIEW-AS COMBO-BOX INNER-LINES 30
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 33 BY 1 NO-UNDO.

DEFINE VARIABLE cCodeValue AS CHARACTER FORMAT "X(256)" 
     LABEL "Value" 
     VIEW-AS FILL-IN 
     SIZE 37 BY 1.

DEFINE VARIABLE cDescription AS CHARACTER FORMAT "X(256)" 
     LABEL "Description" 
     VIEW-AS FILL-IN 
     SIZE 36.8 BY 1.

DEFINE VARIABLE cLanguage AS CHARACTER FORMAT "X(3)" 
     LABEL "Language" 
     VIEW-AS FILL-IN 
     SIZE 8.6 BY 1.

DEFINE VARIABLE cMisc1 AS CHARACTER FORMAT "X(256)" 
     LABEL "Extra1" 
     VIEW-AS FILL-IN 
     SIZE 37.2 BY 1.

DEFINE VARIABLE cMisc2 AS CHARACTER FORMAT "X(256)" 
     LABEL "Extra2" 
     VIEW-AS FILL-IN 
     SIZE 37.2 BY 1.

DEFINE VARIABLE cTypeDescription AS CHARACTER FORMAT "X(50)" 
     VIEW-AS FILL-IN 
     SIZE 28.4 BY 1.

DEFINE RECTANGLE RECT-74
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 91 BY 1.43.

DEFINE RECTANGLE RECT-75
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 51 BY 13.

DEFINE RECTANGLE rectBrowse
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 40 BY 12.95.

DEFINE RECTANGLE rectTBcodeType
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 17.4 BY 1.05.

DEFINE RECTANGLE rectToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 25 BY .95.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     cmbCodeType AT ROW 1.24 COL 11.2 COLON-ALIGNED
     cTypeDescription AT ROW 1.24 COL 44.6 COLON-ALIGNED NO-LABEL
     cmbCompany AT ROW 2.67 COL 11 COLON-ALIGNED
     cCodeValue AT ROW 4.48 COL 53 COLON-ALIGNED
     cDescription AT ROW 5.57 COL 53 COLON-ALIGNED
     cLanguage AT ROW 6.71 COL 52.8 COLON-ALIGNED
     cMisc1 AT ROW 7.81 COL 52.8 COLON-ALIGNED
     cMisc2 AT ROW 8.91 COL 52.8 COLON-ALIGNED
     RECT-74 AT ROW 1 COL 2
     RECT-75 AT ROW 3.86 COL 42
     rectBrowse AT ROW 3.86 COL 2
     rectTBcodeType AT ROW 1.24 COL 76
     rectToolBar AT ROW 15.52 COL 68
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 93 BY 16.


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
         TITLE              = "General code table"
         HEIGHT             = 16
         WIDTH              = 93
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
ASSIGN 
       cmbCodeType:DELIMITER IN FRAME DEFAULT-FRAME      = "|".

ASSIGN 
       cTypeDescription:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

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
ON END-ERROR OF C-Win /* General code table */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* General code table */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* General code table */
DO:
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbCodeType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbCodeType C-Win
ON VALUE-CHANGED OF cmbCodeType IN FRAME DEFAULT-FRAME /* Codetype */
DO:
  RUN OpenQuery.
  DYNAMIC-FUNCTION("setToolbar",rectTBcodeType:HANDLE,"avail").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbCompany
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbCompany C-Win
ON VALUE-CHANGED OF cmbCompany IN FRAME DEFAULT-FRAME /* Company */
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
  DYNAMIC-FUNCTION("setCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
  DYNAMIC-FUNCTION("DoCleanUpObjects",THIS-PROCEDURE:CURRENT-WINDOW).
  PUBLISH "InvalidateHandle" (THIS-PROCEDURE).
  RUN disable_UI.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

{incl/wintrigg.i}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ChangeCompany C-Win 
PROCEDURE ChangeCompany :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  cmbCompany:SCREEN-VALUE = DYNAMIC-FUNCTION("getCompany") NO-ERROR.
  APPLY "value-changed" TO cmbCodeType.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CopyType C-Win 
PROCEDURE CopyType :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iToCompanyId  AS INT NO-UNDO.
DEF VAR cCurrDesc     AS CHAR NO-UNDO.

iToCompanyId = DYNAMIC-FUNCTION("getCompanyId").
RUN JBoxDSelectCompany.w (FALSE,INPUT-OUTPUT iToCompanyId).
IF iToCompanyId NE 0 THEN DO WITH FRAME {&FRAME-NAME}:
  hBrowse:QUERY:GET-FIRST().
  REPEAT WHILE NOT hBrowse:QUERY:QUERY-OFF-END:
    DYNAMIC-FUNCTION("DoCreate","JBoxGenCode","ignore",
                     "iJBoxCompanyId,cCodeType,cCodeValue,cDescription,cLanguage,cMisc1,cMisc2",
                     STRING(iToCompanyId) + "|" + 
                       hFieldMap:BUFFER-FIELD("cCodeType"):BUFFER-VALUE + "|" + 
                       hFieldMap:BUFFER-FIELD("cCodeValue"):BUFFER-VALUE + "|" +
                       hFieldMap:BUFFER-FIELD("cDescription"):BUFFER-VALUE + "|" +
                       hFieldMap:BUFFER-FIELD("cLanguage"):BUFFER-VALUE + "|" +
                       hFieldMap:BUFFER-FIELD("cMisc1"):BUFFER-VALUE + "|" +
                       hFieldMap:BUFFER-FIELD("cMisc2"):BUFFER-VALUE,
                     FALSE).
    hBrowse:QUERY:GET-NEXT().
  END.
  bOK = DYNAMIC-FUNCTION("DoCommit",FALSE).

  IF bOK AND DYNAMIC-FUNCTION("DoMessage",0,4,"Change to target company?","","") = 6 THEN DO:
    DYNAMIC-FUNCTION("setCompanyId",iToCompanyId).
    RUN ChangeCompany.
  END.
  ELSE IF NOT bOk THEN DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeleteType C-Win 
PROCEDURE DeleteType :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

IF DYNAMIC-FUNCTION("DoMessage",0,4,"Slette kodetype &1 og tilhørende koder?","",
                    DYNAMIC-FUNCTION("getDropDownLabel",cmbCodeType:HANDLE IN FRAME {&FRAME-NAME},cmbCodeType:DELIMITER)
                    ) = 6 THEN DO WITH FRAME {&FRAME-NAME}:
  bOk = DYNAMIC-FUNCTION("DoDelete","JBoxGenCodeType","=jbsetup_jboxgencodetype_delete.p",
                   "iJBoxGenCodeTypeId",
                   cmbCodeType:SCREEN-VALUE,
                   TRUE).
  IF bOk THEN DO:
    cmbCodeType:DELETE(cmbCodeType:SCREEN-VALUE).
    APPLY "value-changed" TO cmbCodeType.
  END.
  ELSE 
    MESSAGE "Feil i sletting av kodetype" SKIP
            DYNAMIC-FUNCTION("getTransactionMessage")
            VIEW-AS ALERT-BOX ERROR.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EditType C-Win 
PROCEDURE EditType :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR ioiTypeId      AS INT NO-UNDO.
DEF VAR iocDescription AS CHAR NO-UNDO.
DEF VAR iocCodeType    AS CHAR NO-UNDO.
bOK = FALSE.
DO WITH FRAME {&FRAME-NAME}:
  ASSIGN iocCodeType    = ENTRY(LOOKUP(cmbCodeType:SCREEN-VALUE, cmbCodeType:LIST-ITEM-PAIRS,"|") - 1,cmbCodeType:LIST-ITEM-PAIRS,"|")
         iocDescription = cTypeDescription:SCREEN-VALUE
         ioiTypeId      = INT(cmbCodeType:SCREEN-VALUE).
  RUN JBoxDCodeType.w ("edit",INPUT-OUTPUT ioiTypeId, INPUT-OUTPUT iocCodeType, INPUT-OUTPUT iocDescription, OUTPUT bOk).
  IF bOk THEN DO:
    cmbCodeType:REPLACE(iocCodeType,STRING(ioiTypeId),STRING(ioiTypeId)).
    cTypeDescription:SCREEN-VALUE = iocDescription.
  END.
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
  DISPLAY cmbCodeType cTypeDescription cmbCompany cCodeValue cDescription 
          cLanguage cMisc1 cMisc2 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-74 RECT-75 rectBrowse rectTBcodeType rectToolBar cmbCodeType 
         cTypeDescription cmbCompany cCodeValue cDescription cLanguage cMisc1 
         cMisc2 
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
DEF VAR hCol AS HANDLE NO-UNDO.

RUN enable_UI.

DYNAMIC-FUNCTION("CreateStatusBar",THIS-PROCEDURE:CURRENT-WINDOW,"",0,YES,?).
DO WITH FRAME {&FRAME-NAME}:
  ASSIGN cmbCodeType:LIST-ITEM-PAIRS = RIGHT-TRIM("| |" + DYNAMIC-FUNCTION("getFieldList",
                                            "JBoxGenCodeType;cCodeType;iJBoxGenCodeTypeId",
                                            "WHERE true BY cCodeType"),"|")  
         cmbCompany:DELIMITER       = "|"
         cmbCompany:LIST-ITEM-PAIRS = RIGHT-TRIM("Global (0)|0|"
                                         +  DYNAMIC-FUNCTION("getFieldList",
                                           "JBoxCompany;cCompanyName;iJBoxCompanyId",
                                           "WHERE true BY cCompanyName"),"|") 
         .
  IF DYNAMIC-FUNCTION("getAttribute",SESSION,"CompanyIsRole") = "yes" THEN cmbCompany:SENSITIVE = NO.       

  hBrowse = DYNAMIC-FUNCTION("NewBrowse",          /* Create a browse object */
                    rectBrowse:HANDLE,              /* Rectangle to define coordinates for browse */
                    100,                            /* Rows to batch */
                    "",                             /* Browse properties, ie MULTIPLE,NUM-LOCKED-COLUMNS, etc */
                    "JBoxGenCode"
                   + ";cCodeValue|Value|x(256)"
                   + ";cDescription|Description|x(256)"
                   + ";cLanguage|Language"
                   + ";cMisc1|Extra1|x(256)"
                   + ";cMisc2|Extra2|x(256)"
                   + ";!cCodeType",  
                    "WHERE false", 
                    "").                            /* Misc - for something I might need in next version.. */
  hBrowse:NAME = "brwGenCode". /* This name is neccessary because the browser is due to special treatment during resize */

  hCol = DYNAMIC-FUNCTION("getBrowseColumn",hBrowse,"cCodeValue").
  hCol:WIDTH-PIXELS = 200.
  hCol = DYNAMIC-FUNCTION("getBrowseColumn",hBrowse,"cDescription").
  hCol:WIDTH-PIXELS = 200.
  hCol = DYNAMIC-FUNCTION("getBrowseColumn",hBrowse,"cMisc1").
  hCol:WIDTH-PIXELS = 200.
  hCol = DYNAMIC-FUNCTION("getBrowseColumn",hBrowse,"cMisc2").
  hCol:WIDTH-PIXELS = 200.


  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap",      /* A fieldmap object holds extra info for display and input fields (fill-ins) 
                                                       and their corresponding buffer columns return handle equals the buffer handle */
                    hBrowse:QUERY,
                    FRAME {&FRAME-NAME}:HANDLE,     /* Frame for the input/display fields (might not be the same frame as the browse) */
                    "cCodeValue,cDescription,cLanguage,cMisc1,cMisc2","",  /* Update columns in buffer */
                    "","",                          /* Additional buffer and displ.fields - not updateable*/
                    "").                            /* Misc - for something I might need in next version.. */
  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"BufferExtraFields","cCodeType,iJBoxCompanyId").


  hToolbar = DYNAMIC-FUNCTION("NewToolBar",
                    rectToolBar:HANDLE,             /* Rectangle to define coordinates for toolbar */
                    "Fil",                          /* Corresponding menu label - no menu if blank */
                    "new;Ny,copy;Kopier,undo;Angre,delete;Slett,save;Lagre,excel;Eksporter til E&xcel",
                                                    /* Buttons / Menu items: action;label;tooltip;Method;image,action;label.. 
                                                       Any number of properties accepted (one ok - if predef. action) */
                    "").                            /* Misc - for something I might need in next version.. */

  DYNAMIC-FUNCTION("NewToolbar",rectTBcodeType:HANDLE,"Fil",
                   "new;N&y kodetype;;NewType,edit;En&dre kodetype;;EditType,copy;Kopier til annet firma;;CopyType,delete;SLett kodetype;;DeleteType","").

  /* Link objects: */

  DYNAMIC-FUNCTION("CreateObjectLink",rectToolBar:HANDLE,hBrowse).                
  DYNAMIC-FUNCTION("CreateObjectLink",rectToolBar:HANDLE,hFieldMap).                
  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hFieldMap).                

  DYNAMIC-FUNCTION("setCompanyHeader",THIS-PROCEDURE:CURRENT-WINDOW).

  RUN ChangeCompany.
  SUBSCRIBE TO "ChangeCompany" ANYWHERE.

END.

DYNAMIC-FUNCTION("setNoMoveY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "btnTo,btnFrom").
DYNAMIC-FUNCTION("setAddMoveX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "RECT-75").
DYNAMIC-FUNCTION("setAddResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "cTypeDescription").
/* DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "RECT-75"). */
DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "RECT-74").

DYNAMIC-FUNCTION("setResizeXGroup", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,50,"RECT-75,cCodeValue,cDescription,cMisc1,cMisc2,rectBrowse," + hBrowse:NAME).
DYNAMIC-FUNCTION("setMoveXGroup", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,50,"RECT-75,cCodeValue,cDescription,cMisc1,cMisc2,cLanguage").

DYNAMIC-FUNCTION("setOrgWinSize", THIS-PROCEDURE:CURRENT-WINDOW,230,200,0,200).

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
APPLY "window-resized" TO {&WINDOW-NAME}. 
APPLY "entry" TO hBrowse.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NewType C-Win 
PROCEDURE NewType :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR ioiTypeId AS INT NO-UNDO.
DEF VAR iocDescription AS CHAR NO-UNDO.
DEF VAR iocCodeType    AS CHAR NO-UNDO.

bOK = FALSE.
RUN JBoxDCodeType.w ("new",INPUT-OUTPUT ioiTypeId, INPUT-OUTPUT iocCodeType, INPUT-OUTPUT iocDescription, OUTPUT bOk).
IF bOk THEN DO WITH FRAME {&FRAME-NAME}:
  cmbCodeType:ADD-LAST(iocCodeType,STRING(ioiTypeId)).
  cmbCodeType:SCREEN-VALUE = STRING(ioiTypeId).
  cTypeDescription:SCREEN-VALUE = iocDescription.
  APPLY "value-changed" TO cmbCodeType.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenQuery C-Win 
PROCEDURE OpenQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cCompName AS CHAR   NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  IF cmbCodeType:SCREEN-VALUE NE "" THEN
    cTypeDescription:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","JBoxGenCodeType;cDescription",
                                                     "WHERE iJBoxGenCodeTypeId = " + cmbCodeType:SCREEN-VALUE).

  IF cmbCompany:SCREEN-VALUE = "" OR cmbCompany:SCREEN-VALUE = ? THEN RETURN.

  DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"queryFilter",
                   "WHERE cCodeType = '" + DYNAMIC-FUNCTION("getDropDownLabel",cmbCodeType:HANDLE,"|") + "'" +
                   "  AND iJBoxCompanyId = " + cmbCompany:SCREEN-VALUE).

  cCompName = DYNAMIC-FUNCTION("getDropDownLabel",cmbCompany:HANDLE,"|").
  IF THIS-PROCEDURE:CURRENT-WINDOW:TITLE MATCHES "*[*" THEN
    THIS-PROCEDURE:CURRENT-WINDOW:TITLE = SUBSTR(THIS-PROCEDURE:CURRENT-WINDOW:TITLE,1,INDEX(THIS-PROCEDURE:CURRENT-WINDOW:TITLE,"[")) + cCompName + "]" NO-ERROR.
  ELSE
    THIS-PROCEDURE:CURRENT-WINDOW:TITLE = THIS-PROCEDURE:CURRENT-WINDOW:TITLE + " [" + cCompName + "]" NO-ERROR.

  RUN SUPER.
END.
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
  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"BufferExtraValues",
                   DYNAMIC-FUNCTION("getDropDownLabel",cmbCodeType:HANDLE,"|") + "|" 
                 + cmbCompany:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
  RUN SUPER.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

