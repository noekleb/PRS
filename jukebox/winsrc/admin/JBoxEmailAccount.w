&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description:        Container for a JukeBox window program

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author:            brynjar@chemistry.no

  Created:           18.oct.2006

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

DEF VAR bOk         AS LOG    NO-UNDO.
DEF VAR ix          AS INT    NO-UNDO.
DEF VAR hBrowse     AS HANDLE NO-UNDO.
DEF VAR hQuery      AS HANDLE NO-UNDO.
DEF VAR hToolbar    AS HANDLE NO-UNDO.
DEF VAR hFieldMap   AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS cAccountName cDisplayName cEmailAddress ~
cReplyToAddress cSignatureText cMailServer cUserName cPassword iPort ~
iAuthMethod cActionInProc cActionInParam cActionOutProc cActionOutParam ~
cReadProc cSendProc iQueue# dCreated iJBoxCompanyId cMerknadLabel ~
cCreatedBy dModified cModifiedBy bActive cUsage TbReiseSatsType ~
brwReiseSatsType 
&Scoped-Define DISPLAYED-OBJECTS cAccountName cDisplayName cEmailAddress ~
cReplyToAddress cSignatureText cMailServer cUserName cPassword iPort ~
iAuthMethod cActionInProc cActionInParam cActionOutProc cActionOutParam ~
cReadProc cSendProc iQueue# dCreated iJBoxCompanyId cMerknadLabel ~
cCreatedBy dModified cModifiedBy bActive cUsage 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE cUsage AS CHARACTER FORMAT "x(15)" 
     LABEL "Usage" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEMS "In","Out","In/Out" 
     DROP-DOWN-LIST
     SIZE 17 BY 1 TOOLTIP "blank=all,In,Out,Sms..".

DEFINE VARIABLE iJBoxCompanyId AS CHARACTER FORMAT "X(256)" INITIAL "0" 
     LABEL "CompanyId" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 43.4 BY 1.

DEFINE VARIABLE cSignatureText AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 105.8 BY 2.76.

DEFINE VARIABLE cAccountName AS CHARACTER FORMAT "x(40)" 
     LABEL "Account name" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1.

DEFINE VARIABLE cActionInParam AS CHARACTER FORMAT "x(30)" 
     LABEL "Parameter inbound action" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE cActionInProc AS CHARACTER FORMAT "x(30)" 
     LABEL "Action proc, inbound" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE cActionOutParam AS CHARACTER FORMAT "x(30)" 
     LABEL "Parameter outbound action" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE cActionOutProc AS CHARACTER FORMAT "x(30)" 
     LABEL "Action proc, outbound" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE cCreatedBy AS CHARACTER FORMAT "x(8)" 
     LABEL "By" 
     VIEW-AS FILL-IN 
     SIZE 13.6 BY 1.

DEFINE VARIABLE cDisplayName AS CHARACTER FORMAT "x(40)" 
     LABEL "Display name" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1.

DEFINE VARIABLE cEmailAddress AS CHARACTER FORMAT "x(40)" 
     LABEL "Email address" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1.

DEFINE VARIABLE cMailServer AS CHARACTER FORMAT "x(40)" 
     LABEL "Mail server" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1.

DEFINE VARIABLE cMerknadLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Signature:" 
      VIEW-AS TEXT 
     SIZE 10 BY .62 NO-UNDO.

DEFINE VARIABLE cModifiedBy AS CHARACTER FORMAT "x(8)" 
     LABEL "By" 
     VIEW-AS FILL-IN 
     SIZE 13.6 BY 1.

DEFINE VARIABLE cPassword AS CHARACTER FORMAT "x(30)" 
     LABEL "Password" 
     VIEW-AS FILL-IN 
     SIZE 27.8 BY 1.

DEFINE VARIABLE cReadProc AS CHARACTER FORMAT "x(30)" 
     LABEL "Read proc" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE cReplyToAddress AS CHARACTER FORMAT "x(40)" 
     LABEL "Reply-to address" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1.

DEFINE VARIABLE cSendProc AS CHARACTER FORMAT "x(30)" 
     LABEL "Send proc" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE cUserName AS CHARACTER FORMAT "x(40)" 
     LABEL "Server user name" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1.

DEFINE VARIABLE dCreated AS DATE FORMAT "99/99/9999" 
     LABEL "Created" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE dModified AS DATE FORMAT "99/99/9999" 
     LABEL "Modified" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE iAuthMethod AS INTEGER FORMAT "9" INITIAL 1 
     LABEL "Auth.method" 
     VIEW-AS FILL-IN 
     SIZE 3.4 BY 1.

DEFINE VARIABLE iPort AS INTEGER FORMAT ">>>>>9" INITIAL 110 
     LABEL "Port" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1.

DEFINE VARIABLE iQueue# AS INTEGER FORMAT ">9" INITIAL 0 
     LABEL "Queue num" 
     VIEW-AS FILL-IN 
     SIZE 4.8 BY 1.

DEFINE RECTANGLE brwReiseSatsType
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 128 BY 8.81.

DEFINE RECTANGLE TbReiseSatsType
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 11 BY 1.19.

DEFINE VARIABLE bActive AS LOGICAL INITIAL no 
     LABEL "Active" 
     VIEW-AS TOGGLE-BOX
     SIZE 12.6 BY 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     cAccountName AT ROW 11.71 COL 22.2 COLON-ALIGNED
     cDisplayName AT ROW 12.71 COL 22.2 COLON-ALIGNED
     cEmailAddress AT ROW 13.71 COL 22.2 COLON-ALIGNED
     cReplyToAddress AT ROW 14.71 COL 22.2 COLON-ALIGNED
     cSignatureText AT ROW 15.95 COL 24.2 NO-LABEL
     cMailServer AT ROW 11.71 COL 85.2 COLON-ALIGNED
     cUserName AT ROW 12.71 COL 85.2 COLON-ALIGNED
     cPassword AT ROW 13.71 COL 85.2 COLON-ALIGNED PASSWORD-FIELD 
     iPort AT ROW 14.71 COL 85.2 COLON-ALIGNED
     iAuthMethod AT ROW 14.71 COL 123.8 COLON-ALIGNED
     cActionInProc AT ROW 18.95 COL 22.2 COLON-ALIGNED
     cActionInParam AT ROW 18.95 COL 86 COLON-ALIGNED
     cActionOutProc AT ROW 19.95 COL 22.2 COLON-ALIGNED
     cActionOutParam AT ROW 19.95 COL 86 COLON-ALIGNED
     cReadProc AT ROW 21.33 COL 22.2 COLON-ALIGNED
     cSendProc AT ROW 22.29 COL 22.2 COLON-ALIGNED
     iQueue# AT ROW 22.29 COL 86 COLON-ALIGNED
     dCreated AT ROW 23.52 COL 22.2 COLON-ALIGNED
     iJBoxCompanyId AT ROW 1.19 COL 45.6 COLON-ALIGNED
     cMerknadLabel AT ROW 16 COL 14.2 NO-LABEL
     cCreatedBy AT ROW 23.52 COL 42.2 COLON-ALIGNED
     dModified AT ROW 23.52 COL 65.4 COLON-ALIGNED
     cModifiedBy AT ROW 23.52 COL 86.2 COLON-ALIGNED
     bActive AT ROW 13.71 COL 116.4
     cUsage AT ROW 21.33 COL 86 COLON-ALIGNED HELP
          "blank=all,In,Out,Sms.."
     TbReiseSatsType AT ROW 1.24 COL 2
     brwReiseSatsType AT ROW 2.67 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 130.2 BY 23.95.


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
         TITLE              = "<Insert window title>"
         HEIGHT             = 23.91
         WIDTH              = 130.4
         MAX-HEIGHT         = 29.43
         MAX-WIDTH          = 150.4
         VIRTUAL-HEIGHT     = 29.43
         VIRTUAL-WIDTH      = 150.4
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
   FRAME-NAME Custom                                                    */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 23.95
       FRAME DEFAULT-FRAME:WIDTH            = 130.2.

/* SETTINGS FOR FILL-IN cMerknadLabel IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <Insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <Insert window title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME iJBoxCompanyId
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL iJBoxCompanyId C-Win
ON VALUE-CHANGED OF iJBoxCompanyId IN FRAME DEFAULT-FRAME /* CompanyId */
DO:
  IF NOT CAN-DO("new,modified",DYNAMIC-FUNCTION("getToolbarState",hToolbar)) THEN DO:
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"queryFilter","WHERE iJBoxCompanyId = " + iJBoxCompanyId:SCREEN-VALUE).
    RUN InvokeMethod(hBrowse,"OpenQuery").
  END.
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
  DYNAMIC-FUNCTION("SetCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
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
DYNAMIC-FUNCTION("setAttribute",hToolbar,"disabledEvents",
                 IF iJBoxCompanyId:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ? THEN "new,copy" ELSE "").
RUN SUPER.
cAccountName:READ-ONLY IN FRAME {&FRAME-NAME} = YES.
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
  DISPLAY cAccountName cDisplayName cEmailAddress cReplyToAddress cSignatureText 
          cMailServer cUserName cPassword iPort iAuthMethod cActionInProc 
          cActionInParam cActionOutProc cActionOutParam cReadProc cSendProc 
          iQueue# dCreated iJBoxCompanyId cMerknadLabel cCreatedBy dModified 
          cModifiedBy bActive cUsage 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE cAccountName cDisplayName cEmailAddress cReplyToAddress cSignatureText 
         cMailServer cUserName cPassword iPort iAuthMethod cActionInProc 
         cActionInParam cActionOutProc cActionOutParam cReadProc cSendProc 
         iQueue# dCreated iJBoxCompanyId cMerknadLabel cCreatedBy dModified 
         cModifiedBy bActive cUsage TbReiseSatsType brwReiseSatsType 
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
  Notes:       Refer to the <jukebox>\winsrc\samples for working examples for Sports2000
------------------------------------------------------------------------------*/
RUN enable_UI.

DYNAMIC-FUNCTION("CreateStatusBar",THIS-PROCEDURE:CURRENT-WINDOW,"",0,YES,?).

DO WITH FRAME {&FRAME-NAME}:
  ASSIGN iJBoxCompanyId:DELIMITER = "|"
         iJBoxCompanyId:LIST-ITEM-PAIRS = RIGHT-TRIM("Global|0|"
                                                   + DYNAMIC-FUNCTION("getFieldList",
                                                                      "JBoxCompany;cCompanyName;iJBoxCompanyId","WHERE true"),"|")
         iJBoxCompanyId:SCREEN-VALUE = "0"

         .

  hBrowse = DYNAMIC-FUNCTION("NewBrowse"
    ,brwReiseSatsType:HANDLE
    ,100
    ,""
    ,"JBoxEmailAccount"
    + ";cAccountName"
    + ";cDisplayName"
    + ";cEmailAddress"
    + ";cReplyToAddress"
    + ";cMailServer"
    + ";cUserName"
    + ";bActive"
    + ";iPort"
    + ";iAuthMethod"
    + ";cActionInParam"
    + ";cActionInProc"
    + ";cActionOutParam"
    + ";cActionOutProc"
    + ";cReadProc"
    + ";cSendProc"
    + ";!cPassword"
    + ";cUsage"
    + ";iQueue#"
    + ";cSignatureText"
    + ";dCreated"
    + ";cCreatedBy"
    + ";dModified"
    + ";cModifiedBy"
    ,"WHERE false"
    ,"").

  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap"
    ,hBrowse:QUERY
    ,FRAME {&FRAME-NAME}:HANDLE
    ,"cAccountName,cSignatureText,cActionInParam,cPassword,cUsage,iQueue#,cActionInProc,cActionOutParam,cActionOutProc,cDisplayName,cEmailAddress,cMailServer,cReadProc,cReplyToAddress,cSendProc,cUserName,iAuthMethod,iPort,bActive",""
    ,"cCreatedBy,cModifiedBy,dCreated,dModified",""
    ,"").

  DYNAMIC-FUNCTION("CreateObjectLink",hFieldMap,hBrowse).
  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"bufferExtraFields","iJBoxCompanyId").

  hToolbar = DYNAMIC-FUNCTION("NewToolBar"
    ,TbReiseSatsType:HANDLE
    ,"Fil"
    ,"new;Ny,copy;Kopier,undo;Angre,delete;Slett,save;Lagre,excel;Eksporter til E&xcel"
   + ",BrowseConfig;Column setup"
    ,"maxborder").

  DYNAMIC-FUNCTION("CreateObjectLink",hToolbar,hFieldMap).
  DYNAMIC-FUNCTION("CreateObjectLink",hToolbar,hBrowse).

  APPLY "value-changed" TO iJBoxCompanyId.
END.

DYNAMIC-FUNCTION("InitTranslation",THIS-PROCEDURE:CURRENT-WINDOW).

DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,"cSignatureText").
DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,500,200,0,0).

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
THIS-PROCEDURE:CURRENT-WINDOW:WINDOW-STATE = 3.
THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().

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
/* If the screen-value is ? this setting doesn't have effect so you can still update the record
   New/Copy is however disable (see DisplayRecord) */
DYNAMIC-FUNCTION("setAttribute",hFieldMap,"bufferExtraValues",iJBoxCompanyId:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
RUN SUPER.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

