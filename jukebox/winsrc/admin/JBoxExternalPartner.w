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
DEF VAR hBrwReqType AS HANDLE NO-UNDO.
DEF VAR hBrwRequest AS HANDLE NO-UNDO.
DEF VAR hBuffReq    AS HANDLE NO-UNDO.

DEF TEMP-TABLE ttReport NO-UNDO
    FIELD cLine AS CHAR 
    .
DEF VAR hReportBuffer AS HANDLE NO-UNDO.
hReportBuffer = BUFFER ttReport:HANDLE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS brwExternalPartner tbExternalPartner ~
brwExternalRequest brwRequestType cExternalPartnerName ~
cExtPartnerPassPhrase dCreated cCreatedBy cModifiedBy dModified 
&Scoped-Define DISPLAYED-OBJECTS cExternalPartnerName cExtPartnerPassPhrase ~
dCreated cCreatedBy cModifiedBy dModified 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE cExtPartnerPassPhrase AS CHARACTER FORMAT "x(20)" 
     LABEL "Pass.phrase" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 42 BY 1.

DEFINE VARIABLE cCreatedBy AS CHARACTER FORMAT "X(8)" 
     LABEL "By" 
     VIEW-AS FILL-IN 
     SIZE 13.6 BY 1.

DEFINE VARIABLE cExternalPartnerName AS CHARACTER FORMAT "x(40)" 
     LABEL "External partner" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1.

DEFINE VARIABLE cModifiedBy AS CHARACTER FORMAT "X(8)" 
     LABEL "By" 
     VIEW-AS FILL-IN 
     SIZE 13.6 BY 1.

DEFINE VARIABLE dCreated AS DATE FORMAT "99/99/9999" 
     LABEL "Created" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE dModified AS DATE FORMAT "99/99/9999" 
     LABEL "Modified" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE RECTANGLE brwExternalPartner
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 68 BY 12.38.

DEFINE RECTANGLE brwExternalRequest
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 132 BY 8.33.

DEFINE RECTANGLE brwRequestType
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 63 BY 6.91.

DEFINE RECTANGLE tbExternalPartner
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 18 BY .91.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     cExternalPartnerName AT ROW 2.81 COL 88 COLON-ALIGNED
     cExtPartnerPassPhrase AT ROW 4.1 COL 88 COLON-ALIGNED WIDGET-ID 24
     dCreated AT ROW 5.43 COL 88 COLON-ALIGNED
     cCreatedBy AT ROW 5.43 COL 109.8 COLON-ALIGNED
     cModifiedBy AT ROW 6.48 COL 110 COLON-ALIGNED
     dModified AT ROW 6.52 COL 88 COLON-ALIGNED
     brwExternalPartner AT ROW 2.67 COL 2 WIDGET-ID 2
     tbExternalPartner AT ROW 1.48 COL 2 WIDGET-ID 4
     brwExternalRequest AT ROW 15.52 COL 2 WIDGET-ID 6
     brwRequestType AT ROW 8.14 COL 71 WIDGET-ID 22
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 134.2 BY 23.24 WIDGET-ID 100.


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
         HEIGHT             = 23.24
         WIDTH              = 134.2
         MAX-HEIGHT         = 23.52
         MAX-WIDTH          = 139.6
         VIRTUAL-HEIGHT     = 23.52
         VIRTUAL-WIDTH      = 139.6
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
       FRAME DEFAULT-FRAME:HEIGHT           = 23.24
       FRAME DEFAULT-FRAME:WIDTH            = 134.2.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DefaultActionBrowse C-Win 
PROCEDURE DefaultActionBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF DYNAMIC-FUNCTION("getCurrentObject") = hBrwRequest THEN
  RUN ViewResultRecord.
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
  DISPLAY cExternalPartnerName cExtPartnerPassPhrase dCreated cCreatedBy 
          cModifiedBy dModified 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE brwExternalPartner tbExternalPartner brwExternalRequest brwRequestType 
         cExternalPartnerName cExtPartnerPassPhrase dCreated cCreatedBy 
         cModifiedBy dModified 
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
  ASSIGN cExtPartnerPassPhrase:DELIMITER = "|"
         cExtPartnerPassPhrase:LIST-ITEM-PAIRS = RIGHT-TRIM("None|None|"
                                               + DYNAMIC-FUNCTION("getFieldList",
                                                                  "JBoxPassPhraseValidation;cDescription;cPassPhraseResolver","WHERE true")
                                                            ,"|")
         .

  hBrowse = DYNAMIC-FUNCTION("NewBrowse"
          ,brwExternalPartner:HANDLE
          ,100
          ,""
          ,"JBoxExternalPartner"
          + ";cExternalPartnerName"
          + ";cExtPartnerPassPhrase"
          + ";dCreated|Created"
          + ";cCreatedBy|Cre.by"
          + ";dModified|Modified"
          + ";cModifiedBy|Mod.by"
          + ";!iJBoxExternalPartnerId"
          ,"WHERE false"
          ,"").

  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap"
          ,hBrowse:QUERY
          ,FRAME {&FRAME-NAME}:HANDLE
          ,"cExternalPartnerName,cExtPartnerPassPhrase",""
          ,"cCreatedBy,cModifiedBy,dCreated,dModified",""
          ,"").

  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hFieldMap).

  hToolbar = DYNAMIC-FUNCTION("NewToolBar"
          ,tbExternalPartner:HANDLE
          ,"Fil"
          ,"new,copy,undo,delete,save,excel"
        + ",SelectReqType;Select request types"
          ,"maxborder").

  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hToolbar).
  DYNAMIC-FUNCTION("CreateObjectLink",hFieldMap,hToolbar).

  hBrwReqType = DYNAMIC-FUNCTION("NewBrowse"
          ,brwRequestType:HANDLE
          ,100
          ,""
          ,"JBoxExternalPartnerRequestType"
          + ";!cPartnerRequestTypePassPhrase"
          + ";!iJBoxExternalPartnerId"
        + ",JBoxExternalRequestType"
          + ";cExternalRequestType"
          + ";!cRequestTypePassPhrase"
          + ";cProgram"
          ,"WHERE false"
         + ",FIRST JBoxExternalRequestType NO-LOCK OF JBoxExternalPartnerRequestType"
          ,"").

  DYNAMIC-FUNCTION("CreateParentLink",hBrwReqType,hBrowse,"iJBoxExternalPartnerId").


  hBrwRequest = DYNAMIC-FUNCTION("NewBrowse"
          ,brwExternalRequest:HANDLE
          ,100
          ,""
          ,"JBoxExternalRequest"
          + ";dtRequest"
          + ";cRequestInputParam"
          + ";cPassPhrase"
          + ";bRequestOk"
          + ";bResultOk"
          + ";cRequestResult"
        + ",JBoxExternalRequestType"
          + ";cExternalRequestType"
          + ";!cProgram"
          ,"WHERE false"
         + ",FIRST JBoxExternalRequestType NO-LOCK OF JBoxExternalRequest OUTER-JOIN"
          ,"").

  hBuffReq = hBrwRequest:QUERY:GET-BUFFER-HANDLE(1).

  DYNAMIC-FUNCTION("NewMenuBand",hBrwRequest
          ,"MultiSortBrowse;Sort on multiple columns,Filter,Excel"
         + ",ViewResult;View result"
          ,"").

  DYNAMIC-FUNCTION("setSortString",hBrwRequest,"dtRequest;desc").
  DYNAMIC-FUNCTION("CreateParentLink",hBrwRequest,hBrowse,"iJBoxExternalPartnerId").

  RUN InvokeMethod(hBrowse,"OpenQuery").

END.

DYNAMIC-FUNCTION("setResizeXGroup", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,50,
                 "brwExternalPartner,brwRequestType," + hBrowse:NAME + "," + hBrwReqType:NAME).
DYNAMIC-FUNCTION("setMoveXGroup", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,50,
                 "brwRequestType,cExtPartnerPassPhrase,cCreatedBy,cExternalPartnerName,cModifiedBy,dCreated,dModified,"
               + hBrwReqType:NAME
                 ).

DYNAMIC-FUNCTION("setResizeYGroup", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,50,
                 "brwExternalPartner,brwRequestType,brwExternalRequest," + hBrowse:NAME + "," + hBrwReqType:NAME + "," + hBrwRequest:NAME).
DYNAMIC-FUNCTION("setMoveYGroup", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,50,
                 "brwExternalRequest," + hBrwRequest:NAME).

DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,200,200,0,0).

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

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SelectReqTypeRecord C-Win 
PROCEDURE SelectReqTypeRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cRowIdList  AS CHAR NO-UNDO.
DEF VAR cIdList     AS CHAR NO-UNDO.  
DEF VAR bOk         AS LOG  NO-UNDO.

/* Uncomment and modify to fetch pre-selected rows from database: */
cRowIdList = DYNAMIC-FUNCTION("getRowIdList",
                              "JBoxExternalPartnerRequestType,JBoxExternalRequestType",       /* Buffer(list) for query */
                              "",              /* Name of buffer to fetch rowid from. Blank: Last buffer in bufferlist) */
                              "WHERE iJBoxExternalPartnerId = " + STRING(hFieldMap:BUFFER-FIELD("iJBoxExternalPartnerId"):BUFFER-VALUE)
                            + ",FIRST JBoxExternalRequestType NO-LOCK OF JBoxExternalPartnerRequestType"
                              ).

THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
RUN JBoxSelector.w (THIS-PROCEDURE,0,
                    "JBoxExternalRequestType"      
                    + ";cExternalRequestType"
                        + ";cProgram"
                        + ";!iJBoxExternalRequestTypeId"
                        + ";cRequestTypePassPhrase"
                    ,"where true",
                    INPUT-OUTPUT cRowIdList,
                    "iJBoxExternalRequestTypeId", /* Primary key */
                    INPUT-OUTPUT cIdList,
                    "","",
                    OUTPUT bOK).
THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

IF bOk THEN DO:
  IF NOT DYNAMIC-FUNCTION("runProc","jbservice_edit_partner_request_type.p",
                          STRING(hFieldMap:BUFFER-FIELD("iJBoxExternalPartnerId"):BUFFER-VALUE) + ";" + cIdList
                         ,?) THEN
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
  ELSE RUN InvokeMethod(hBrwReqType,"OpenQuery").
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ViewResultRecord C-Win 
PROCEDURE ViewResultRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cFileName AS CHAR   NO-UNDO.
DEF VAR cProcName AS CHAR   NO-UNDO.
DEF VAR hQuery    AS HANDLE NO-UNDO.

hReportBuffer:EMPTY-TEMP-TABLE().
IF DYNAMIC-FUNCTION("getMyTempTable","jbservice_test_ws.p",
                    hBuffReq::cProgram + ";" + hBuffReq::cRequestInputParam,hReportBuffer) THEN DO:

  cFileName = DYNAMIC-FUNCTION("getUniqueFileName") + ".txt".       

  FIND FIRST ttReport NO-ERROR.

  CREATE QUERY hQuery.
  hQuery:SET-BUFFERS(hReportBuffer).
  hQuery:QUERY-PREPARE("FOR EACH " + hReportBuffer:NAME).
  hQuery:QUERY-OPEN().
  hQuery:GET-FIRST().
  OUTPUT TO VALUE(cFileName).
  REPEAT WHILE NOT hQuery:QUERY-OFF-END:
    PUT UNFORMATTED ttReport.cLine SKIP.
    hQuery:GET-NEXT().
  END.
  OUTPUT CLOSE.

  DELETE OBJECT hQuery.

  DYNAMIC-FUNCTION("setWebDoc","",cFileName).

END.
ELSE DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

