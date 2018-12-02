&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
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

DEF VAR bOK           AS LOG NO-UNDO.
DEF VAR ix            AS INT NO-UNDO.
                      
DEF VAR hBrowse       AS HANDLE NO-UNDO.
DEF VAR hFieldMap     AS HANDLE NO-UNDO.
DEF VAR hUpdTB        AS HANDLE NO-UNDO.
DEF VAR hWinTB        AS HANDLE NO-UNDO.
DEF VAR hBrowseUser   AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btndStartBroadCast rectUpdTB rectWinTB ~
btndEndBroadCast rectBrw rectBrwUser dStartBroadCast iStartBroadCastTime ~
dEndBroadCast iEndBroadCastTime iJBoxCompanyId bAllUsers cBroadCastURL ~
cBroadCastMessage 
&Scoped-Define DISPLAYED-OBJECTS dStartBroadCast iStartBroadCastTime ~
dEndBroadCast iEndBroadCastTime iJBoxCompanyId bAllUsers cBroadCastURL ~
cBroadCastMessage 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btndEndBroadCast 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 3.8 BY .81.

DEFINE BUTTON btndStartBroadCast 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 3.8 BY .81.

DEFINE VARIABLE iEndBroadCastTime AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 10.6 BY 1 NO-UNDO.

DEFINE VARIABLE iJBoxCompanyId AS CHARACTER FORMAT "X(256)":U 
     LABEL "Publish to company" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE iStartBroadCastTime AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 10.6 BY 1 NO-UNDO.

DEFINE VARIABLE cBroadCastMessage AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 70 BY 4.86 NO-UNDO.

DEFINE VARIABLE cBroadCastURL AS CHARACTER FORMAT "x(60)" 
     LABEL "Document" 
     VIEW-AS FILL-IN 
     SIZE 65 BY 1 TOOLTIP "Optional: Either a web-address or a document (drag-drop document here)" DROP-TARGET.

DEFINE VARIABLE dEndBroadCast AS DATE FORMAT "99/99/99" 
     LABEL "End date" 
     VIEW-AS FILL-IN 
     SIZE 12.4 BY 1.

DEFINE VARIABLE dStartBroadCast AS DATE FORMAT "99/99/99" 
     LABEL "Start date" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1.

DEFINE RECTANGLE rectBrw
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 50 BY 20.48.

DEFINE RECTANGLE rectBrwUser
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 77 BY 11.19.

DEFINE RECTANGLE rectUpdTB
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 18.2 BY 1.

DEFINE RECTANGLE rectWinTB
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 7 BY 1.

DEFINE VARIABLE bAllUsers AS LOGICAL INITIAL no 
     LABEL "Publish to all users" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81 NO-UNDO.

DEFINE BUTTON btnSplitBarX 
     IMAGE-UP FILE "bmp/tableft.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Button 1" 
     SIZE 2 BY 20.24.

DEFINE BUTTON btnSplitBarY  NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 77 BY .67.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btndStartBroadCast AT ROW 2.76 COL 79
     btndEndBroadCast AT ROW 2.76 COL 116.6
     dStartBroadCast AT ROW 2.67 COL 64 COLON-ALIGNED
     iStartBroadCastTime AT ROW 2.67 COL 80.8 COLON-ALIGNED NO-LABEL
     dEndBroadCast AT ROW 2.67 COL 102 COLON-ALIGNED
     iEndBroadCastTime AT ROW 2.67 COL 118.4 COLON-ALIGNED NO-LABEL
     iJBoxCompanyId AT ROW 3.76 COL 105 COLON-ALIGNED
     bAllUsers AT ROW 3.86 COL 66
     cBroadCastURL AT ROW 4.86 COL 64 COLON-ALIGNED HELP
          "Read more.."
     cBroadCastMessage AT ROW 6 COL 61 NO-LABEL
     rectUpdTB AT ROW 1.19 COL 1.8
     rectWinTB AT ROW 1.19 COL 123.8
     rectBrw AT ROW 2.43 COL 2
     rectBrwUser AT ROW 11.48 COL 54
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 130.8 BY 21.91.

DEFINE FRAME frmSplitBarY
     btnSplitBarY AT ROW 2.71 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER 
         THREE-D 
         AT COL 54 ROW 9.1
         SIZE 77 BY 6.19.

DEFINE FRAME frmSplitBarX
     btnSplitBarX AT ROW 1 COL 13
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 40 ROW 2.43
         SCROLLABLE SIZE 23 BY 20.48.


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
         TITLE              = "<insert window title>"
         HEIGHT             = 21.91
         WIDTH              = 130.8
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
ASSIGN FRAME frmSplitBarX:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME frmSplitBarY:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 21.91
       FRAME DEFAULT-FRAME:WIDTH            = 130.8.

/* SETTINGS FOR FRAME frmSplitBarX
                                                                        */
ASSIGN 
       FRAME frmSplitBarX:HEIGHT           = 20.48
       FRAME frmSplitBarX:WIDTH            = 23.

ASSIGN 
       btnSplitBarX:MOVABLE IN FRAME frmSplitBarX          = TRUE.

/* SETTINGS FOR FRAME frmSplitBarY
   UNDERLINE                                                            */
ASSIGN 
       btnSplitBarY:MOVABLE IN FRAME frmSplitBarY          = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME frmSplitBarX
/* Query rebuild information for FRAME frmSplitBarX
     _Query            is NOT OPENED
*/  /* FRAME frmSplitBarX */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME frmSplitBarY
/* Query rebuild information for FRAME frmSplitBarY
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME frmSplitBarY */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <insert window title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btndEndBroadCast
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btndEndBroadCast C-Win
ON CHOOSE OF btndEndBroadCast IN FRAME DEFAULT-FRAME /* ... */
DO:
  RUN Cal.w (dEndBroadCast:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btndStartBroadCast
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btndStartBroadCast C-Win
ON CHOOSE OF btndStartBroadCast IN FRAME DEFAULT-FRAME /* ... */
DO:
  RUN Cal.w (dStartBroadCast:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmSplitBarX
&Scoped-define SELF-NAME btnSplitBarX
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSplitBarX C-Win
ON END-MOVE OF btnSplitBarX IN FRAME frmSplitBarX /* Button 1 */
DO:
  DYNAMIC-FUNCTION("setSplitBarX",THIS-PROCEDURE:CURRENT-WINDOW,btnSplitBarX:HANDLE IN FRAME frmSplitBarX,NO).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmSplitBarY
&Scoped-define SELF-NAME btnSplitBarY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSplitBarY C-Win
ON END-MOVE OF btnSplitBarY IN FRAME frmSplitBarY
DO:
  DYNAMIC-FUNCTION("setSplitBarY",THIS-PROCEDURE:CURRENT-WINDOW,btnSplitBarY:HANDLE IN FRAME frmSplitBarY,NO).
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
  RUN disable_UI.
  PUBLISH "InvalidateHandle" (THIS-PROCEDURE).
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

ON 'drop-file-notify':U OF cBroadCastURL DO:
  bOK = NO.
  IF cBroadCastURL:SENSITIVE THEN DO:
    DO ix = 1 TO cBroadCastURL:NUM-DROPPED-FILES:
      IF DYNAMIC-FUNCTION("LoadDocs",cBroadCastURL:GET-DROPPED-FILE(ix),
                       "JBoxBroadCastMessage",
                       STRING(hFieldMap:BUFFER-FIELD("iJBoxBroadCastMessageId"):BUFFER-VALUE),
                       hFieldMap:BUFFER-FIELD("cBroadCastMessage"):BUFFER-VALUE) THEN
        bOK = YES.
    END.
    IF bOK THEN DO: 
      DYNAMIC-FUNCTION("refreshRowids",hBrowse,hFieldMap:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).
      APPLY "value-changed" TO hBrowse.
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AddUserRecord C-Win 
PROCEDURE AddUserRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cUserRowIdList  AS CHAR NO-UNDO.
DEF VAR cUserIdList     AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  cUserRowIdList = DYNAMIC-FUNCTION("getRowidList","JBoxUserBroadCast,JBoxUser","",
                                    "WHERE iJBoxBroadCastMessageId = " + STRING(hFieldMap:BUFFER-FIELD("iJBoxBroadCastMessageId"):BUFFER-VALUE)
                                  + ",FIRST JBoxUser NO-LOCK OF JBoxUserBroadCast").
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "JBoxUser;cJBoxUserId|UserId;cUserName|Name"
                    + ";+UserCompanies|CHARACTER|x(256)|jbserv_usercompanies.p(ROWID"
                        + (IF iJBoxCompanyId:SCREEN-VALUE NE ? THEN iJBoxCompanyId:SCREEN-VALUE ELSE "")
                        + ")|Companies",
                      "WHERE true",
                      INPUT-OUTPUT cUserRowIdList,
                      "cJBoxUserId",
                      INPUT-OUTPUT cUserIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.
  
  IF bOk THEN DO:
    IF NOT DYNAMIC-FUNCTION("runproc","jbserv_edituserbroadcast.p",
                            STRING(hFieldMap:BUFFER-FIELD("iJBoxBroadCastMessageId"):BUFFER-VALUE) + ";" + cUserIdList,?) THEN
      DYNAMIC-FUNCTION("DoMessage",0,1,DYNAMIC-FUNCTION("getTransactionMessage"),"Error","").
    ELSE 
      APPLY "value-changed" TO hBrowse.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplayRecord C-Win 
PROCEDURE DisplayRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN SUPER.

IF hFieldMap:AVAIL THEN DO:
  IF hFieldMap:BUFFER-FIELD("iJBoxDocumentId"):BUFFER-VALUE NE 0 THEN
    cBroadCastURL:SENSITIVE IN FRAME {&FRAME-NAME} = FALSE.
  ELSE
    cBroadCastURL:SENSITIVE IN FRAME {&FRAME-NAME} = TRUE.
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
  DISPLAY dStartBroadCast iStartBroadCastTime dEndBroadCast iEndBroadCastTime 
          iJBoxCompanyId bAllUsers cBroadCastURL cBroadCastMessage 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btndStartBroadCast rectUpdTB rectWinTB btndEndBroadCast rectBrw 
         rectBrwUser dStartBroadCast iStartBroadCastTime dEndBroadCast 
         iEndBroadCastTime iJBoxCompanyId bAllUsers cBroadCastURL 
         cBroadCastMessage 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  ENABLE btnSplitBarX 
      WITH FRAME frmSplitBarX IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frmSplitBarX}
  ENABLE btnSplitBarY 
      WITH FRAME frmSplitBarY IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frmSplitBarY}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExtraFlatViewRecord C-Win 
PROCEDURE ExtraFlatViewRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihFlatView   AS HANDLE NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO INIT TRUE.

DYNAMIC-FUNCTION("setUseLocalData" IN ihFlatView,TRUE).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getDocListContext C-Win 
PROCEDURE getDocListContext :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF OUTPUT PARAM ocEntityId       AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocContext        AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocDocDescription AS CHAR NO-UNDO.

ASSIGN ocEntityId       = STRING(hFieldMap:BUFFER-FIELD("iJBoxBroadCastMessageId"):BUFFER-VALUE)
       ocContext        = "JBoxBroadCastMessage"
       ocDocDescription = hFieldMap:BUFFER-FIELD("cBroadCastMessage"):BUFFER-VALUE.
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
DEF VAR cTime AS CHAR NO-UNDO.
  
RUN enable_UI.

DO WITH FRAME {&FRAME-NAME}:
  DO ix = 0 TO 23:
    cTime = cTime + STRING(ix,"99") + ":00" + "|" + STRING(ix * 60 * 60) + "|".
  END.
  ASSIGN iStartBroadCastTime:DELIMITER       = "|"
         iEndBroadCastTime:DELIMITER         = "|"
         iStartBroadCastTime:LIST-ITEM-PAIRS = TRIM(cTime,"|")
         iEndBroadCastTime:LIST-ITEM-PAIRS   = iStartBroadCastTime:LIST-ITEM-PAIRS
         iJBoxCompanyId:DELIMITER            = "|"
         iJBoxCompanyId:LIST-ITEM-PAIRS      = RIGHT-TRIM("|0|" + DYNAMIC-FUNCTION("getFieldList","JBoxCompany;cCompanyName;iJBoxCompanyId","where true"),"|")
         .


  hBrowse = DYNAMIC-FUNCTION("NewBrowse",
                   rectBrw:HANDLE,
                   100,
                   "",
                   "JBoxBroadCastMessage"
                   + ";dStartBroadCast"
                   + ";+cBroadCastStartTime|CHARACTER|x(5)|jbserv_int_to_hhmm_time.p(iStartBroadCastTime)|Start time"
                   + ";dEndBroadCast"
                   + ";+cBroadCastEndTime|CHARACTER|x(5)|jbserv_int_to_hhmm_time.p(iEndBroadCastTime)|End time"
                   + ";cBroadCastMessage"
                   + ";cBroadCastURL"
                   + ";!iEndBroadCastTime;!iStartBroadCastTime;!iJBoxBroadCastMessageId;!iJBoxCompanyId;!bAllUsers"
                 + ",JBoxDocRel;!iJBoxDocumentId;!cEntityId;!cContext"
                  ,"WHERE true"
                + ",FIRST JBoxDocRel NO-LOCK WHERE cContext = 'JBoxBroadCastMessage' AND JBoxDocRel.cEntityId = STRING(JBoxBroadCastMessage.iJBoxBroadCastMessageId) OUTER-JOIN"
                   ,"sort|dStartBroadCast DESC").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"flatviewjointype","outer-join").

  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap",
                   hBrowse:QUERY,
                   FRAME {&FRAME-NAME}:HANDLE,
                   "dStartBroadCast,iStartBroadCastTime,dEndBroadCast,iEndBroadCastTime,cBroadCastMessage,cBroadCastURL,bAllUsers,iJBoxCompanyId","",
                   "","",
                   "btndStartBroadCast,btndEndBroadCast").

  hBrowseUser = DYNAMIC-FUNCTION("NewBrowse",
                   rectBrwUser:HANDLE,
                   100,
                   "",
                   "JBoxUserBroadcast"
                   + ";cJBoxUserId|Userid"
                   + ";dReceived"
                   + ";iReceived"
                   + ";!iJBoxBroadCastMessageId"
                 + ",JBoxUser"
                   + ";cUserName|Name"
                  ,"WHERE false"
                 + ",FIRST JBoxUser NO-LOCK OF JBoxUserBroadcast",
                   "").
  hBrowseUser:MOVE-COLUMN(4,2).

  hUpdTB = DYNAMIC-FUNCTION("NewToolbar",
                   rectUpdTB:HANDLE,
                   "File",
                   "new,copy,undo,delete,save" 
                   + ",flatview,excel"
                   + ",ViewDocList;View documents"
                   + ",AddUser;Recipients",
                   "maxborder").
  hWinTB = DYNAMIC-FUNCTION("NewToolbar",
                   rectWinTB:HANDLE,
                   "File",
                   "close;Exit",
                   "enable,right").

  DYNAMIC-FUNCTION("CreateObjectLink",hUpdTB,hBrowse).
  DYNAMIC-FUNCTION("CreateObjectLink",hUpdTB,hFieldMap).
  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hFieldMap).
  DYNAMIC-FUNCTION("CreateParentLink",hBrowseUser,hBrowse,"iJBoxBroadCastMessageId").

  DYNAMIC-FUNCTION("setSplitBarX",THIS-PROCEDURE:CURRENT-WINDOW,btnSplitBarX:HANDLE IN FRAME frmSplitBarX,NO).
  DYNAMIC-FUNCTION("setFollowSplitBarX",THIS-PROCEDURE:CURRENT-WINDOW,btnSplitBarX:HANDLE IN FRAME frmSplitBarX,
                    STRING(hBrowse) + "," +
                    STRING(rectBrw:HANDLE) + "," +
                    STRING(cBroadCastMessage:HANDLE) + "," +
                    STRING(rectBrwUser:HANDLE) + "," +
                    STRING(hBrowseUser) + "," +
                    STRING(FRAME frmSplitBarY:HANDLE)
                   ).

  DYNAMIC-FUNCTION("setSplitBarY",THIS-PROCEDURE:CURRENT-WINDOW,btnSplitBarY:HANDLE IN FRAME frmSplitBarY,NO).
  DYNAMIC-FUNCTION("setFollowSplitBarY",THIS-PROCEDURE:CURRENT-WINDOW,btnSplitBarY:HANDLE IN FRAME frmSplitBarY,
                    STRING(cBroadCastMessage:HANDLE) + "," +
                    STRING(rectBrwUser:HANDLE) + "," +
                    STRING(hBrowseUser) 
                   ).

  DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectUpdTB,cBroadCastMessage").
  DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, hBrowse:NAME + ",rectBrw").
  DYNAMIC-FUNCTION("setAddResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "cRessursGruppe").
  DYNAMIC-FUNCTION("setOrgWinSize", THIS-PROCEDURE:CURRENT-WINDOW,250,150,0,200).

  APPLY "value-changed" TO hBrowse.
  THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.

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
THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = NO.
THIS-PROCEDURE:CURRENT-WINDOW:WINDOW-STATE = 3.
THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
APPLY "entry" TO hBrowse.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ViewDocListRecord C-Win 
PROCEDURE ViewDocListRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
RUN JBoxViewDocList.w.
THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

