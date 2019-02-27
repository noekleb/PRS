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

DEF VAR bOk             AS LOG    NO-UNDO.
DEF VAR ix              AS INT    NO-UNDO.
DEF VAR hBrowse         AS HANDLE NO-UNDO.
DEF VAR hQuery          AS HANDLE NO-UNDO.
DEF VAR hToolbar        AS HANDLE NO-UNDO.
DEF VAR hBuffer         AS HANDLE NO-UNDO.
DEF VAR hTimer          AS HANDLE NO-UNDO.
DEF VAR iTimerInterval  AS INT    NO-UNDO INIT 60000.
DEF VAR cCurrAccount    AS CHAR   NO-UNDO.
DEF VAR hEmailListener  AS HANDLE NO-UNDO.
DEF VAR httEmailBuffer  AS HANDLE NO-UNDO.
DEF VAR httAttachBuffer AS HANDLE NO-UNDO.
DEF VAR hdsEmail        AS HANDLE NO-UNDO.
DEF VAR iTotCount       AS INT    NO-UNDO.
DEF VAR iMailCount      AS INT    NO-UNDO.
DEF VAR hSimpleListener AS HANDLE NO-UNDO.
DEF VAR hTransRecord    AS HANDLE NO-UNDO.
DEF VAR iNumChecks      AS INT    NO-UNDO.

DEF TEMP-TABLE ttEmailAccount
    FIELD iJBoxEmailAccountId AS INT
    FIELD iMsgCount           AS INT
    .

DEF TEMP-TABLE ttInboxEntries NO-UNDO
    FIELD iJBoxEmailInboxId AS INT
    .
DEF VAR hbInboxEntries AS HANDLE NO-UNDO.
hbInboxEntries = BUFFER ttInboxEntries:HANDLE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS brwEmailAccount fiTimerInterval fiEmailCount ~
fiLogFile btnfiLogFile fiReportFreq 
&Scoped-Define DISPLAYED-OBJECTS fiTimerInterval fiEmailCount fiLogFile ~
fiReportFreq 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CreLog C-Win 
FUNCTION CreLog RETURNS LOGICAL
  ( INPUT icLogText AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LogQuit C-Win 
FUNCTION LogQuit RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnfiLogFile 
     IMAGE-UP FILE "bmp/folderopen.bmp":U
     LABEL "..." 
     SIZE 4 BY 1.

DEFINE VARIABLE fiEmailCount AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Email count" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE fiLogFile AS CHARACTER FORMAT "X(256)":U 
     LABEL "Logfile" 
     VIEW-AS FILL-IN 
     SIZE 67 BY 1 NO-UNDO.

DEFINE VARIABLE fiReportFreq AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 720 
     LABEL "Email activity report every (min)" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1 NO-UNDO.

DEFINE VARIABLE fiTimerInterval AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 30 
     LABEL "Freq (sec):" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE RECTANGLE brwEmailAccount
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 103 BY 10.95.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     fiTimerInterval AT ROW 1.48 COL 93.6 COLON-ALIGNED
     fiEmailCount AT ROW 14.57 COL 13 COLON-ALIGNED
     fiLogFile AT ROW 14.57 COL 33 COLON-ALIGNED
     btnfiLogFile AT ROW 14.57 COL 102 NO-TAB-STOP 
     fiReportFreq AT ROW 15.71 COL 33 COLON-ALIGNED
     brwEmailAccount AT ROW 3.14 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 106.6 BY 15.91.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Email import"
         HEIGHT             = 15.91
         WIDTH              = 106.6
         MAX-HEIGHT         = 23.52
         MAX-WIDTH          = 114.2
         VIRTUAL-HEIGHT     = 23.52
         VIRTUAL-WIDTH      = 114.2
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
       FRAME DEFAULT-FRAME:HEIGHT           = 15.91
       FRAME DEFAULT-FRAME:WIDTH            = 106.6.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Email import */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Email import */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnfiLogFile
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnfiLogFile C-Win
ON CHOOSE OF btnfiLogFile IN FRAME DEFAULT-FRAME /* ... */
DO:
  OS-COMMAND NO-WAIT VALUE("notepad " + fiLogFile:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiReportFreq
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiReportFreq C-Win
ON LEAVE OF fiReportFreq IN FRAME DEFAULT-FRAME /* Email activity report every (min) */
DO:
  IF SELF:MODIFIED THEN ASSIGN fiReportFreq.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiTimerInterval
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiTimerInterval C-Win
ON LEAVE OF fiTimerInterval IN FRAME DEFAULT-FRAME /* Freq (sec): */
DO:
  IF SELF:MODIFIED THEN DO:
    ASSIGN fiTimerInterval
           iTimerInterval = fiTimerInterval * 1000.
    DYNAMIC-FUNCTION("setInterval" IN hTimer,iTimerInterval).
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
  IF VALID-HANDLE(hTimer) THEN APPLY "close" TO hTimer.
  LogQuit().
  IF VALID-HANDLE(hEmailListener) THEN APPLY "close" TO hEmailListener.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CheckTimerEvent C-Win 
PROCEDURE CheckTimerEvent :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* IF TIME < 10000 THEN DO:            */
/*   APPLY "close" TO THIS-PROCEDURE.  */
/*   RETURN.                           */
/* END.                                */

IF NOT hBrowse:QUERY:IS-OPEN THEN DO: 
  RUN InvokeMethod(hBrowse,"OpenQuery").
  RETURN.
END.

IF NUM-ENTRIES(DYNAMIC-FUNCTION("getFieldList","JBoxEmailAccount;iJBoxEmailAccountId","WHERE bActive AND cUsage NE 'out'"),"|") NE hBrowse:QUERY:NUM-RESULTS THEN DO:
  RUN InvokeMethod(hBrowse,"OpenQuery").
  CreLog("Change in number of email accounts. Reopens query to invoke change").
  RETURN.
END.

IF NOT hBrowse:SELECT-NEXT-ROW() AND hBrowse:QUERY:IS-OPEN THEN DO:
  hBrowse:QUERY:GET-FIRST().
  hBrowse:QUERY:REPOSITION-TO-ROWID(hBuffer:ROWID).
  hBrowse:SELECT-FOCUSED-ROW().    
END. 

IF hBuffer:AVAIL THEN
  RUN InvokeMethod(hBrowse,"DisplayRecord").

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

IF hBuffer:AVAIL THEN DO:
  FIND FIRST ttEmailAccount
       WHERE ttEmailAccount.iJBoxEmailAccountId = hBuffer:BUFFER-FIELD("iJBoxEmailAccountId"):BUFFER-VALUE
       NO-ERROR.
  IF NOT AVAIL ttEmailAccount THEN DO:
    CREATE ttEmailAccount.  
    ttEmailAccount.iJBoxEmailAccountId = hBuffer:BUFFER-FIELD("iJBoxEmailAccountId"):BUFFER-VALUE.
  END.
    
  SESSION:SET-WAIT-STATE("General").
  RUN SwitchAccount(STRING(hBuffer:BUFFER-FIELD("iJBoxEmailAccountId"):BUFFER-VALUE)).
  SESSION:SET-WAIT-STATE("").
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
  DISPLAY fiTimerInterval fiEmailCount fiLogFile fiReportFreq 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE brwEmailAccount fiTimerInterval fiEmailCount fiLogFile btnfiLogFile 
         fiReportFreq 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getLastEmailCount C-Win 
PROCEDURE getLastEmailCount :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Check if the current email count has changed.
               If so the getEmail procedure in the listener should retrieve
               emails and the timer should also pause
------------------------------------------------------------------------------*/
DEF INPUT  PARAM iiCurrentCount   AS INT NO-UNDO.
DEF OUTPUT PARAM oiLastEmailCount AS INT NO-UNDO.

IF AVAIL ttEmailAccount THEN DO:

  ASSIGN iMailCount = iiCurrentCount
         fiEmailCount:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(iMailCount)
         .

  IF iiCurrentCount > ttEmailAccount.iMsgCount AND VALID-HANDLE(hTimer) THEN DO:
    RUN SuspendJBoxTimer IN hTimer (YES).
  END.

  ASSIGN oiLastEmailCount = ttEmailAccount.iMsgCount
         ttEmailAccount.iMsgCount = iMailCount
         .
END.

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
  iTimerInterval = MAX(10000,INTEGER(fiTimerInterval:SCREEN-VALUE) * 1000).

  fiLogFile:SCREEN-VALUE = SESSION:TEMP-DIR + "OpenPop.log".

  OUTPUT TO VALUE(fiLogFile:SCREEN-VALUE) APPEND.
  PUT UNFORMATTED SKIP(1) TODAY " " STRING(TIME,"hh:mm:ss") " *** Email listener started" SKIP.
  OUTPUT CLOSE.

  /* To be able to fetch id of logrecord after creation: */
  hTransRecord = DYNAMIC-FUNCTION("getTransRecordBufferHandle").

  hBrowse = DYNAMIC-FUNCTION("NewBrowse"
          ,brwEmailAccount:HANDLE
          ,100
          ,""
          ,"JBoxEmailAccount"
          + ";cAccountName"
          + ";cEmailAddress"
          + ";cUserName"
          + ";cMailServer"
          + ";iPort"
          + ";iAuthMethod"
          + ";cActionInProc"
                + ";cActionInParam"
          + ";!cPassword"
          + ";!iJBoxCompanyId"
          + ";!iJBoxEmailAccountId"
        + ",JBoxCompany"
          + ";cCompanyName|Company@1"
          ,"WHERE false"
         + ",FIRST JBoxCompany NO-LOCK OF JBoxEmailAccount"
          ,"").

  hBuffer = hBrowse:QUERY:GET-BUFFER-HANDLE(1).

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"baseQuery","WHERE bActive AND cUsage NE 'out'").
  
  &IF DEFINED(UIB_is_Running) = 0 &THEN
/*     IF SEARCH("controls.dll") NE ? AND                                          */
/*       (SEARCH("JBoxJLWTimer.w") NE ? OR SEARCH("JBoxJLWTimer.r") NE ?) THEN DO: */
/*         MESSAGE iTimerInterval                                                  */
/*             VIEW-AS ALERT-BOX INFO BUTTONS OK.                                  */
/*       RUN JBoxJLWtimer.w PERSIST SET hTimer ("CheckTimerEvent",iTimerInterval). */
/*     END.                                                                        */
/*     ELSE                                                                        */
     RUN JBoxOOTimer.p PERSIST SET hTimer ("CheckTimerEvent",iTimerInterval).
  &ENDIF
  
  RUN InvokeMethod(hBrowse,"OpenQuery").

END.


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NewEmailCount C-Win 
PROCEDURE NewEmailCount :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM iiMailCount AS INT NO-UNDO.

ASSIGN iMailCount = iiMailCount
       fiEmailCount:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(iMailCount)
       .
/* IF AVAIL ttEmailAccount THEN                                                  */
/*   ttEmailAccount.iMsgCount = iMailCount.                                      */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SwitchAccount C-Win 
PROCEDURE SwitchAccount :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icAccountId AS CHAR NO-UNDO.

DEF VAR cSysInfoMail       AS CHAR   NO-UNDO.
DEF VAR cSysInfoMailTo     AS CHAR   NO-UNDO.
DEF VAR cSubjectList       AS CHAR   NO-UNDO.
DEF VAR cProcessTrigger    AS CHAR   NO-UNDO.
DEF VAR cProcessId         AS CHAR   NO-UNDO.
DEF VAR cProcessSteps      AS CHAR   NO-UNDO.
DEF VAR ix                 AS INT    NO-UNDO.
DEF VAR cLogId             AS CHAR   NO-UNDO.
DEF VAR cReturn            AS CHAR   NO-UNDO.

IF (icAccountId = cCurrAccount AND hBrowse:QUERY:NUM-RESULTS NE 1) OR icAccountId = "" OR icAccountId = ? THEN RETURN.

cCurrAccount = icAccountId.

IF VALID-HANDLE(hEmailListener) THEN 
  APPLY "close" TO hEmailListener.

DYNAMIC-FUNCTION("setCompanyId",hBuffer:BUFFER-FIELD("iJBoxCompanyId"):BUFFER-VALUE).

RUN JBoxEmailListener.p PERSIST SET hEmailListener.
  
DYNAMIC-FUNCTION("setLogFile" IN hEmailListener,fiLogFile:SCREEN-VALUE IN FRAME {&FRAME-NAME}).

DYNAMIC-FUNCTION("setMailServerProperties" IN hEmailListener,
                 hBuffer:BUFFER-FIELD("cMailServer"):BUFFER-VALUE,
                 hBuffer:BUFFER-FIELD("iPort"):BUFFER-VALUE,
                 hBuffer:BUFFER-FIELD("cUserName"):BUFFER-VALUE,
                 hBuffer:BUFFER-FIELD("cPassword"):BUFFER-VALUE,
                 hBuffer:BUFFER-FIELD("iAuthMethod"):BUFFER-VALUE
                 ).
                 
DYNAMIC-FUNCTION("setActionProcsAndParams" IN hEmailListener,
                 hBuffer:BUFFER-FIELD("cActionInProc"):BUFFER-VALUE,
                 hBuffer:BUFFER-FIELD("cActionInParam"):BUFFER-VALUE
                 ).

DYNAMIC-FUNCTION("setSubjectList" IN hEmailListener, cSubjectList).
RUN InitializeObject IN hEmailListener.

IF VALID-HANDLE(hEmailListener) THEN DO:
  SUBSCRIBE TO "NewEmailCount" IN hEmailListener.
  httEmailBuffer  = DYNAMIC-FUNCTION("getTTEmailBuffer"  IN hEmailListener).
  httAttachBuffer = DYNAMIC-FUNCTION("getTTAttachBuffer" IN hEmailListener).

  DYNAMIC-FUNCTION("setCheckStoredExp" IN hEmailListener,
                   "JBoxEmailInbox","cFrom","dDateSent","iTimeSent","iJBoxEmailAccountId",icAccountId,YES).

  DYNAMIC-FUNCTION("setLoadBlobs" IN hEmailListener,YES).

  RUN getEmail IN hEmailListener.

  PROCESS EVENTS.

  IF iMailCount > 0 THEN DO:

    iTotCount = iTotCount + iMailCount.

    EMPTY TEMP-TABLE ttInboxEntries.
  
    hdsEmail = DYNAMIC-FUNCTION("createDataSet",
                     STRING(httEmailBuffer) + "," + STRING(httAttachBuffer) + "," + STRING(hbInboxEntries),
                     "iMsgId,iMsgId",
                     "iMsgId,cFileName"
                     ).
    
  
    IF VALID-HANDLE(hdsEmail) THEN DO:
      cProcessTrigger = DYNAMIC-FUNCTION("getFieldValues","JBoxProcessRel",
                                    "WHERE cContext = 'JBoxEmailAccount' AND cEntityId = '" + icAccountId + "'",
                                    "iJBoxProcessRelId,iJBoxProcessId").

      IF cProcessTrigger NE ? THEN DO:
        ASSIGN cProcessId      = ENTRY(2,cProcessTrigger,"|")
               cProcessTrigger = ENTRY(1,cProcessTrigger,"|")
               cProcessSteps   = DYNAMIC-FUNCTION("getFieldList","JBoxProcessStep;cProgramName;iJBoxProcessStepId;bLogProcessStep","WHERE iJBoxProcessId = " + cProcessId + " BY iSeq")
               .

        CreLog("Process trigger: " + cProcessTrigger + " Steps: " + cProcessSteps).
        DO ix = 1 TO NUM-ENTRIES(cProcessSteps,"|") BY 3:
          cLogId = "".
          CASE ENTRY(ix,cProcessSteps,"|"):
            WHEN "jbdoc_create_email_inbox.p" THEN DO:
              IF LOGICAL(ENTRY(ix + 2,cProcessSteps,"|")) THEN DO:
                DYNAMIC-FUNCTION("DoCreate","JBoxProcessStepLog","ignore",
                                 "iJBoxProcessStepId,cStepStatus,iCreTime",
                                 ENTRY(ix + 1,cProcessSteps,"|") + "|Started|" + STRING(TIME),
                                 YES).
                hTransRecord:FIND-FIRST("where true") NO-ERROR.
                IF hTransRecord:AVAIL THEN
                  cLogId = ENTRY(1,hTransRecord:BUFFER-FIELD("cReturnExtraValues"):BUFFER-VALUE,"|").
              END.

              cReturn = "".
              IF DYNAMIC-FUNCTION("runProcDs","jbdoc_create_email_inbox.p"
                                  ,icAccountId + "|" + fiLogFile:SCREEN-VALUE IN FRAME {&FRAME-NAME}
                                  ,hdsEmail) THEN 
                DYNAMIC-FUNCTION("getRunProcReturnDs",hdsEmail,NO,NO).  
              ELSE cReturn = DYNAMIC-FUNCTION("getTransactionMessage").

              IF cLogId NE "" THEN
                DYNAMIC-FUNCTION("DoUpdate","JBoxProcessStepLog","ignore",
                                 "iJBoxProcessStepId",cLogId
                                 ,"cStepStatus,cLogMessage",
                                 (IF cReturn = "" THEN "Complete|" ELSE "Failed|") + cReturn,
                                 YES).

              IF cReturn NE "" THEN DO:
                CreLog(cReturn).
                LEAVE.  
              END. 
            END.
            WHEN "jbadmin_send_new_email_alert.p" THEN DO:
              IF LOGICAL(ENTRY(ix + 2,cProcessSteps,"|")) THEN DO:
                DYNAMIC-FUNCTION("DoCreate","JBoxProcessStepLog","ignore",
                                 "iJBoxProcessStepId,cStepStatus,iCreTime",
                                 ENTRY(ix + 1,cProcessSteps,"|") + "|Started|" + STRING(TIME),
                                 YES).
                hTransRecord:FIND-FIRST("where true") NO-ERROR.
                IF hTransRecord:AVAIL THEN
                  cLogId = ENTRY(1,hTransRecord:BUFFER-FIELD("cReturnExtraValues"):BUFFER-VALUE,"|").
              END.

              cReturn = "".
              IF NOT DYNAMIC-FUNCTION("runProc","jbadmin_send_new_email_alert.p",
                                  "type|email received"
                                + ";processStep|" + ENTRY(ix + 1,cProcessSteps,"|")
                                + ";processTrigger|" + cProcessTrigger
                                  ,hbInboxEntries) THEN
                cReturn = DYNAMIC-FUNCTION("getTransactionMessage").

              IF cLogId NE "" THEN
                DYNAMIC-FUNCTION("DoUpdate","JBoxProcessStepLog","ignore",
                                 "iJBoxProcessStepId",cLogId
                                 ,"cStepStatus,cLogMessage",
                                 (IF cReturn = "" THEN "Complete|" ELSE "Failed|") + cReturn,
                                 YES).
                                                                     
              IF cReturn NE "" THEN DO:
                CreLog(cReturn).
                LEAVE.  
              END. 
            END.
          END CASE.
        END.
      END.
      IF cProcessSteps = "" THEN
        DYNAMIC-FUNCTION("runProcDs","jbdoc_create_email_inbox.p",icAccountId,hdsEmail).
  
      DELETE OBJECT hdsEmail.
    END.
  END.

  IF VALID-HANDLE(hEmailListener) THEN
    APPLY "close" TO hEmailListener.

  iNumChecks = iNumChecks + 1.

  IF fiReportFreq > 0 AND fiTimerInterval / 60 * iNumChecks > fiReportFreq THEN DO:
      
    ASSIGN iNumChecks = 0
           cSysInfoMail = DYNAMIC-FUNCTION("getFieldValues","JBoxEmailAccount",
                                           "WHERE iJBoxCompanyId = 0 AND cUsage = 'out' AND bActive","cMailServer,cUserName,cPassword,cEmailAddress")
           cSysInfoMailTo = DYNAMIC-FUNCTION("getFieldValues","JBoxGenCode",
                                             "WHERE cCodeType = 'SysinfoEmail' AND iJBoxCompanyId = 0","cCodeValue")
           .


    IF cSysInfoMail NE ? AND NUM-ENTRIES(cSysInfoMailTo,"@") = 2 THEN
      RUN JBoxSendBlatMail.p (
          cSysInfoMailTo,
          IF ENTRY(4,cSysInfoMail,"|") = "" THEN "sysinfo@ruben-sys.no" ELSE ENTRY(4,cSysInfoMail,"|"),
          "",
          "Ruben email listener alive",
          "Emails processed: " + STRING(iTotCount),
          "",
          ENTRY(1,cSysInfoMail,"|"),
          ENTRY(2,cSysInfoMail,"|"),
          ENTRY(3,cSysInfoMail,"|"),
          "",
          "",
          "",
          "",
          NO,
          NO,
          ?,
          OUTPUT bOK,
          OUTPUT cReturn
          ).

  END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CreLog C-Win 
FUNCTION CreLog RETURNS LOGICAL
  ( INPUT icLogText AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF fiLogFile:SCREEN-VALUE IN FRAME {&FRAME-NAME} NE "" THEN DO:
  OUTPUT TO VALUE(fiLogFile:SCREEN-VALUE) APPEND.
  PUT UNFORMATTED TODAY " " STRING(TIME,"hh:mm:ss") " " icLogText  SKIP.
  OUTPUT CLOSE.
  RETURN YES.
END.
  
RETURN NO.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LogQuit C-Win 
FUNCTION LogQuit RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR bLog AS LOG NO-UNDO.
IF fiLogFile:SCREEN-VALUE IN FRAME {&FRAME-NAME} NE "" THEN DO:
  OUTPUT TO VALUE(fiLogFile:SCREEN-VALUE) APPEND.
  bLog = YES.
END.

IF VALID-HANDLE(hEmailListener) THEN DO:
  bOk = DYNAMIC-FUNCTION("ReleaseWordHandle" IN hEmailListener).  
  IF bLog THEN
    PUT UNFORMATTED STRING(TODAY) " " STRING(TIME,"hh:mm:ss") 
        (IF bOk THEN " Word handle closed" ELSE " Failed to close Word") SKIP.
END.
IF bLog THEN DO:
  PUT UNFORMATTED STRING(TODAY) " " STRING(TIME,"hh:mm:ss") " Email import done. QUITs" SKIP.
  OUTPUT CLOSE.
END.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

