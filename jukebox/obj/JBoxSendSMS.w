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
&IF DEFINED(UIB_is_Running) NE 0 &THEN
  DEF VAR icMobileNumList    AS CHAR NO-UNDO INIT "9092 3875".
  DEF VAR icReplyTo          AS CHAR NO-UNDO.
  DEF VAR icSubject          AS CHAR NO-UNDO INIT "Emne".
  DEF VAR icMessage          AS CHAR NO-UNDO INIT "Ny test fra Skotex".
  DEF VAR icEventType        AS CHAR NO-UNDO.
  DEF VAR icEventExtraText   AS CHAR NO-UNDO.
  DEF VAR icContextRowidList AS CHAR NO-UNDO.
  DEF VAR icContextKeyFields AS CHAR NO-UNDO.
  DEF VAR obOk               AS LOG  NO-UNDO.
  DEF VAR ocMessage          AS CHAR NO-UNDO.
&ELSE
  DEF INPUT  PARAM icMobileNumList    AS CHAR NO-UNDO.
  DEF INPUT  PARAM icReplyTo          AS CHAR NO-UNDO.
  DEF INPUT  PARAM icSubject          AS CHAR NO-UNDO.
  DEF INPUT  PARAM icMessage          AS CHAR NO-UNDO.
  DEF INPUT  PARAM icEventType        AS CHAR NO-UNDO. /* If blank no event is created */
  DEF INPUT  PARAM icEventExtraText   AS CHAR NO-UNDO. /* Event text will be sms subject */
  DEF INPUT  PARAM icContextRowidList AS CHAR NO-UNDO. /* Link info: <table>;<rowid>,<table>;<rowid>,<table>.. */
                                                      /* If used the list must correspond with icRecipientList */
  DEF INPUT PARAM icContextKeyFields  AS CHAR NO-UNDO. /* Key fields for event link: <table>;<key fields>,<table>;<Key.. */
  DEF OUTPUT PARAM obOk               AS LOG  NO-UNDO.
  DEF OUTPUT PARAM ocMessage          AS CHAR NO-UNDO.
&ENDIF

/* Local Variable Definitions ---                                       */

DEF VAR bOk         AS LOG    NO-UNDO.
DEF VAR ix          AS INT    NO-UNDO.
DEF VAR hBrowse     AS HANDLE NO-UNDO.
DEF VAR hQuery      AS HANDLE NO-UNDO.
DEF VAR hToolbar    AS HANDLE NO-UNDO.
DEF VAR hFieldMap   AS HANDLE NO-UNDO.
DEF VAR hParent     AS HANDLE NO-UNDO.

DEF VAR cCompanyArchiveEmail  AS CHAR   NO-UNDO.
DEF VAR cCompanyMailServer    AS CHAR   NO-UNDO.
DEF VAR cCompanySendFromEmail AS CHAR   NO-UNDO.
DEF VAR cCompanyReplyToEmail  AS CHAR   NO-UNDO.
DEF VAR cEmailUserName        AS CHAR   NO-UNDO.
DEF VAR cAuthorize       AS CHAR NO-UNDO.
DEF VAR cAuthType        AS CHAR NO-UNDO.   
DEF VAR cMailPwd         AS CHAR NO-UNDO.
DEF VAR cMailProgram     AS CHAR NO-UNDO INIT "blat".
DEF VAR cSmsDomain       AS CHAR NO-UNDO.
DEF VAR cSMSReplyTo      AS CHAR NO-UNDO.
DEF VAR cSMSProvider     AS CHAR NO-UNDO.

DEF TEMP-TABLE ttEmail
    FIELD cSendAs         AS CHARACTER
    FIELD cEmail          AS CHARACTER
    FIELD cAttachments    AS CHARACTER
    FIELD RowIdent1       AS CHARACTER
    FIELD cSourceTable    AS CHARACTER
    FIELD cKeyFieldList   AS CHARACTER
    FIELD cEventText      AS CHARACTER
    FIELD cEventType      AS CHARACTER
    FIELD cEventExtraText AS CHARACTER
    .

DEF VAR hResBuffer AS HANDLE NO-UNDO.
hResBuffer = BUFFER ttEmail:HANDLE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fiSubject edSMS fiChars fiNumMsg fiTo ~
fiProvider fiReplyTo fiDomain tbCopyToArchive tbLoggEvent btnSend btnCancel 
&Scoped-Define DISPLAYED-OBJECTS fiSubject edSMS fiChars fiNumMsg fiTo ~
fiProvider fiReplyTo fiDomain tbCopyToArchive tbLoggEvent 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD PopulateRecipients C-Win 
FUNCTION PopulateRecipients RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCancel 
     LABEL "&Avbryt" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnSend AUTO-GO 
     LABEL "&Send" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE edSMS AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 60 BY 6.19 NO-UNDO.

DEFINE VARIABLE fiChars AS INTEGER FORMAT "->,>>>>>9":U INITIAL 0 
     LABEL "Antall tegn" 
     VIEW-AS FILL-IN 
     SIZE 7.2 BY 1 TOOLTIP "Antall tegn" NO-UNDO.

DEFINE VARIABLE fiDomain AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 28 BY 1 NO-UNDO.

DEFINE VARIABLE fiNumMsg AS INTEGER FORMAT "->,>>>>>9":U INITIAL 0 
     LABEL "Antall meldinger" 
     VIEW-AS FILL-IN 
     SIZE 7.2 BY 1 TOOLTIP "Antall meldinger" NO-UNDO.

DEFINE VARIABLE fiProvider AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 28 BY 1 NO-UNDO.

DEFINE VARIABLE fiReplyTo AS CHARACTER FORMAT "X(256)":U 
     LABEL "Svar til" 
     VIEW-AS FILL-IN 
     SIZE 21.2 BY 1 NO-UNDO.

DEFINE VARIABLE fiSubject AS CHARACTER FORMAT "X(256)":U 
     LABEL "Emne" 
     VIEW-AS FILL-IN 
     SIZE 52 BY 1 NO-UNDO.

DEFINE VARIABLE fiTo AS CHARACTER FORMAT "X(256)":U 
     LABEL "Til" 
     VIEW-AS FILL-IN 
     SIZE 21.2 BY 1 NO-UNDO.

DEFINE VARIABLE tbCopyToArchive AS LOGICAL INITIAL no 
     LABEL "Send kopi til &arkiv" 
     VIEW-AS TOGGLE-BOX
     SIZE 24.4 BY .81 NO-UNDO.

DEFINE VARIABLE tbLoggEvent AS LOGICAL INITIAL yes 
     LABEL "Opprett hendelseslogg" 
     VIEW-AS TOGGLE-BOX
     SIZE 27 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     fiSubject AT ROW 1.48 COL 8 COLON-ALIGNED
     edSMS AT ROW 2.91 COL 2.6 NO-LABEL
     fiChars AT ROW 9.38 COL 11.8 COLON-ALIGNED
     fiNumMsg AT ROW 9.38 COL 37 COLON-ALIGNED
     fiTo AT ROW 10.57 COL 11.8 COLON-ALIGNED
     fiProvider AT ROW 10.57 COL 33 COLON-ALIGNED NO-LABEL
     fiReplyTo AT ROW 11.67 COL 11.8 COLON-ALIGNED
     fiDomain AT ROW 11.67 COL 33 COLON-ALIGNED NO-LABEL
     tbCopyToArchive AT ROW 12.86 COL 13.8
     tbLoggEvent AT ROW 13.81 COL 13.6
     btnSend AT ROW 14.95 COL 32.8
     btnCancel AT ROW 14.95 COL 47.8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 63.2 BY 15.29
         DEFAULT-BUTTON btnSend CANCEL-BUTTON btnCancel.


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
         TITLE              = "Send SMS"
         HEIGHT             = 15.33
         WIDTH              = 63
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
       fiDomain:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       fiProvider:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Send SMS */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Send SMS */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel C-Win
ON CHOOSE OF btnCancel IN FRAME DEFAULT-FRAME /* Avbryt */
DO:
  APPLY "close" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSend C-Win
ON CHOOSE OF btnSend IN FRAME DEFAULT-FRAME /* Send */
DO:
  RUN SendSMS.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME edSMS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL edSMS C-Win
ON VALUE-CHANGED OF edSMS IN FRAME DEFAULT-FRAME
DO:
  ASSIGN fiChars  = LENGTH(fiSubject:SCREEN-VALUE) + LENGTH(edSMS:SCREEN-VALUE)
         fiNumMsg = TRUNC(fiChars / 161,0) + 1
         .
  DISP fiChars fiNumMsg WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiSubject
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiSubject C-Win
ON VALUE-CHANGED OF fiSubject IN FRAME DEFAULT-FRAME /* Emne */
DO:
  ASSIGN fiChars  = LENGTH(fiSubject:SCREEN-VALUE) + LENGTH(edSMS:SCREEN-VALUE)
         fiNumMsg = TRUNC(fiChars / 161,0) + 1
         .
  DISP fiChars fiNumMsg WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiTo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiTo C-Win
ON MOUSE-SELECT-DBLCLICK OF fiTo IN FRAME DEFAULT-FRAME /* Til */
DO:
  IF fiTo:READ-ONLY THEN
    DYNAMIC-FUNCTION("ToExcelViaFile",hResBuffer,0).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbCopyToArchive
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbCopyToArchive C-Win
ON VALUE-CHANGED OF tbCopyToArchive IN FRAME DEFAULT-FRAME /* Send kopi til arkiv */
DO:
  DEF VAR cAttatch   AS CHAR NO-UNDO.
  DEF VAR cRowId     AS CHAR NO-UNDO.
  DEF VAR cSourceTab AS CHAR NO-UNDO.
  DEF VAR cKeyFldLst AS CHAR NO-UNDO.
  DEF VAR cEventText AS CHAR NO-UNDO.
  DEF VAR cEventType AS CHAR NO-UNDO.
  DEF VAR cEventETxt AS CHAR NO-UNDO.

  hResBuffer:FIND-FIRST("WHERE cEmail = '" + cCompanyArchiveEmail + "'") NO-ERROR.

  IF tbCopyToArchive:CHECKED THEN DO:
    IF NOT hResBuffer:AVAIL THEN
      hResBuffer:FIND-FIRST("WHERE true") NO-ERROR.
    IF hResBuffer:AVAIL THEN
      ASSIGN cAttatch   = hResBuffer:BUFFER-FIELD("cAttachments"):BUFFER-VALUE
             cRowId     = hResBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE
             cSourceTab = hResBuffer:BUFFER-FIELD("cSourceTable"):BUFFER-VALUE
             cKeyFldLst = hResBuffer:BUFFER-FIELD("cKeyFieldList"):BUFFER-VALUE
             cEventType = hResBuffer:BUFFER-FIELD("cEventType"):BUFFER-VALUE
             cEventText = hResBuffer:BUFFER-FIELD("cEventText"):BUFFER-VALUE
             cEventETxt = hResBuffer:BUFFER-FIELD("cEventExtraText"):BUFFER-VALUE
                          .

    hResBuffer:BUFFER-CREATE().
    ASSIGN hResBuffer:BUFFER-FIELD("cSendAs"):BUFFER-VALUE         = "Bcc"
           hResBuffer:BUFFER-FIELD("cEmail"):BUFFER-VALUE          = cCompanyArchiveEmail
           hResBuffer:BUFFER-FIELD("cAttachments"):BUFFER-VALUE    = cAttatch
           hResBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE       = cRowId
           hResBuffer:BUFFER-FIELD("cSourceTable"):BUFFER-VALUE    = cSourceTab
           hResBuffer:BUFFER-FIELD("cKeyFieldList"):BUFFER-VALUE   = cKeyFldLst
           hResBuffer:BUFFER-FIELD("cEventType"):BUFFER-VALUE      = cEventType
           hResBuffer:BUFFER-FIELD("cEventText"):BUFFER-VALUE      = cEventText
           hResBuffer:BUFFER-FIELD("cEventExtraText"):BUFFER-VALUE = cEventETxt
           .
      
  END.
  ELSE IF NOT tbCopyToArchive:CHECKED AND hResBuffer:AVAIL THEN
    hResBuffer:BUFFER-DELETE().
  ELSE RETURN NO-APPLY.
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
  RUN enable_UI.

  hParent = SOURCE-PROCEDURE.
  RUN InitWindow.

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
  DISPLAY fiSubject edSMS fiChars fiNumMsg fiTo fiProvider fiReplyTo fiDomain 
          tbCopyToArchive tbLoggEvent 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE fiSubject edSMS fiChars fiNumMsg fiTo fiProvider fiReplyTo fiDomain 
         tbCopyToArchive tbLoggEvent btnSend btnCancel 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitWindow C-Win 
PROCEDURE InitWindow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Refer to the <jukebox>\winsrc\samples for working examples for Sports2000
------------------------------------------------------------------------------*/
DEF VAR cReturn          AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  ASSIGN cCompanyArchiveEmail = DYNAMIC-FUNCTION("getFieldValues","JBoxSysParam",
                                                 "WHERE bActive AND cSysParamName = 'CompanyArchiveEmailOut' AND iJBoxCompanyId = " + DYNAMIC-FUNCTION("getCompany"),
                                                 "cSysParamCharValue")

         cCompanyMailServer = DYNAMIC-FUNCTION("getFieldValues","JBoxSysParam",
                                                 "WHERE bActive AND cSysParamName = 'CompanyMailServer'(Codemaster)",
                                                 "cSysParamCharValue")

         cCompanySendFromEmail = DYNAMIC-FUNCTION("getFieldValues","JBoxSysParam",
                                                  "WHERE bActive AND cSysParamName = 'CompanySendFromEmail' AND iJBoxCompanyId = " + DYNAMIC-FUNCTION("getCompany"),
                                                  "cSysParamCharValue")

         cCompanyReplyToEmail = DYNAMIC-FUNCTION("getFieldValues","JBoxSysParam",
                                                  "WHERE bActive AND cSysParamName = 'CompanyReplyToEmail' AND iJBoxCompanyId = " + DYNAMIC-FUNCTION("getCompany"),
                                                  "cSysParamCharValue")

         cSmsProvider         = DYNAMIC-FUNCTION("getFieldValues","JBoxSysParam",
                                                  "WHERE bActive AND cSysParamName = 'CompanySMSprovider'(Codemaster)",
                                                  "cSysParamCharValue")

         cSmsDomain           = DYNAMIC-FUNCTION("getFieldValues","JBoxSysParam",
                                                   "WHERE bActive AND cSysParamName = 'CompanySMSdomain'(Codemaster)",
                                                   "cSysParamCharValue")
         cEmailUserName = DYNAMIC-FUNCTION("getFieldValues","JBoxGenCode",
                                           "WHERE iJBoxCompanyId = " + DYNAMIC-FUNCTION("getCompany")
                                         + " AND cDescription = '" + DYNAMIC-FUNCTION("getASuserId") + "'",
                                           "cCodeValue")
         .

  IF cEmailUserName = ? THEN
    cEmailUserName = DYNAMIC-FUNCTION("getFieldValues","JBoxUser",
                                      "WHERE cJBoxUserId = '" + DYNAMIC-FUNCTION("getASuserId") + "'",
                                      "cEmail").

  IF cEmailUserName = "" THEN DO:
    DYNAMIC-FUNCTION("DoMessage",0,0,IF DYNAMIC-FUNCTION("Scandinavian") THEN 
                                       "Kan ikke sende SMS pga ukjent brukernavn for epost-server"
                                     ELSE
                                       "Cannot send SMS due to unknown username for email server"
                                ,"","").
    APPLY "close" TO THIS-PROCEDURE.
    RETURN.
  END.
/*
         cAuthorize   = ENTRY(2,cReturn,"|")
         cAuthType    = ENTRY(3,cReturn,"|")
         cEmailUserName    = ENTRY(4,cReturn,"|")
         cMailPwd     = ENTRY(5,cReturn,"|")
         cMailProgram = ENTRY(6,cReturn,"|")
         cSmsDomain   = ENTRY(7,cReturn,"|")
         cSMSProvider = ENTRY(8,cReturn,"|")
         .
  IF NUM-ENTRIES(cReturn,"|") > 8 THEN
    cSMSReplyTo  = ENTRY(9,cReturn,"|").
  ELSE 
  */
  IF icReplyTo = "" THEN
    cSMSReplyTo = cEmailUserName.
  ELSE cSMSReplyTo = icReplyTo.

  IF cCompanyArchiveEmail = ? THEN
    ASSIGN tbCopyToArchive:CHECKED = NO
           tbCopyToArchive:SENSITIVE = NO
           .

  IF NOT DYNAMIC-FUNCTION("IsFieldNameInTable","JBoxEventLog","iJBoxEventLogId") OR icEventType = "" THEN
    ASSIGN tbLoggEvent:CHECKED = NO
           tbLoggEvent:SENSITIVE = NO
           .

  PopulateRecipients().

  ASSIGN fiSubject:SCREEN-VALUE  = icSubject
         fiTo:SCREEN-VALUE       = (IF NUM-ENTRIES(icMobileNumList) = 1 THEN REPLACE(icMobileNumList," ","") 
                                    ELSE STRING(NUM-ENTRIES(icMobileNumList)) +
                                       (IF DYNAMIC-FUNCTION("Scandinavian") THEN " mottakere" ELSE " recipients")
                                    )
         fiTo:READ-ONLY          = NUM-ENTRIES(icMobileNumList) > 1
         fiReplyTo:SCREEN-VALUE  = ENTRY(1,cSMSReplyTo,"@")
         fiProvider:SCREEN-VALUE = "@" + cSMSProvider
         fiDomain:SCREEN-VALUE   = "@" + cSmsDomain
         edSMS:SCREEN-VALUE      = icMessage
         .
  IF fiTo:READ-ONLY THEN 
    fiTo:TOOLTIP = IF DYNAMIC-FUNCTION("Scandinavian") THEN
                     "Dobbeltklikk for å vise mottakere"
                   ELSE
                     "Doubleclick to view recipients"
                   .
  APPLY "value-changed" TO edSMS.
END.

DYNAMIC-FUNCTION("InitTranslation",THIS-PROCEDURE:CURRENT-WINDOW).

DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,200,200,0,0).

THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = NO.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendSMS C-Win 
PROCEDURE SendSMS :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cReturn         AS CHAR NO-UNDO.
DEF VAR bUseResBuffer   AS LOG  NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  IF tbCopyToArchive:CHECKED AND NOT tbLoggEvent:CHECKED AND 
     DYNAMIC-FUNCTION("DoMessage",0,1,
                     IF DYNAMIC-FUNCTION("Scandinavian") THEN
                       "Hvis du ikke oppretter hendelseslogg så kan ikke meldingen arkiveres automatisk" + CHR(10) + "Vil du fortsette?"
                     ELSE
                       "If no event is created the message cannot be archived automatically" + CHR(10) + "Do you want to continue?"
                    ,"","") = 2 THEN 
    RETURN.


  hResBuffer:FIND-FIRST("where true") NO-ERROR.
  IF hResBuffer:AVAIL THEN DO:
    bUseResBuffer = YES.
    IF NUM-ENTRIES(icMobileNumList) LE 1 THEN DO:
      hResBuffer:BUFFER-CREATE().
      ASSIGN hResBuffer:BUFFER-FIELD("cSendAs"):BUFFER-VALUE = "TO"
             hResBuffer:BUFFER-FIELD("cEmail"):BUFFER-VALUE  = TRIM(fiTo:SCREEN-VALUE) + fiProvider:SCREEN-VALUE
             .
      IF icContextRowidList NE "" AND NUM-ENTRIES(icContextRowidList) = NUM-ENTRIES(icMobileNumList) THEN 
        ASSIGN hResBuffer:BUFFER-FIELD("cSourceTable"):BUFFER-VALUE = ENTRY(1,ENTRY(ix,icContextRowidList),";")
               hResBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE    = ENTRY(2,ENTRY(ix,icContextRowidList),";")
               .
    END.

  END.

  IF cMailProgram = "blat" THEN DO:
    RUN JBoxSendBlatMail.p ("To;" + TRIM(fiTo:SCREEN-VALUE) + fiProvider:SCREEN-VALUE,
                            TRIM(fiReplyTo:SCREEN-VALUE) + fiDomain:SCREEN-VALUE,
                            "",
                            fiSubject:SCREEN-VALUE,
                            TRIM(edSMS:SCREEN-VALUE),
                            "",
                            cCompanyMailServer,
                            cEmailUserName,
                            cMailPwd,
                            "",
                            "",
                            "",
                            "",
                            NO,
                            NO, /* Vis logg */
                            IF bUseResBuffer THEN hResBuffer ELSE ?,
                            OUTPUT bOk,
                            OUTPUT cReturn
                            ).
    IF NOT bOk THEN
      DYNAMIC-FUNCTION("DoMessage",0,0,cReturn,"","").
  END.
  ELSE DO:
    bOK = DYNAMIC-FUNCTION("runProc","jbserv_call_smtpmail.p",
                        cCompanyMailServer
                + "|" + TRIM(fiTo:SCREEN-VALUE) + fiProvider:SCREEN-VALUE    /* Til */
                + "|" + TRIM(fiReplyTo:SCREEN-VALUE) + fiDomain:SCREEN-VALUE  /* Fra */
                      + "|||||" /* |CC|BCC|Attach|localfiles| */
                      + fiSubject:SCREEN-VALUE
                + "|" + edSMS:SCREEN-VALUE
                      + "||||" /* |cont.type|bodytype|importance| */
                      + (IF cAuthorize = "1" THEN "YES" ELSE "NO")
                + "|" + cAuthType
                + "|" + cEmailUserName
                + "|" + cMailPwd
                + "|" + cMailProgram
                       ,IF NUM-ENTRIES(icMobileNumList) > 1 THEN hResBuffer ELSE ?).
    IF NOT bOk THEN 
      DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
  END.
END.

IF bOk THEN DO:
  ASSIGN obOk      = YES
         ocMessage = "SMS " + STRING(TODAY) + ", " + STRING(TIME,"HH:MM") + " " + fiTo:SCREEN-VALUE + CHR(10)
                   + fiSubject:SCREEN-VALUE + CHR(10)
                   + edSMS:SCREEN-VALUE
                   . 
  APPLY "close" TO THIS-PROCEDURE.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION PopulateRecipients C-Win 
FUNCTION PopulateRecipients RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hBrowse              AS HANDLE NO-UNDO.
DEF VAR hQuery               AS HANDLE NO-UNDO.
DEF VAR hBuffer              AS HANDLE NO-UNDO.
DEF VAR hSMSList             AS HANDLE NO-UNDO.
DEF VAR cMobileField         AS CHAR   NO-UNDO.
DEF VAR bSelectedRows        AS LOG    NO-UNDO. /* If hSMSList is a browse, send to only selected rows? */
DEF VAR cSendAsField         AS CHAR   NO-UNDO. /* If hSMSList contains a field to decide TO,CC or BCC */
DEF VAR cAttachmentField     AS CHAR   NO-UNDO. /* If hSMSList contains a field pointing to an attachment */
DEF VAR cKeyFldNameList      AS CHAR   NO-UNDO.
DEF VAR hMobileField         AS HANDLE NO-UNDO.
DEF VAR cRowIdentField       AS CHAR   NO-UNDO INIT "RowIdent1".
DEF VAR cEventTextField      AS CHAR   NO-UNDO.
DEF VAR hEventTextField      AS HANDLE NO-UNDO.
DEF VAR cEventText           AS CHAR   NO-UNDO.
DEF VAR cSourceTable         AS CHAR   NO-UNDO.
DEF VAR hKeyFldHandleList    AS HANDLE NO-UNDO EXTENT 10.
DEF VAR cIdList              AS CHAR   NO-UNDO.
DEF VAR iAddressWarning      AS INT    NO-UNDO.
DEF VAR iDuplWarning         AS INT    NO-UNDO.

icMobileNumList = TRIM(icMobileNumList,",").
IF NUM-ENTRIES(icMobileNumList) > 1 THEN DO ix = 1 TO NUM-ENTRIES(icMobileNumList):
  hResBuffer:BUFFER-CREATE().
  ASSIGN hResBuffer:BUFFER-FIELD("cSendAs"):BUFFER-VALUE = "TO"
         hResBuffer:BUFFER-FIELD("cEmail"):BUFFER-VALUE  = ENTRY(ix,icMobileNumList) + "@" + cSmsDomain
         .
  IF hResBuffer:BUFFER-FIELD("cEmail"):BUFFER-VALUE = cCompanyArchiveEmail THEN
            tbCopyToArchive:CHECKED IN FRAME {&FRAME-NAME} = YES.
  IF NUM-ENTRIES(icContextRowidList) = NUM-ENTRIES(icMobileNumList) THEN 
    ASSIGN hResBuffer:BUFFER-FIELD("cSourceTable"):BUFFER-VALUE = ENTRY(1,ENTRY(ix,icContextRowidList),";")
           hResBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE    = ENTRY(2,ENTRY(ix,icContextRowidList),";")
           .
END.  

IF CAN-DO(hParent:INTERNAL-ENTRIES,"setSMSList") THEN DO:
  RUN setSMSList IN hParent (OUTPUT hSMSList,
                             OUTPUT bSelectedRows,
                             OUTPUT cMobileField,
                             OUTPUT cRowIdentField,
                             OUTPUT cSourceTable,
                             OUTPUT cKeyFldNameList,
                             OUTPUT cEventTextField,
                             OUTPUT cEventText
                             ).
  IF VALID-HANDLE(hSMSList) THEN DO:
    CASE hSMSList:TYPE:
      WHEN "buffer" THEN DO:
        hBuffer = hSMSList.
        CREATE QUERY hQuery.
        hQuery:SET-BUFFERS(hBuffer).
      END.
      WHEN "query" THEN DO:
        hBuffer = hSMSList:GET-BUFFER-HANDLE(1).
        CREATE QUERY hQuery.
        hQuery:SET-BUFFERS(hBuffer).
      END.
      WHEN "browse" THEN DO: 
        hBuffer = hSMSList:QUERY:GET-BUFFER-HANDLE(1).
        IF NOT bSelectedRows THEN DO:
          CREATE QUERY hQuery.
          hQuery:SET-BUFFERS(hBuffer).
        END.
        ELSE hBrowse = hSMSList.
      END.
    END CASE.
    DO ix = 1 TO NUM-ENTRIES(cKeyFldNameList):
      hKeyFldHandleList[ix] = hBuffer:BUFFER-FIELD(ENTRY(ix,cKeyFldNameList)) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN DO:
        DYNAMIC-FUNCTION("DoMessage",0,0,"Invalid id field name: " + ENTRY(ix,cKeyFldNameList),"","").
        DELETE OBJECT hQuery NO-ERROR.
        APPLY "close" TO THIS-PROCEDURE.
        RETURN NO.
      END.
    END.
    hMobileField = hBuffer:BUFFER-FIELD(cMobileField) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
      DYNAMIC-FUNCTION("DoMessage",0,0,"Invalid field name for mobile number: " + cMobileField,"","").
      DELETE OBJECT hQuery NO-ERROR.
      RETURN NO.
    END.
    IF cEventTextField NE "" THEN DO:
      hEventTextField = hBuffer:BUFFER-FIELD(cEventTextField) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN DO:
        DYNAMIC-FUNCTION("DoMessage",0,0,"Invalid field name for event text field: " + cEventTextField,"","").
        DELETE OBJECT hQuery NO-ERROR.
        RETURN NO.
      END.
    END.

    IF VALID-HANDLE(hQuery) THEN DO:
      hQuery:QUERY-PREPARE("FOR EACH " + hBuffer:NAME).
      hQuery:QUERY-OPEN().
      hQuery:GET-FIRST().
      REPEAT WHILE NOT hQuery:QUERY-OFF-END:
        IF hMobileField:BUFFER-VALUE NE "" AND NOT CAN-DO(icMobileNumList,REPLACE(hMobileField:BUFFER-VALUE," ","")) THEN DO:
          hResBuffer:BUFFER-CREATE().
          ASSIGN hResBuffer:BUFFER-FIELD("cEmail"):BUFFER-VALUE  = REPLACE(hMobileField:BUFFER-VALUE," ","") + "@" + cSmsDomain
                 hResBuffer:BUFFER-FIELD("cSendAs"):BUFFER-VALUE = "TO"
                 icMobileNumList = icMobileNumList + (IF icMobileNumList NE "" THEN "," ELSE "") + REPLACE(hMobileField:BUFFER-VALUE," ","")
                 hResBuffer:BUFFER-FIELD("cKeyFieldList"):BUFFER-VALUE = cKeyFldNameList
                 hResBuffer:BUFFER-FIELD("cSourceTable"):BUFFER-VALUE = cSourceTable
                 .
          IF VALID-HANDLE(hEventTextField) THEN
            hResBuffer:BUFFER-FIELD("cEventText"):BUFFER-VALUE = cEventText + (IF cEventText NE "" THEN ": " ELSE "") + STRING(hEventTextField:BUFFER-VALUE).
          ELSE
            hResBuffer:BUFFER-FIELD("cEventText"):BUFFER-VALUE = cEventText.
  
        END.
        ELSE IF hMobileField:BUFFER-VALUE = "" THEN 
          iAddressWarning = iAddressWarning + 1.
        ELSE 
          iDuplWarning = iDuplWarning + 1.
        hQuery:GET-NEXT().
      END.
    END.
    ELSE DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
      IF hBrowse:FETCH-SELECTED-ROW(ix) THEN DO:
        IF hMobileField:BUFFER-VALUE NE "" AND NOT CAN-DO(icMobileNumList,REPLACE(hMobileField:BUFFER-VALUE," ","")) THEN DO:
          hResBuffer:BUFFER-CREATE().
          ASSIGN hResBuffer:BUFFER-FIELD("cEmail"):BUFFER-VALUE  = REPLACE(hMobileField:BUFFER-VALUE," ","") + "@" + cSmsDomain
                 hResBuffer:BUFFER-FIELD("cSendAs"):BUFFER-VALUE = "TO"
                 icMobileNumList = icMobileNumList + (IF icMobileNumList NE "" THEN "," ELSE "") + REPLACE(hMobileField:BUFFER-VALUE," ","")
                 hResBuffer:BUFFER-FIELD("cKeyFieldList"):BUFFER-VALUE = cKeyFldNameList
                 hResBuffer:BUFFER-FIELD("cSourceTable"):BUFFER-VALUE = cSourceTable
                 .
          IF VALID-HANDLE(hEventTextField) THEN
            hResBuffer:BUFFER-FIELD("cEventText"):BUFFER-VALUE = cEventText + (IF cEventText NE "" THEN ": " ELSE "") + STRING(hEventTextField:BUFFER-VALUE).
          ELSE
            hResBuffer:BUFFER-FIELD("cEventText"):BUFFER-VALUE = cEventText.

        END.
        ELSE IF hMobileField:BUFFER-VALUE = "" THEN 
          iAddressWarning = iAddressWarning + 1.
        ELSE 
          iDuplWarning = iDuplWarning + 1.
      END.
    END.
  END.
END.

DELETE OBJECT hQuery NO-ERROR.

IF iAddressWarning > 0 OR iDuplWarning > 0 THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,
                   IF DYNAMIC-FUNCTION("Scandinavian") THEN 
                     (IF iAddressWarning > 0 THEN
                       "Det mangler mobilnr for " + STRING(iAddressWarning) + " rader i utvalget" + CHR(10)
                      ELSE "")
                   + (IF iDuplWarning > 0 THEN
                       "Det er " + STRING(iDuplWarning) + " duplikate mobilnr i utvalget (bare en melding blir sendt til disse)"
                      ELSE "")
                   ELSE 
                     (IF iAddressWarning > 0 THEN
                       "Mobile number is missing for " + STRING(iAddressWarning) + " records in selection" + CHR(10)
                      ELSE "")
                   + (IF iDuplWarning > 0 THEN
                       "There are " + STRING(iDuplWarning) + " duplikate mobile numbers in the selection (only one message is sent for these)"
                      ELSE "")
                  ,"","").

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

