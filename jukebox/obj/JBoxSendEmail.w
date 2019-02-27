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
  DEF VAR icRecipientList     AS CHAR NO-UNDO INIT "To;brynjar@chemistry.no,Cc;gry@chemistry.no,Bcc;arkiv@chemistry.no".
  DEF VAR icFrom              AS CHAR NO-UNDO INIT "post@chemistry.no".
  DEF VAR icReplyTo           AS CHAR NO-UNDO.
  DEF VAR icSubject           AS CHAR NO-UNDO.
  DEF VAR icBody              AS CHAR NO-UNDO.
  DEF VAR icAttachments       AS CHAR NO-UNDO. /* INIT "C:\temp\Chemistry-logoer\ChemistrySortCMYK.jpg". */
  DEF VAR icEventType         AS CHAR NO-UNDO.  
  DEF VAR icEventExtraText    AS CHAR NO-UNDO.  
  DEF VAR icContextRowidList  AS CHAR NO-UNDO.
  DEF VAR icContextKeyFields  AS CHAR NO-UNDO.
  DEF VAR ibAutosend          AS LOG  NO-UNDO.
&ELSE
  DEF INPUT PARAM icRecipientList    AS CHAR NO-UNDO.
  DEF INPUT PARAM icFrom             AS CHAR NO-UNDO.
  DEF INPUT PARAM icReplyTo          AS CHAR NO-UNDO.
  DEF INPUT PARAM icSubject          AS CHAR NO-UNDO.
  DEF INPUT PARAM icBody             AS CHAR NO-UNDO.
  DEF INPUT PARAM icAttachments      AS CHAR NO-UNDO.
  DEF INPUT PARAM icEventType        AS CHAR NO-UNDO. /* If blank no event is created */
  DEF INPUT PARAM icEventExtraText   AS CHAR NO-UNDO. /* Event text will be email subject */
  DEF INPUT PARAM icContextRowidList AS CHAR NO-UNDO. /* Link info: <table>;<rowid>,<table>;<rowid>,<table>.. */
                                                      /* If used the list must correspond with icRecipientList */
  DEF INPUT PARAM icContextKeyFields AS CHAR NO-UNDO. /* Key fields for event link: <table>;<key fields>,<table>;<Key.. */
  DEF INPUT PARAM ibAutosend         AS LOG  NO-UNDO. /* Set when f.ex invioce is emailed */
&ENDIF

/* Local Variable Definitions ---                                       */

DEF VAR bOk                   AS LOG    NO-UNDO.
DEF VAR ix                    AS INT    NO-UNDO.
DEF VAR hToolbar              AS HANDLE NO-UNDO.
DEF VAR hBuffer               AS HANDLE NO-UNDO.
DEF VAR hParent               AS HANDLE NO-UNDO.
DEF VAR cCompanyArchiveEmail  AS CHAR   NO-UNDO.
DEF VAR cCompanyMailServer    AS CHAR   NO-UNDO.
DEF VAR cCompanySendFromEmail AS CHAR   NO-UNDO.
DEF VAR cCompanyReplyToEmail  AS CHAR   NO-UNDO.
DEF VAR cEmailUserName        AS CHAR   NO-UNDO.
DEF VAR hResBuffer            AS HANDLE NO-UNDO.
DEF VAR hResBrowse            AS HANDLE NO-UNDO.
DEF VAR hResQuery             AS HANDLE NO-UNDO.
DEF VAR hMenuItemSlettVedlegg AS HANDLE NO-UNDO.
DEF VAR cMailProgram          AS CHAR   NO-UNDO INIT "blat".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnSplitBarY fiSubject edBody btnSend ~
btnContinue tbCopyToArchive tbLoggEvent btnAttachments cmbFrom cmbReplyTo ~
fiBodyLabel brwRecipients 
&Scoped-Define DISPLAYED-OBJECTS fiSubject edBody tbCopyToArchive ~
tbLoggEvent cmbFrom cmbReplyTo fiBodyLabel 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AddAttachments C-Win 
FUNCTION AddAttachments RETURNS LOGICAL
  ( INPUT icFileNames AS CHAR,
    INPUT ibAppend    AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LocalTranslation C-Win 
FUNCTION LocalTranslation RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD PopulateRecipients C-Win 
FUNCTION PopulateRecipients RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnAttachments 
     LABEL "&Vedlegg" 
     SIZE 22 BY 1.14.

DEFINE BUTTON btnContinue 
     LABEL "&Fortsett til epost program >>" 
     SIZE 32 BY 1.14.

DEFINE BUTTON btnSend 
     LABEL "&Send direkte >>" 
     SIZE 32 BY 1.14.

DEFINE BUTTON btnSplitBarY 
     IMAGE-UP FILE "bmp/tabup.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Button 1" 
     SIZE 107.4 BY .38.

DEFINE VARIABLE cmbFrom AS CHARACTER 
     LABEL "Fra" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEMS "Item 1" 
     DROP-DOWN
     SIZE 38 BY 1 TOOLTIP "Hvis ikke utfylt benyttes brukernavn epost-server" NO-UNDO.

DEFINE VARIABLE cmbReplyTo AS CHARACTER 
     LABEL "Svar til" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN
     SIZE 37.8 BY 1 TOOLTIP "Angi hvis svar ønsket til en egen adresse" NO-UNDO.

DEFINE VARIABLE edBody AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 84.6 BY 4.43 NO-UNDO.

DEFINE VARIABLE fiBodyLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Tekst:" 
      VIEW-AS TEXT 
     SIZE 6 BY .62 NO-UNDO.

DEFINE VARIABLE fiSubject AS CHARACTER FORMAT "X(256)":U 
     LABEL "Emne" 
     VIEW-AS FILL-IN 
     SIZE 84.8 BY 1 NO-UNDO.

DEFINE RECTANGLE brwRecipients
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 84.6 BY 4.29.

DEFINE VARIABLE tbCopyToArchive AS LOGICAL INITIAL no 
     LABEL "Send kopi til &arkiv" 
     VIEW-AS TOGGLE-BOX
     SIZE 24.4 BY .81 NO-UNDO.

DEFINE VARIABLE tbLoggEvent AS LOGICAL INITIAL yes 
     LABEL "Opprett hendelseslogg" 
     VIEW-AS TOGGLE-BOX
     SIZE 34.4 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnSplitBarY AT ROW 8.43 COL 1.6
     fiSubject AT ROW 9 COL 14.2 COLON-ALIGNED
     edBody AT ROW 10.1 COL 16.2 NO-LABEL
     btnSend AT ROW 14.86 COL 42.6
     btnContinue AT ROW 14.86 COL 74.8
     tbCopyToArchive AT ROW 1.48 COL 16.6
     tbLoggEvent AT ROW 1.48 COL 42.6
     btnAttachments AT ROW 1.24 COL 79
     cmbFrom AT ROW 2.71 COL 14.6 COLON-ALIGNED
     cmbReplyTo AT ROW 2.71 COL 61.6 COLON-ALIGNED
     fiBodyLabel AT ROW 10.19 COL 7.6 COLON-ALIGNED NO-LABEL
     brwRecipients AT ROW 4.1 COL 16.4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 108.8 BY 15.33.


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
         TITLE              = "Lag epost-forsendelse"
         HEIGHT             = 15.33
         WIDTH              = 108
         MAX-HEIGHT         = 23.52
         MAX-WIDTH          = 115.6
         VIRTUAL-HEIGHT     = 23.52
         VIRTUAL-WIDTH      = 115.6
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
       btnSplitBarY:MOVABLE IN FRAME DEFAULT-FRAME          = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Lag epost-forsendelse */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Lag epost-forsendelse */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAttachments
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAttachments C-Win
ON CHOOSE OF btnAttachments IN FRAME DEFAULT-FRAME /* Vedlegg */
DO:
  DEF VAR cFileNames AS CHAR NO-UNDO.

  cFileNames = DYNAMIC-FUNCTION("SelectFileNames","Alle filer|*.*",?,"Velg fil(er) for vedlegg").

  AddAttachments(cFileNames,YES).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnContinue
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnContinue C-Win
ON CHOOSE OF btnContinue IN FRAME DEFAULT-FRAME /* Fortsett til epost program >> */
DO:
  RUN CreateEmail (YES).
  APPLY "close" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSend C-Win
ON CHOOSE OF btnSend IN FRAME DEFAULT-FRAME /* Send direkte >> */
DO:
  RUN CreateEmail (NO).
  IF RETURN-VALUE NE "retry" THEN
    APPLY "close" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSplitBarY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSplitBarY C-Win
ON END-MOVE OF btnSplitBarY IN FRAME DEFAULT-FRAME /* Button 1 */
DO:
  DYNAMIC-FUNCTION("setSplitBarY",CURRENT-WINDOW,btnSplitBarY:HANDLE IN FRAME {&FRAME-NAME},NO).
  APPLY "window-resized" TO {&WINDOW-NAME}.
/*   RUN MoveToTop. */
/*   DYNAMIC-FUNCTION("MoveTabToTop" IN hTabFolder,OrderFolder:HANDLE). */
/*   RUN MoveToTop IN hCurrTabProc NO-ERROR.                            */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbReplyTo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbReplyTo C-Win
ON TAB OF cmbReplyTo IN FRAME DEFAULT-FRAME /* Svar til */
DO:
  APPLY "entry" TO hResBrowse.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiSubject
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiSubject C-Win
ON BACK-TAB OF fiSubject IN FRAME DEFAULT-FRAME /* Emne */
DO:
  APPLY "entry" TO hResBrowse.
  RETURN NO-APPLY.
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

  RUN InvokeMethod(hResBrowse,"OpenQuery").
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

  hParent = SOURCE-PROCEDURE.

  RUN InitWindow.

  PROCESS EVENTS.
  APPLY "entry" TO fiSubject.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BackTabFromBrowse C-Win 
PROCEDURE BackTabFromBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
APPLY "entry" TO cmbReplyTo IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CreateEmail C-Win 
PROCEDURE CreateEmail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ibContinue AS LOG NO-UNDO.

DEF VAR ix           AS INT  NO-UNDO.
DEF VAR cURL         AS CHAR NO-UNDO INIT "mailto:".
DEF VAR cAdrList     AS CHAR NO-UNDO.
DEF VAR cAttachList  AS CHAR NO-UNDO.
DEF VAR cToList      AS CHAR NO-UNDO.
DEF VAR cCcFile      AS CHAR NO-UNDO.
DEF VAR cBccFile     AS CHAR NO-UNDO.
DEF VAR cBatFile     AS CHAR NO-UNDO.
DEF VAR cBodyFile    AS CHAR NO-UNDO.
DEF VAR cSubjectFile AS CHAR NO-UNDO.
DEF VAR bMailTo      AS LOG  NO-UNDO.
DEF VAR bMailCc      AS LOG  NO-UNDO.
DEF VAR bMailBcc     AS LOG  NO-UNDO.
DEF VAR cBlatExe     AS CHAR NO-UNDO.
DEF VAR cMailLog     AS CHAR NO-UNDO.
DEF VAR bViewLogFile AS LOG  NO-UNDO.
DEF VAR iResult      AS INT  NO-UNDO.
DEF VAR cEventHdrId  AS CHAR NO-UNDO.
DEF VAR cReturn      AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  IF tbCopyToArchive:CHECKED AND NOT tbLoggEvent:CHECKED AND 
     DYNAMIC-FUNCTION("DoMessage",0,1,
                       IF DYNAMIC-FUNCTION("Scandinavian") THEN
                         "Hvis du ikke oppretter hendelseslogg så kan ikke eposten arkiveres automatisk" + CHR(10) + "Vil du fortsette?"
                       ELSE
                         "If no event is created the email cannot be archived automatically" + CHR(10) + "Do you want to continue?"
                      ,"","") = 2 THEN 
    RETURN.
  IF tbLoggEvent:CHECKED THEN DO:
    hResQuery:GET-FIRST().
    REPEAT WHILE NOT hResQuery:QUERY-OFF-END:
      hResBuffer:BUFFER-FIELD("cEventText"):BUFFER-VALUE = fiSubject:SCREEN-VALUE + CHR(10)
                                                         + hResBuffer:BUFFER-FIELD("cSendAs"):BUFFER-VALUE + ": "
                                                         + hResBuffer:BUFFER-FIELD("cEmail"):BUFFER-VALUE + CHR(10)
                                                         + "From: " + (IF cmbFrom:SCREEN-VALUE NE "" AND cmbFrom:SCREEN-VALUE NE ? THEN cmbFrom:SCREEN-VALUE ELSE cEmailUserName) + CHR(10)   
                                                         + edBody:SCREEN-VALUE
                                                         .
      hResQuery:GET-NEXT().
    END.
  END.

  IF ibContinue THEN DO:
    IF tbLoggEvent:CHECKED THEN DO:
      IF NOT DYNAMIC-FUNCTION("runProc","jbadmin_make_eventlog.p",
                                                  icEventType + "|"
                                                + "|||||"   /* subject|date|time|source table|rowid */
                                                + icContextKeyFields + "|"
                                                + icEventExtraText
                                                  ,hResBuffer) THEN DO:
        DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
        RETURN "retry".
      END.
      IF tbCopyToArchive:CHECKED THEN cEventHdrId = DYNAMIC-FUNCTION("getTransactionMessage").
    END.

    cURL = cURL
         + "?subject=" + REPLACE(fiSubject:SCREEN-VALUE + (IF cEventHdrId NE "" THEN " [" + cEventHdrId + "]" ELSE "")," ","%20")  
         + "&body=" + REPLACE(REPLACE(edBody:SCREEN-VALUE," ","%20"),CHR(10),"%0A")
         + (IF cmbFrom:SCREEN-VALUE NE ? AND cmbFrom:SCREEN-VALUE NE "" THEN "&from=" + cmbFrom:SCREEN-VALUE ELSE "")
         + (IF cmbReplyTo:SCREEN-VALUE NE ? AND cmbReplyTo:SCREEN-VALUE NE "" THEN "&reply-to=" + cmbReplyTo:SCREEN-VALUE ELSE "")
         . 


    hResQuery:GET-FIRST().
    REPEAT WHILE NOT hResQuery:QUERY-OFF-END:
      cToList = cToList + "&" + hResBuffer:BUFFER-FIELD("cSendAs"):BUFFER-VALUE + "="
           + hResBuffer:BUFFER-FIELD("cEmail"):BUFFER-VALUE
           .
      ix = ix + 1.

      IF ix MOD 50 = 0 THEN DO:
        cReturn = DYNAMIC-FUNCTION("setWebDoc","",cURL + cToList).
        IF cReturn NE "" THEN DO:
          DYNAMIC-FUNCTION("DoMessage",0,0,cReturn,"","").
          RETURN.
        END.
        cToList = "".
      END.

      hResQuery:GET-NEXT().
    END.

    IF cToList NE "" THEN DO:
      cReturn = DYNAMIC-FUNCTION("setWebDoc","",cURL + cToList).
      IF cReturn NE "" THEN DO:
        DYNAMIC-FUNCTION("DoMessage",0,0,cReturn,"","").
        RETURN.
      END.
    END.
      
  END.
  ELSE DO:
    IF cMailProgram = "blat" THEN DO:
      RUN JBoxSendBlatMail.p ("",
                              IF cmbFrom:SCREEN-VALUE NE ? THEN cmbFrom:SCREEN-VALUE ELSE "",
                              IF cmbReplyTo:SCREEN-VALUE NE ? THEN cmbReplyTo:SCREEN-VALUE ELSE "",
                              fiSubject:SCREEN-VALUE,
                              edBody:SCREEN-VALUE,
                              "",
                              "",
                              "",
                              "",
                              icEventType,
                              icEventExtraText,
                              icContextRowidList,
                              icContextKeyFields,
                              ibAutoSend,
                              bViewLogFile,
                              hResBuffer,
                              OUTPUT bOk,
                              OUTPUT cReturn).
     
      IF NOT bOk THEN DO:
        DYNAMIC-FUNCTION("DoMessage",0,0,cReturn,"","").
        RETURN "cancel".
      END.
    END.
    ELSE DO:
      /*
      bOK = DYNAMIC-FUNCTION("runProc","jbserv_call_smtpmail.p",
                          cMailServer
                  + "|" + TRIM(fiTil:SCREEN-VALUE) + fiProvider:SCREEN-VALUE    /* Til */
                  + "|" + TRIM(fiSvarTil:SCREEN-VALUE) + fiDomain:SCREEN-VALUE  /* Fra */
                        + "|||||" /* |CC|BCC|Attach|localfiles| */
                        + fiEmne:SCREEN-VALUE
                  + "|" + edSMS:SCREEN-VALUE
                        + "||||" /* |cont.type|bodytype|importance| */
                        + (IF cAuthorize = "1" THEN "YES" ELSE "NO")
                  + "|" + cAuthType
                  + "|" + cMailUser
                  + "|" + cMailPwd
                  + "|" + cMailProgram
                         ,?).
      IF NOT bOk THEN 
        DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").        
        */
    END.
  END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeleteRecord C-Win 
PROCEDURE DeleteRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR bUncheckArchive AS LOG NO-UNDO.
bUncheckArchive = hResBuffer:BUFFER-FIELD("cEmail"):BUFFER-VALUE = cCompanyArchiveEmail.
RUN SUPER.
IF bUncheckArchive THEN
  tbCopyToArchive:CHECKED IN FRAME {&FRAME-NAME} = NO.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DropFileNotifyBrowse C-Win 
PROCEDURE DropFileNotifyBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cFileNames AS CHAR NO-UNDO.
DO ix = 1 TO hResBrowse:NUM-DROPPED-FILES:
  cFileNames = cFileNames + hResBrowse:GET-DROPPED-FILE(ix) + ";".
END.
IF cFileNames NE "" THEN
  AddAttachments(cFileNames,YES).

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
  DISPLAY fiSubject edBody tbCopyToArchive tbLoggEvent cmbFrom cmbReplyTo 
          fiBodyLabel 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btnSplitBarY fiSubject edBody btnSend btnContinue tbCopyToArchive 
         tbLoggEvent btnAttachments cmbFrom cmbReplyTo fiBodyLabel 
         brwRecipients 
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
RUN enable_UI.

DYNAMIC-FUNCTION("CreateStatusBar",THIS-PROCEDURE:CURRENT-WINDOW,"",0,YES,?).

DO WITH FRAME {&FRAME-NAME}:
  ASSIGN fiSubject:SCREEN-VALUE = icSubject
         edBody:SCREEN-VALUE    = icBody

         cCompanyArchiveEmail = DYNAMIC-FUNCTION("getFieldValues","JBoxSysParam",
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
    IF ibAutoSend THEN DO:
      DYNAMIC-FUNCTION("DoMessage",0,0,IF DYNAMIC-FUNCTION("Scandinavian") THEN 
                                         "Kan ikke sende epost automatisk pga ukjent brukernavn for epost-server"
                                       ELSE
                                         "Email cannot be sent automatically due to unknown username for email server"
                                  ,"","").
      APPLY "close" TO THIS-PROCEDURE.
      RETURN.
    END.
    ELSE ASSIGN btnSend:HIDDEN        = YES
                btnAttachments:HIDDEN = YES.
  END.
  IF cCompanyMailServer = ? AND ibAutoSend THEN DO:
    DYNAMIC-FUNCTION("DoMessage",0,0,"Kan ikke sende epost automatisk pga ukjent navn på epost-server","","").
    APPLY "close" TO THIS-PROCEDURE.
    RETURN.
  END.

  ASSIGN cmbFrom:LIST-ITEMS = TRIM(
                              (IF cCompanySendFromEmail NE ? THEN cCompanySendFromEmail + "," ELSE "")
                            + (IF cEmailUserName NE "" THEN cEmailUserName + "," ELSE "")
                            + (IF icFrom NE "" AND icFrom NE cEmailUserName AND icFrom NE cCompanySendFromEmail THEN icFrom ELSE "")
                              ,",")
         cmbReplyTo:LIST-ITEMS = TRIM(
                         (IF cCompanyReplyToEmail NE ? THEN cCompanyReplyToEmail + "," ELSE "")
                       + (IF cEmailUserName NE "" THEN cEmailUserName + "," ELSE "")
                       + (IF icReplyTo NE "" THEN icReplyTo ELSE "")
                         ,",")
         .

  IF icFrom    NE "" THEN 
    cmbFrom:SCREEN-VALUE = icFrom.
  ELSE 
    cmbFrom:SCREEN-VALUE = cEmailUserName.

  IF icReplyTo NE "" THEN cmbReplyTo:SCREEN-VALUE = icReplyTo.

  IF cCompanyArchiveEmail = ? OR ibAutoSend THEN
    ASSIGN tbCopyToArchive:CHECKED = NO
           tbCopyToArchive:SENSITIVE = NO
           .
  IF NOT DYNAMIC-FUNCTION("IsFieldNameInTable","JBoxEventLog","iJBoxEventLogId") OR icEventType = "" THEN
    ASSIGN tbLoggEvent:CHECKED = NO
           tbLoggEvent:SENSITIVE = NO
           .

  &IF DEFINED(UIB_is_Running) = 0 &THEN
    IF ibAutoSend THEN
      ASSIGN btnContinue:HIDDEN    = YES
             btnAttachments:HIDDEN = YES
             btnSend:X = btnContinue:X 
             .
    IF DYNAMIC-FUNCTION("getAttribute",SESSION:FIRST-PROCEDURE,"CompanyMailServer") = "" THEN
      ASSIGN btnSend:HIDDEN        = YES
             btnAttachments:HIDDEN = YES
             .
  &ENDIF

  hResBrowse = DYNAMIC-FUNCTION("NewBrowse"
          ,brwRecipients:HANDLE
          ,100
          ,""
          ,"temp-table"
           + ";cSendAs|CHARACTER|x(12)||To/Cc/Bcc"
           + ";cEmail|CHARACTER|x(50)||" + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Mottaker" ELSE "Recipient")
           + ";cAttachments|CHARACTER|x(256)||" + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Vedlegg" ELSE "Attachments")
           + ";!RowIdent1|CHARACTER|x(30)"
           + ";!cSourceTable|CHARACTER|x(30)"
           + ";!cKeyFieldList|CHARACTER|x(40)"
           + ";!cKeyValueList|CHARACTER|x(40)"
           + ";!cEventText|CHARACTER|x(50)"
           + ";!cEventType|CHARACTER|x(20)"
           + ";!cEventExtraText|CHARACTER|x(20)"
          ,"WHERE false"
          ,"").
  ASSIGN hResBuffer = hResBrowse:QUERY:GET-BUFFER-HANDLE(1)
         hResQuery  = hResBrowse:QUERY
         hResBrowse:DROP-TARGET = YES
         .

  DYNAMIC-FUNCTION("setSortString",hResBrowse,"cSendAs;desc,cEmail").

  DYNAMIC-FUNCTION("setAttribute",hResBrowse,"allowDeleteKey","yes").

  DYNAMIC-FUNCTION("NewMenuBand",hResBrowse
                  ,"MultiSortBrowse;" + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Sorter på flere kolonner" ELSE "Sort on multiple columns")
                + ",sendAs;" + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Endre To/Cc/Bcc" ELSE "Change To/Cc/Bcc") 
                + ",delete;" + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Slett mottaker" ELSE "Delete recipient") 
                + (IF NOT ibAutoSend THEN 
                    ",SlettVedlegg;" + (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Slett vedlegg" ELSE "Delete attachment")
                   ELSE "")
                  ,"").
  DYNAMIC-FUNCTION("setAttribute",hResBrowse,"disabledEvents","SlettVedlegg").
  DYNAMIC-FUNCTION("setAttribute",hResBrowse,"allowDeleteKey","yes").

  IF NOT PopulateRecipients() THEN DO:
    APPLY "close" TO THIS-PROCEDURE.  
    RETURN.
  END.

END.

LocalTranslation().
DYNAMIC-FUNCTION("InitTranslation",THIS-PROCEDURE:CURRENT-WINDOW).

DYNAMIC-FUNCTION("setNoMoveX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,"btnAttachments,cmbReplyTo,tbLoggEvent").
DYNAMIC-FUNCTION("setAddResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,"fiSubject").
DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:HANDLE,"brwRecipients," + hResBrowse:NAME).
DYNAMIC-FUNCTION("setSplitBarY",THIS-PROCEDURE:CURRENT-WINDOW, btnSplitBarY:HANDLE IN FRAME {&FRAME-NAME},NO).
DYNAMIC-FUNCTION("setSplitBarYlimits",btnSplitBarY:HANDLE IN FRAME {&FRAME-NAME},100,100).
DYNAMIC-FUNCTION("setFollowSplitBarY",THIS-PROCEDURE:CURRENT-WINDOW,btnSplitBarY:HANDLE IN FRAME {&FRAME-NAME},
                 STRING(brwRecipients:HANDLE) + "," + STRING(hResBrowse) + "," + STRING(edBody:HANDLE) + "," + STRING(fiBodyLabel:HANDLE) + "," + STRING(fiSubject:HANDLE)). 

DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,200,100,0,0).

IF icEventType = "" AND cCompanyArchiveEmail = "" AND (hResBuffer:AVAIL AND hResBuffer:BUFFER-FIELD("cAttachments"):BUFFER-VALUE = "") THEN
  APPLY "choose" TO btnContinue.
ELSE
  THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = NO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendAsRecord C-Win 
PROCEDURE SendAsRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cReturn AS CHAR NO-UNDO.
RUN JBoxDSimpleSelectList.w ("To|To|Cc|Cc|Bcc|Bcc",?,OUTPUT cReturn).
IF cReturn NE "" THEN DO:
  IF DYNAMIC-FUNCTION("DoMessage",0,4,(IF DYNAMIC-FUNCTION("Scandinavian") THEN "Endre for alle mottakere?" ELSE "Change for all recipients?"),"","") = 6 THEN DO:
    hResBrowse:QUERY:GET-FIRST().
    REPEAT WHILE NOT hResBrowse:QUERY:QUERY-OFF-END:
      hResBuffer:BUFFER-FIELD("cSendAs"):BUFFER-VALUE = cReturn.
      hResBrowse:QUERY:GET-NEXT().
    END.
  END.
  ELSE hResBuffer:BUFFER-FIELD("cSendAs"):BUFFER-VALUE = cReturn.

  hResBrowse:REFRESH().
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

DEF VAR hSourceBuffer  AS HANDLE NO-UNDO.
DEF VAR hTargetBuffer  AS HANDLE NO-UNDO.
DEF VAR ix             AS INT    NO-UNDO.
DEF VAR cSelectedRows  AS CHAR   NO-UNDO.

hSourceBuffer = ihSourceBrw:QUERY:GET-BUFFER-HANDLE(1).
hTargetBuffer = ihTargetBrw:QUERY:GET-BUFFER-HANDLE(1).

DYNAMIC-FUNCTION("setAttribute",ihSourceBrw,"basequery","where true").
DYNAMIC-FUNCTION("setAttribute",ihSourceBrw,"uselocaldata","yes").
DYNAMIC-FUNCTION("setAttribute",ihTargetBrw,"uselocaldata","yes").

/* Fill temp-table: */
DO ix = 1 TO NUM-ENTRIES(hResBuffer:BUFFER-FIELD("cAttachments"):BUFFER-VALUE,";"):
  hSourceBuffer:BUFFER-CREATE().
  ASSIGN hSourceBuffer:BUFFER-FIELD("cAttachment"):BUFFER-VALUE = ENTRY(ix,hResBuffer:BUFFER-FIELD("cAttachments"):BUFFER-VALUE,";")
         hSourceBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE = "rowid" + STRING(ix)
         .
  hTargetBuffer:BUFFER-CREATE().
  ASSIGN hTargetBuffer:BUFFER-FIELD("cAttachment"):BUFFER-VALUE = ENTRY(ix,hResBuffer:BUFFER-FIELD("cAttachments"):BUFFER-VALUE,";")
         hTargetBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE = "rowid" + STRING(ix)
         cSelectedRows = cSelectedRows + "rowid" + STRING(ix) + ","
         .

END.

DYNAMIC-FUNCTION("setSelectedRowids" IN SOURCE-PROCEDURE,cSelectedRows).

DYNAMIC-FUNCTION("setCurrentObject",ihSourceBrw).
RUN OpenQuerySource IN SOURCE-PROCEDURE.

DYNAMIC-FUNCTION("setCurrentObject",ihTargetBrw).
RUN OpenQueryTarget IN SOURCE-PROCEDURE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettVedleggRecord C-Win 
PROCEDURE SlettVedleggRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cRowIdList  AS CHAR NO-UNDO.
DEF VAR cIdList     AS CHAR NO-UNDO.  
DEF VAR bOk         AS LOG  NO-UNDO.

IF NOT hResBuffer:AVAIL THEN RETURN.

IF NUM-ENTRIES(hResBuffer:BUFFER-FIELD("cAttachments"):BUFFER-VALUE,";") = 1 THEN 
  AddAttachments("",NO).
ELSE DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "temp-table"      
                      + ";!dummyField|character|x(10)"  
                      + ";cAttachment|CHARACTER|x(50)||Vedlegg"
                      ,"where false",
                      INPUT-OUTPUT cRowIdList,
                      "cAttachment", 
                      INPUT-OUTPUT cIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.
  
  IF bOk THEN AddAttachments(REPLACE(cIdList,"|",";"),NO).
END.

APPLY "entry" TO hResBrowse.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TabFromBrowse C-Win 
PROCEDURE TabFromBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
APPLY "entry" TO fiSubject IN FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AddAttachments C-Win 
FUNCTION AddAttachments RETURNS LOGICAL
  ( INPUT icFileNames AS CHAR,
    INPUT ibAppend    AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  IF btnAttachments:HIDDEN THEN
    ASSIGN icFileNames = ""
           ibAppend    = NO.

  hResQuery:GET-FIRST().
  REPEAT WHILE NOT hResQuery:QUERY-OFF-END:
    hResBuffer:BUFFER-FIELD("cAttachments"):BUFFER-VALUE = IF ibAppend THEN
                                                             hResBuffer:BUFFER-FIELD("cAttachments"):BUFFER-VALUE
                                                             + (IF hResBuffer:BUFFER-FIELD("cAttachments"):BUFFER-VALUE NE "" AND ibAppend THEN ";" ELSE "")
                                                             + icFileNames
                                                            ELSE icFileNames.
    hResQuery:GET-NEXT().
  END.

  hResBuffer:FIND-FIRST("WHERE cAttachments > ''") NO-ERROR.
  btnContinue:SENSITIVE = NOT hResBuffer:AVAIL AND NOT ibAutosend.

  DYNAMIC-FUNCTION("setAttribute",hResBrowse,"disabledEvents",(IF NOT hResBuffer:AVAIL OR ibAutosend THEN "SlettVedlegg" ELSE "")).

  RUN InvokeMethod(hResBrowse,"OpenQuery").
END.
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LocalTranslation C-Win 
FUNCTION LocalTranslation RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  IF NOT DYNAMIC-FUNCTION("Scandinavian") THEN
    ASSIGN THIS-PROCEDURE:CURRENT-WINDOW:TITLE = "Send email"
           btnAttachments:LABEL = "&Attachments"
           btnContinue:LABEL = "&Continue to email program >>" 
           btnSend:LABEL = "&Send directly >>"
           cmbFrom:LABEL = "From"
           cmbFrom:TOOLTIP = "If blank your username for the email server is used"
           cmbReplyTo:LABEL = "Reply to"
           cmbReplyTo:TOOLTIP = "Used when replies should be sent to a different address. Otherwise leave blank"
           fiBodyLabel:SCREEN-VALUE = " Text:" 
           fiSubject:LABEL = "Subject"
           tbCopyToArchive:LABEL = "Send copy to archive" 
           tbLoggEvent:LABEL = "Create event"
           .
END.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION PopulateRecipients C-Win 
FUNCTION PopulateRecipients RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hBrowse              AS HANDLE NO-UNDO.
DEF VAR hQuery               AS HANDLE NO-UNDO.
DEF VAR hEmailList           AS HANDLE NO-UNDO.
DEF VAR cEmailAdrField       AS CHAR   NO-UNDO.
DEF VAR bSelectedRows        AS LOG    NO-UNDO. /* If hEmailList is a browse, send to only selected rows? */
DEF VAR cSendAsField         AS CHAR   NO-UNDO. /* If hEmailList contains a field to decide TO,CC or BCC */
DEF VAR cAttachmentField     AS CHAR   NO-UNDO. /* If hEmailList contains a field pointing to an attachment */
DEF VAR cKeyFldNameListField AS CHAR   NO-UNDO.
DEF VAR hEmailAddrField      AS HANDLE NO-UNDO.
DEF VAR hSendAsField         AS HANDLE NO-UNDO.
DEF VAR cRowIdentField       AS CHAR   NO-UNDO INIT "RowIdent1".
DEF VAR cSourceTable         AS CHAR   NO-UNDO.
DEF VAR hAttachmentField     AS HANDLE NO-UNDO.
DEF VAR hKeyFldHandleList    AS HANDLE NO-UNDO EXTENT 10.
DEF VAR hRowIdentField       AS HANDLE NO-UNDO.
DEF VAR cIdList              AS CHAR   NO-UNDO.
DEF VAR iAddressWarning      AS INT    NO-UNDO.
DEF VAR iDuplWarning         AS INT    NO-UNDO.

IF icAttachments NE "" THEN DO:
  icAttachments = REPLACE(icAttachments,",",";").
  DO ix = 1 TO NUM-ENTRIES(icAttachments,";"):
    FILE-INFO:FILE-NAME = ENTRY(ix,icAttachments,";").
    IF FILE-INFO:FULL-PATHNAME = ? THEN DO:
      DYNAMIC-FUNCTION("DoMessage",0,0,"Invalid file name for attachment: " + ENTRY(ix,icAttachments,";"),"","").
      RETURN NO.
    END.
  END.
END.

DO ix = 1 TO NUM-ENTRIES(icRecipientList):
  hResBuffer:BUFFER-CREATE().
  ASSIGN hResBuffer:BUFFER-FIELD("cSendAs"):BUFFER-VALUE = ENTRY(1,ENTRY(ix,icRecipientList),";")
         hResBuffer:BUFFER-FIELD("cEmail"):BUFFER-VALUE  = ENTRY(2,ENTRY(ix,icRecipientList),";")
         .
  IF hResBuffer:BUFFER-FIELD("cEmail"):BUFFER-VALUE = cCompanyArchiveEmail THEN
            tbCopyToArchive:CHECKED IN FRAME {&FRAME-NAME} = YES.
  IF NUM-ENTRIES(icContextRowidList) = NUM-ENTRIES(icRecipientList) THEN 
    ASSIGN hResBuffer:BUFFER-FIELD("cSourceTable"):BUFFER-VALUE = ENTRY(1,ENTRY(ix,icContextRowidList),";")
           hResBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE    = ENTRY(2,ENTRY(ix,icContextRowidList),";")
           .
END.  

IF CAN-DO(hParent:INTERNAL-ENTRIES,"setEmailList") THEN DO:
  RUN setEmailList IN hParent (OUTPUT hEmailList,
                               OUTPUT bSelectedRows,
                               OUTPUT cEmailAdrField,
                               OUTPUT cSendAsField,
                               OUTPUT cAttachmentField,
                               OUTPUT cRowIdentField,
                               OUTPUT cSourceTable,
                               OUTPUT cKeyFldNameListField
                               ).
  IF VALID-HANDLE(hEmailList) THEN DO:
    CASE hEmailList:TYPE:
      WHEN "buffer" THEN DO:
        hBuffer = hEmailList.
        CREATE QUERY hQuery.
        hQuery:SET-BUFFERS(hBuffer).
      END.
      WHEN "query" THEN DO:
        hBuffer = hEmailList:GET-BUFFER-HANDLE(1).
        CREATE QUERY hQuery.
        hQuery:SET-BUFFERS(hBuffer).
      END.
      WHEN "browse" THEN DO: 
        hBuffer = hEmailList:QUERY:GET-BUFFER-HANDLE(1).
        IF NOT bSelectedRows THEN DO:
          CREATE QUERY hQuery.
          hQuery:SET-BUFFERS(hBuffer).
        END.
        ELSE hBrowse = hEmailList.
      END.
    END CASE.
    DO ix = 1 TO NUM-ENTRIES(cKeyFldNameListField):
      hKeyFldHandleList[ix] = hBuffer:BUFFER-FIELD(ENTRY(ix,cKeyFldNameListField)) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN DO:
        DYNAMIC-FUNCTION("DoMessage",0,0,"Invalid id field name: " + ENTRY(ix,cKeyFldNameListField),"","").
        DELETE OBJECT hQuery NO-ERROR.
        APPLY "close" TO THIS-PROCEDURE.
        RETURN NO.
      END.
    END.
    hEmailAddrField = hBuffer:BUFFER-FIELD(cEmailAdrField) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
      DYNAMIC-FUNCTION("DoMessage",0,0,"Invalid field name for email address: " + cEmailAdrField,"","").
      DELETE OBJECT hQuery NO-ERROR.
      RETURN NO.
    END.
    IF cSendAsField NE "" THEN DO:
      hSendAsField = hBuffer:BUFFER-FIELD(cSendAsField) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN DO:
        DYNAMIC-FUNCTION("DoMessage",0,0,"Invalid field name for send-as info: " + cSendAsField,"","").
        DELETE OBJECT hQuery NO-ERROR.
        RETURN NO.
      END.
    END.
    IF cAttachmentField NE "" THEN DO:
      hAttachmentField = hBuffer:BUFFER-FIELD(cAttachmentField) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN DO:
        DYNAMIC-FUNCTION("DoMessage",0,0,"Invalid field name for attachment info: " + cAttachmentField,"","").
        DELETE OBJECT hQuery NO-ERROR.
        RETURN NO.
      END.
    END.
    IF cRowIdentField NE "" THEN DO:
      hRowIdentField = hBuffer:BUFFER-FIELD(cRowIdentField) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN DO:
        DYNAMIC-FUNCTION("DoMessage",0,0,"Invalid field name for attachment info: " + cRowIdentField,"","").
        DELETE OBJECT hQuery NO-ERROR.
        RETURN NO.
      END.
    END.

    IF VALID-HANDLE(hQuery) THEN DO:
      hQuery:QUERY-PREPARE("FOR EACH " + hBuffer:NAME).
      hQuery:QUERY-OPEN().
      hQuery:GET-FIRST().
      REPEAT WHILE NOT hQuery:QUERY-OFF-END:
        IF hEmailAddrField:BUFFER-VALUE NE "" THEN DO:
          hResBuffer:FIND-FIRST("WHERE cEmail = '" + hEmailAddrField:BUFFER-VALUE + "'",NO-LOCK) NO-ERROR.
          IF NOT hResBuffer:AVAIL THEN DO:
            hResBuffer:BUFFER-CREATE().
            ASSIGN hResBuffer:BUFFER-FIELD("cEmail"):BUFFER-VALUE  = hEmailAddrField:BUFFER-VALUE
                   hResBuffer:BUFFER-FIELD("cSourceTable"):BUFFER-VALUE = cSourceTable
                   cIdList = ""
                   .
            DO ix = 1 TO NUM-ENTRIES(cKeyFldNameListField):
              cIdList = cIdList + (IF cIdList NE "" THEN "|" ELSE "") + hKeyFldHandleList[ix]:BUFFER-VALUE. 
            END.
            IF cIdList NE "" THEN
              hResBuffer:BUFFER-FIELD("cKeyValueList"):BUFFER-VALUE = cIdList.
    
            IF cSendAsField NE "" THEN
              hResBuffer:BUFFER-FIELD("cSendAs"):BUFFER-VALUE = hSendAsField:BUFFER-VALUE.
            ELSE 
              hResBuffer:BUFFER-FIELD("cSendAs"):BUFFER-VALUE = "Bcc".
    
            IF cAttachmentField NE "" THEN
              hResBuffer:BUFFER-FIELD("cAttachments"):BUFFER-VALUE = hAttachmentField:BUFFER-VALUE. 

            IF cRowIdentField NE "" THEN
              hResBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE = hRowIdentField:BUFFER-VALUE.
          END.
          ELSE 
            iDuplWarning = iDuplWarning + 1.
        END.
        ELSE iAddressWarning = iAddressWarning + 1.
        hQuery:GET-NEXT().
      END.
    END.
    ELSE DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
      IF hBrowse:FETCH-SELECTED-ROW(ix) THEN DO:
        IF hEmailAddrField:BUFFER-VALUE NE "" THEN DO:
          hResBuffer:FIND-FIRST("WHERE cEmail = '" + hEmailAddrField:BUFFER-VALUE + "'",NO-LOCK) NO-ERROR.
          IF NOT hResBuffer:AVAIL THEN DO:
            hResBuffer:BUFFER-CREATE().
            ASSIGN hResBuffer:BUFFER-FIELD("cEmail"):BUFFER-VALUE  = hEmailAddrField:BUFFER-VALUE
                   hResBuffer:BUFFER-FIELD("cSourceTable"):BUFFER-VALUE = cSourceTable
                   hResBuffer:BUFFER-FIELD("cKeyFieldList"):BUFFER-VALUE = cKeyFldNameListField
                   .
  
            IF cSendAsField NE "" THEN
              hResBuffer:BUFFER-FIELD("cSendAs"):BUFFER-VALUE = hSendAsField:BUFFER-VALUE.
            ELSE 
              hResBuffer:BUFFER-FIELD("cSendAs"):BUFFER-VALUE = "Bcc".
  
            IF cAttachmentField NE "" THEN
              hResBuffer:BUFFER-FIELD("cAttachments"):BUFFER-VALUE = hAttachmentField:BUFFER-VALUE.        

            IF cRowIdentField NE "" THEN
              hResBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE = hRowIdentField:BUFFER-VALUE.
          END.
          ELSE 
            iDuplWarning = iDuplWarning + 1.
        END.
        ELSE iAddressWarning = iAddressWarning + 1.
      END.
    END.
  END.
END.

DELETE OBJECT hQuery NO-ERROR.

RUN InvokeMethod(hResBrowse,"OpenQuery").
AddAttachments(icAttachments,YES).

IF iAddressWarning > 0 OR iDuplWarning > 0 THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,
                   IF DYNAMIC-FUNCTION("Scandinavian") THEN 
                     (IF iAddressWarning > 0 THEN
                       "Det mangler epost for " + STRING(iAddressWarning) + " rader i utvalget" + CHR(10)
                      ELSE "")
                   + (IF iDuplWarning > 0 THEN
                       "Det er " + STRING(iDuplWarning) + " duplikate adresser i utvalget (bare en melding blir sendt til disse)"
                      ELSE "")
                   ELSE 
                     (IF iAddressWarning > 0 THEN
                       "Email address is missing for " + STRING(iAddressWarning) + " records in selection" + CHR(10)
                      ELSE "")
                   + (IF iDuplWarning > 0 THEN
                       "There are " + STRING(iDuplWarning) + " duplikate mail addresses in the selection (only one mail is sent for these)"
                      ELSE "")
                  ,"","").

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

