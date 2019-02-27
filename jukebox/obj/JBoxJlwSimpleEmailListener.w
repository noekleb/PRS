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

DEFINE VARIABLE bConvHtmlToText     AS LOGICAL     NO-UNDO.
DEFINE VARIABLE bCheckIfStored      AS LOGICAL     NO-UNDO.
DEFINE VARIABLE bFirstSuspend       AS LOGICAL     NO-UNDO.
DEFINE VARIABLE bSuspend            AS LOGICAL     NO-UNDO.
DEFINE VARIABLE bUploadAttAsync     AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cDocLoadContext     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cMethod             AS CHARACTER   NO-UNDO.
DEFINE VARIABLE hParent             AS HANDLE      NO-UNDO.
DEFINE VARIABLE iChrPrLineHtmlToTxt AS INTEGER     NO-UNDO.
DEFINE VARIABLE iLastMsgCount       AS INTEGER     NO-UNDO.
DEFINE VARIABLE cLastLoadedDocIds   AS CHARACTER   NO-UNDO.

DEF TEMP-TABLE ttEmail 
    FIELD dDateSent       AS DATE 
    FIELD iTimeSent       AS INT
    FIELD cTimeSent       AS CHAR FORMAT "x(5)"
    FIELD cSubject        AS CHAR FORMAT "x(80)"
    FIELD cFrom           AS CHAR FORMAT "x(40)"
    FIELD cFromEmail      AS CHAR FORMAT "x(40)"
    FIELD cFromName       AS CHAR FORMAT "x(40)"
    FIELD cTo             AS CHAR FORMAT "x(40)"
    FIELD cCc             AS CHAR FORMAT "x(40)"
    FIELD cBcc            AS CHAR FORMAT "x(40)"
    FIELD cBody           AS CHAR FORMAT "x(256)"
    FIELD iPriority       AS INT
    FIELD iAttachCount    AS INT
    FIELD cAttachments    AS CHAR FORMAT "x(80)"
    FIELD iMsgId          AS INT
    FIELD bBodyRetrieved  AS LOG
    FIELD iMsgType        AS INT        /*Determines type of message, 1 = Plain 2 = HTML*/
    FIELD cFileName       AS CHAR FORMAT "x(40)"
    FIELD RowIdent1       AS CHAR
    .
DEF VAR httEmailBuffer AS HANDLE NO-UNDO.
DEF VAR httEmail       AS HANDLE NO-UNDO.
httEmailBuffer = BUFFER ttEmail:HANDLE.
httEmail       = httEmailBuffer:TABLE-HANDLE.

DEF TEMP-TABLE ttNewEmail
    FIELD dDateSent       AS DATE
    FIELD iTimeSent       AS INT
    FIELD cTimeSent       AS CHAR FORMAT "x(5)"
    FIELD cSubject        AS CHAR
    FIELD cFrom           AS CHAR
    FIELD cFromEmail      AS CHAR
    FIELD cFromName       AS CHAR
    FIELD cTo             AS CHAR
    FIELD cCc             AS CHAR
    FIELD cBcc            AS CHAR
    FIELD cBody           AS CHAR
    FIELD iPriority       AS INT
    FIELD iAttachCount    AS INT
    FIELD cAttachments    AS CHAR
    FIELD iMsgId          AS INT
    FIELD bBodyRetrieved  AS LOG
    FIELD iMsgType        AS INT        /*Determines type of message, 1 = Plain 2 = HTML*/
    FIELD cEntityId       AS CHAR
    FIELD cFileName       AS CHAR FORMAT "x(40)"
    INDEX idxMsgId AS PRIMARY iMsgId.

DEF VAR httNewEmail       AS HANDLE NO-UNDO.
DEF VAR httNewEmailBuffer AS HANDLE NO-UNDO.
httNewEmailBuffer = BUFFER ttNewEmail:HANDLE.
httNewEmail       = httNewEmailBuffer:TABLE-HANDLE.

DEFINE STREAM s1.
DEFINE VARIABLE iLastMsgId AS INTEGER NO-UNDO INIT 0. /*Used to deal with retrievals.*/
&SCOPED-DEFINE MAILBOXLIB "ThreadMailBox.dll"
DEFINE VARIABLE bSingleMsg    AS LOGICAL   INIT FALSE.
DEFINE VARIABLE hGetMsgBtn    AS HANDLE    NO-UNDO.
DEFINE VARIABLE iThreadHandle AS INTEGER   NO-UNDO.
DEFINE VARIABLE iMsgCount     AS INTEGER   NO-UNDO.
DEFINE VARIABLE iSize         AS MEMPTR    NO-UNDO.
DEFINE VARIABLE ix            AS INTEGER   NO-UNDO.
DEFINE VARIABLE iReturn       AS INTEGER   NO-UNDO.

DEFINE VARIABLE cMailServer AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cPassword   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cUserName   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iAuthMethod AS INTEGER     NO-UNDO.
DEFINE VARIABLE iPort       AS INTEGER     NO-UNDO.

PROCEDURE CreateMailBox EXTERNAL {&MAILBOXLIB} 
    ORDINAL 1 PERSISTENT:
    DEF INPUT PARAMETER hHWND AS LONG.
    DEF RETURN PARAMETER hHandle AS LONG.
END PROCEDURE.

PROCEDURE SetPOPHostSettings EXTERNAL {&MAILBOXLIB}
    ORDINAL 3 :
    DEF INPUT  PARAMETER hHandle AS LONG.
    DEF INPUT  PARAMETER cHost AS CHAR.
    DEF INPUT  PARAMETER port  AS LONG.
    DEF RETURN PARAMETER iSuccess AS LONG.
END PROCEDURE.

PROCEDURE SetAuthentication EXTERNAL {&MAILBOXLIB}
    ORDINAL 5 :
    DEF INPUT  PARAMETER hHandle AS LONG.
    DEF INPUT  PARAMETER cUserId AS CHAR.
    DEF INPUT  PARAMETER cPass   AS CHAR.
    DEF RETURN PARAMETER iSuccess AS LONG.
END PROCEDURE.

PROCEDURE SetAuthType EXTERNAL {&MAILBOXLIB}
    ORDINAL 6 :
    DEF INPUT  PARAMETER hHandle AS LONG.
    DEF INPUT  PARAMETER iAuthType AS LONG.
    DEF RETURN PARAMETER iSuccess AS LONG.
END PROCEDURE.

PROCEDURE CheckForMessages EXTERNAL {&MAILBOXLIB}
    ORDINAL 22 :
    DEF INPUT  PARAMETER hHandle AS LONG.
    DEF RETURN PARAMETER iCount AS LONG.
END PROCEDURE.

PROCEDURE GetMessageHeader EXTERNAL {&MAILBOXLIB}
    ORDINAL 23 :
    DEF INPUT  PARAMETER hHandle AS LONG.
    DEF INPUT  PARAMETER iMsgId  AS LONG.
    DEF OUTPUT PARAMETER cFrom   AS CHAR.
    DEF OUTPUT PARAMETER cTo     AS CHAR.
    DEF OUTPUT PARAMETER cBcc    AS CHAR.
    DEF OUTPUT PARAMETER cCC     AS CHAR.
    DEF OUTPUT PARAMETER cReplyTo AS CHAR.
    DEF OUTPUT PARAMETER cSubject AS CHAR.
    DEF OUTPUT PARAMETER iAttachList AS CHAR.
    DEF OUTPUT PARAMETER iPriority  AS HANDLE TO LONG.
    DEF OUTPUT PARAMETER iAttachCount AS HANDLE TO LONG.
    DEF OUTPUT PARAMETER iDateSent    AS HANDLE TO LONG.
    DEF OUTPUT PARAMETER iTimeSent    AS HANDLE TO LONG.
    DEF RETURN PARAMETER iScucess AS LONG.
END PROCEDURE.

PROCEDURE SaveRecievedAttachment EXTERNAL {&MAILBOXLIB}
    ORDINAL 24 :
    DEF INPUT  PARAMETER hHandle AS LONG.
    DEF INPUT  PARAMETER iAttachIndex AS LONG.
    DEF INPUT  PARAMETER iMethode     AS LONG.
    DEF INPUT  PARAMETER cFileName    AS CHAR.
    DEF INPUT  PARAMETER mpBlob       AS MEMPTR.
    DEF INPUT  PARAMETER iBlobSize    AS LONG.
    DEF RETURN PARAMETER iSuccess     AS LONG.
END PROCEDURE.


PROCEDURE RequestMessageAsync EXTERNAL {&MAILBOXLIB}
    ORDINAL 25 :
    DEF INPUT  PARAMETER hHandle AS LONG.
    DEF INPUT  PARAMETER iMsgId AS LONG.
    DEF INPUT  PARAMETER icSize     AS MEMPTR.
    DEF RETURN PARAMETER iSuccess     AS LONG.
END PROCEDURE.

PROCEDURE GetMessageContent EXTERNAL {&MAILBOXLIB}
    ORDINAL 26 :
    DEF INPUT  PARAMETER hHandle AS LONG.
    DEF OUTPUT PARAMETER cFrom   AS CHAR.
    DEF OUTPUT PARAMETER cTo     AS CHAR.
    DEF OUTPUT PARAMETER cBcc    AS CHAR.
    DEF OUTPUT PARAMETER cCC     AS CHAR.
    DEF OUTPUT PARAMETER cReplyTo AS CHAR.
    DEF OUTPUT PARAMETER cSubject AS CHAR.
    DEF OUTPUT PARAMETER iAttachList AS CHAR.
    DEF OUTPUT PARAMETER iPriority  AS HANDLE TO LONG.
    DEF OUTPUT PARAMETER iDateSent  AS HANDLE TO LONG.
    DEF OUTPUT PARAMETER iTimeSent  AS HANDLE TO LONG.
    DEF OUTPUT PARAMETER iAttachCount AS HANDLE TO LONG.
    DEF OUTPUT PARAMETER cBody    AS CHAR.
    DEF OUTPUT PARAMETER iMsgType     AS HANDLE TO LONG.
    DEF RETURN PARAMETER iScucess AS LONG.
END PROCEDURE.

PROCEDURE OpenConnection EXTERNAL {&MAILBOXLIB}
    ORDINAL 27:
    DEF INPUT  PARAMETER hHandle AS LONG.
    DEF RETURN PARAMETER iSuccess AS LONG.
END PROCEDURE.

PROCEDURE CloseConnection EXTERNAL {&MAILBOXLIB}
    ORDINAL 28:
    DEF INPUT  PARAMETER hHandle AS LONG.
    DEF RETURN PARAMETER iSuccess AS LONG.
END PROCEDURE.

PROCEDURE DeleteMessage EXTERNAL {&MAILBOXLIB}
    ORDINAL 29:
    DEF INPUT  PARAMETER hHandle AS LONG.
    DEF INPUT  PARAMETER iMsgNum AS LONG.
    DEF RETURN PARAMETER iSuccess AS LONG.
END PROCEDURE.

PROCEDURE GenerateHTMLFromMail EXTERNAL {&MAILBOXLIB}
    ORDINAL 30:
    DEF INPUT  PARAMETER cFrom   AS CHAR.
    DEF INPUT  PARAMETER cTo     AS CHAR.
    DEF INPUT  PARAMETER cSubject AS CHAR.
    DEF INPUT  PARAMETER cAttachments AS CHAR.
    DEF INPUT  PARAMETER cBody        AS CHAR.
    DEF INPUT  PARAMETER cHTMLTemplate AS CHAR.
    DEF INPUT  PARAMETER cSaveHTMLAs   AS CHAR.
    DEF INPUT  PARAMETER iType         AS LONG.
    DEF INPUT  PARAMETER iDateSent     AS LONG.
    DEF INPUT  PARAMETER iTimeSent     AS LONG.
    DEF RETURN PARAMETER iSuccess      AS LONG.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME frmEmail

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD checkIfMailIsStored C-Win 
FUNCTION checkIfMailIsStored RETURNS LOGICAL
  ( INPUT ibCheckIfStored AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ConvertHtmlToText C-Win 
FUNCTION ConvertHtmlToText RETURNS LOGICAL
  ( INPUT ibConvHtmlToText AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD DeleteEmail C-Win 
FUNCTION DeleteEmail RETURNS LOGICAL
  ( INPUT iiMsgId AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getEmailServer C-Win 
FUNCTION getEmailServer RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getEmailTT C-Win 
FUNCTION getEmailTT RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getEmailUser C-Win 
FUNCTION getEmailUser RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLastLoadedDocIds C-Win 
FUNCTION getLastLoadedDocIds RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTTEmailBuffer C-Win 
FUNCTION getTTEmailBuffer RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTTNewEmailBuffer C-Win 
FUNCTION getTTNewEmailBuffer RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD OpenAttachment C-Win 
FUNCTION OpenAttachment RETURNS LOGICAL
  ( INPUT iiMsgId          AS INT,
    INPUT icAttachmentList AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SaveMailAsHTML C-Win 
FUNCTION SaveMailAsHTML RETURNS CHARACTER
  ( INPUT iiMsgId AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setChrPrLineHtmlToTxt C-Win 
FUNCTION setChrPrLineHtmlToTxt RETURNS LOGICAL
  ( INPUT iiChrPrLineHtmlToTxt AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setDocLoadContext C-Win 
FUNCTION setDocLoadContext RETURNS LOGICAL
  ( INPUT icDocLoadContext AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setMailServerProperties C-Win 
FUNCTION setMailServerProperties RETURNS LOGICAL
  ( INPUT icMailServer AS CHARACTER,
    INPUT iiPort       AS INTEGER,
    INPUT icUserName   AS CHARACTER,
    INPUT icPassword   AS CHARACTER,
    INPUT iiAuthMethod AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setSaveBodyMethod C-Win 
FUNCTION setSaveBodyMethod RETURNS LOGICAL
  ( INPUT icMethod AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setUploadAttAsync C-Win 
FUNCTION setUploadAttAsync RETURNS LOGICAL
  ( INPUT ibUploadAttAsync AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD UploadMailToDb C-Win 
FUNCTION UploadMailToDb RETURNS LOGICAL
  ( INPUT iiMsgId      AS INT,
    INPUT icEntity     AS CHAR,
    INPUT icEntityId   AS CHAR,
    INPUT icEntityText AS CHAR,
    INPUT ibDeleteMail AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME frmEmail
     "JLW simple Email listener" VIEW-AS TEXT
          SIZE 25 BY .62 AT ROW 1.81 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 27.6 BY 2.76.


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
         HEIGHT             = 2.76
         WIDTH              = 27.6
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 80
         SHOW-IN-TASKBAR    = no
         CONTROL-BOX        = no
         MIN-BUTTON         = no
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         MESSAGE-AREA       = no
         SENSITIVE          = no.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME frmEmail
   FRAME-NAME                                                           */
ASSIGN 
       FRAME frmEmail:HIDDEN           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME frmEmail
/* Query rebuild information for FRAME frmEmail
     _Query            is NOT OPENED
*/  /* FRAME frmEmail */
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


&Scoped-define SELF-NAME frmEmail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL frmEmail C-Win
ON A OF FRAME frmEmail
DO:
  DEFINE VARIABLE cAttachments AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cBcc         AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cBody        AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cCC          AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cFrom        AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cReplyTo     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cSubject     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cTo          AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iReturn      AS INTEGER   NO-UNDO.
  DEFINE VARIABLE ia           AS INTEGER   NO-UNDO.
  DEFINE VARIABLE iAttachCount AS INTEGER   NO-UNDO.
  DEFINE VARIABLE iPriority    AS INTEGER   NO-UNDO.
  DEFINE VARIABLE iDateSent    AS INTEGER   NO-UNDO.
  DEFINE VARIABLE iTimeSent    AS INTEGER   NO-UNDO.
  DEFINE VARIABLE iMsgType     AS INTEGER   NO-UNDO.
  DEF VAR bDeleted             AS LOG       NO-UNDO.

/*
  ASSIGN cFrom        = FILL(" ",255)
         cTo          = FILL(" ",255)
         cBcc         = FILL(" ",255)
         cCC          = FILL(" ",255)
         cReplyTo     = FILL(" ",255)
         cSubject     = FILL(" ",255)
         cAttachments = FILL(" ",255)
         cBody        = FILL(" ",MIN(31500,GET-LONG(iSize,5))).

  /*NOTE: cBody is prefilled with a size that is calculated by the dll.
          You must do this to ensure that the body will fit into the 
          variable.  "iSize 5" was passed into by the RequestMessageAsync
          procedure, thus it's value is know before the call to 
          GetMessageContent.                                              */


  IF GET-LONG(iSize,5) LE 31500 THEN DO:
    RUN GetMessageContent(iThreadHandle,
                          OUTPUT cFrom, OUTPUT cTo, OUTPUT cBcc,
                          OUTPUT cCC,   OUTPUT cReplyTo, OUTPUT cSubject,
                          OUTPUT cAttachments, OUTPUT iPriority,OUTPUT iDateSent,
                          OUTPUT iTimeSent,
                          OUTPUT iAttachCount, OUTPUT cBody, OUTPUT iMsgType, OUTPUT iReturn).
    IF iReturn NE 0 THEN
      MESSAGE "Could not retrieve message. Return code: " iReturn
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
  

    IF AVAIL ttNewEmail 
       AND ttNewEmail.cFrom     = cFrom
       AND ttNewEmail.dDateSent = DATE(iDateSent)
  /*      AND ttNewEmail.cTimeSent = cTime  */
       THEN DO:
      ASSIGN ttNewEmail.cAttachments   = TRIM(cAttachments)
             ttNewEmail.cBody          = TRIM(cBody)
             ttNewEmail.iAttachCount   = iAttachCount
             ttNewEmail.bBodyRetrieved = TRUE
             ttNewEmail.iMsgType       = iMsgType
             .
      IF iMsgType = 1 THEN
        ttNewEmail.cFileName = ttNewEmail.cFileName + ".txt". /* SUBSTR(ttNewEmail.cFileName,1,R-INDEX(ttNewEmail.cFileName,".")) + "txt". */
      ELSE
        ttNewEmail.cFileName = ttNewEmail.cFileName + ".htm".
  
      FIND FIRST ttEmail 
           WHERE ttEmail.iMsgId = ttNewEmail.iMsgId
           NO-ERROR.
      IF AVAIL ttEmail THEN
        BUFFER-COPY ttNewEmail TO ttEmail.
  
      RUN SaveEmailMessage.

      IF AVAIL ttNewEmail THEN
        DELETE ttNewEmail.
    END.
  END.
  ELSE IF AVAIL ttNewEmail THEN DO:
    IF DYNAMIC-FUNCTION("DoMessage",0,4,
                        IF DYNAMIC-FUNCTION("Scandinavian") THEN 
                          "Meldingsteksten er for stor for å kunne mottas for melding" + CHR(10)
                        + ttNewEmail.cSubject + CHR(10)
                        + "(Meldingen må videresendes som vedlegg til programmet)" + CHR(10)
                        + "Slett melding fra epostserver?"
                        ELSE
                          "The message body is too large for" + CHR(10)
                        + ttNewEmail.cSubject + CHR(10) 
                        + "(The message should be forwarded as attachment to the application)" + CHR(10)
                        + "Delete message from email server"
                       ,"","") = 6 THEN DO:
      DeleteEmail(ttNewEmail.iMsgId).               
      bDeleted = YES.
    END.

    IF AVAIL ttNewEmail THEN
      DELETE ttNewEmail NO-ERROR.
  END.

  FIND FIRST ttNewEmail NO-ERROR.
  IF AVAIL ttNewEmail THEN DO:
    bSuspend = YES.  
    PAUSE 1.
    RUN RequestMessageAsync(iThreadHandle,ttNewEmail.iMsgId,iSize,OUTPUT iReturn).
  END.
  ELSE DO:
    bSuspend = NO.
    IF bDeleted THEN PUBLISH "RefreshEmailQuery".
  END. 
*/
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
  DEF VAR iSuccess AS INT.
  RELEASE EXTERNAL PROCEDURE {&MAILBOXLIB}.
  RUN disable_UI.
  DELETE PROCEDURE THIS-PROCEDURE.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  hParent = SOURCE-PROCEDURE.

  
  IF SEARCH("ThreadMailBox.dll") = ? THEN DO:
    MESSAGE "Dll program for reading email messages not installed"
             VIEW-AS ALERT-BOX WARNING BUTTONS OK.
    RETURN.
  END.

/*   RUN InitializeObject.  */

  THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = YES.
  
  SUBSCRIBE TO "InvalidateHandle" IN hParent.
  SUBSCRIBE TO "SuspendEmailClient" ANYWHERE.

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
  VIEW FRAME frmEmail IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frmEmail}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getEmailHeader C-Win 
PROCEDURE getEmailHeader :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER iiMsgId     AS INT NO-UNDO.
DEFINE OUTPUT PARAMETER cSubject    AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER cFrom       AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER dDateSent   AS DATE NO-UNDO.
DEFINE OUTPUT PARAMETER iTimeSent   AS INT NO-UNDO.

DEFINE VARIABLE cAttachments AS CHARACTER NO-UNDO.
DEFINE VARIABLE cBcc         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cBody        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCC          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cReplyTo     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTo          AS CHARACTER NO-UNDO.
DEFINE VARIABLE iAttachCount AS INTEGER   NO-UNDO.
DEFINE VARIABLE iPriority    AS INTEGER   NO-UNDO.
DEFINE VARIABLE iDateSent    AS INTEGER   NO-UNDO.
DEFINE VARIABLE iMsgCount    AS INTEGER   NO-UNDO.
DEFINE VARIABLE iy           AS INTEGER   NO-UNDO.
DEF VAR         iCntNewMsg   AS INT       NO-UNDO.

IF bSuspend THEN RETURN.


ASSIGN cFrom        = FILL(" ",255)
       cTo          = FILL(" ",255)
       cBcc         = FILL(" ",255)
       cCC          = FILL(" ",255)
       cReplyTo     = FILL(" ",255)
       cSubject     = FILL(" ",255)
       cAttachments = FILL(" ",255)
       .
RUN GetMessageHeader(iThreadHandle,iiMsgId,
                      OUTPUT cFrom, OUTPUT cTo, OUTPUT cBcc,
                      OUTPUT cCC,   OUTPUT cReplyTo, OUTPUT cSubject,
                      OUTPUT cAttachments, OUTPUT iPriority,
                      OUTPUT iAttachCount, OUTPUT iDateSent, 
                      OUTPUT iTimeSent, OUTPUT iReturn).

dDateSent = DATE(iDateSent).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getSubjectList C-Win 
PROCEDURE getSubjectList :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAM ocSubjectList AS CHARACTER NO-UNDO.

DEFINE VARIABLE cAttachments AS CHARACTER NO-UNDO.
DEFINE VARIABLE cBcc         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cBody        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCC          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFrom        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cReplyTo     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSubject     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTo          AS CHARACTER NO-UNDO.
DEFINE VARIABLE iAttachCount AS INTEGER   NO-UNDO.
DEFINE VARIABLE iPriority    AS INTEGER   NO-UNDO.
DEFINE VARIABLE iDateSent    AS INTEGER   NO-UNDO.
DEFINE VARIABLE iTimeSent    AS INTEGER   NO-UNDO.
DEFINE VARIABLE iMsgCount    AS INTEGER   NO-UNDO.
DEFINE VARIABLE iy           AS INTEGER   NO-UNDO.
DEF VAR         iCntNewMsg   AS INT       NO-UNDO.

IF bSuspend THEN RETURN.


RUN CheckForMessages(iThreadHandle,OUTPUT iMsgCount).

/* The batch server (JBoxEmailImport.w) will keep track of last count */
PUBLISH "getLastEmailCount" (iMsgCount,OUTPUT iLastMsgCount).
  
IF iMsgCount > iLastMsgCount THEN DO:

  EMPTY TEMP-TABLE ttNewEmail.

  DO ix = iLastMsgCount + 1 TO iMsgCount: 
    ASSIGN cFrom        = FILL(" ",255)
           cTo          = FILL(" ",255)
           cBcc         = FILL(" ",255)
           cCC          = FILL(" ",255)
           cReplyTo     = FILL(" ",255)
           cSubject     = FILL(" ",255)
           cAttachments = FILL(" ",255)
           iLastMsgId   = iLastMsgId + 1.
    RUN GetMessageHeader(iThreadHandle,ix,
                          OUTPUT cFrom, OUTPUT cTo, OUTPUT cBcc,
                          OUTPUT cCC,   OUTPUT cReplyTo, OUTPUT cSubject,
                          OUTPUT cAttachments, OUTPUT iPriority,
                          OUTPUT iAttachCount, OUTPUT iDateSent, 
                          OUTPUT iTimeSent, OUTPUT iReturn).

    ocSubjectList = ocSubjectList + (IF ocSubjectList NE "" THEN "|" ELSE "")
                 + STRING(ix) + "|" + cSubject.

 END. 
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
DEF VAR ix              AS INT    NO-UNDO.
DEF VAR bOk             AS LOG    NO-UNDO.
DEF VAR hField          AS HANDLE NO-UNDO.
DEF VAR cUserPwd        AS CHAR   NO-UNDO.
DEF VAR bCompanySuper   AS LOG    NO-UNDO.

SET-SIZE(iSize) = 8.
RUN CreateMailBox(FRAME frmEmail:HWND,OUTPUT iThreadHandle).
HIDE FRAME frmEmail.

bOk = YES.

IF iThreadHandle > 0 AND iThreadHandle NE ? THEN DO:
  SUBSCRIBE TO "SuspendEmailListener" ANYWHERE.
  RUN SetPopHostSettings(iThreadHandle,cMailServer,iPort,OUTPUT iReturn).
  IF iReturn = 0 THEN DO:
    RUN SetAuthType(iThreadHandle,iAuthMethod,OUTPUT iReturn).
    IF iReturn = 0 THEN DO:

      IF cPassword = "" OR cPassword = ? /* OR DYNAMIC-FUNCTION("getUserLevel") = "super" */ THEN 
        DO ix = 1 TO 3:
          IF cPassword = "" OR cPassword = ? OR ix > 1 /* OR DYNAMIC-FUNCTION("getUserLevel") = "super" */ THEN
            RUN JBoxAskUserNamePwd.w (INPUT-OUTPUT cUserName,NO,cMailServer,OUTPUT bOk,OUTPUT cPassword).
          IF bOK THEN DO:
            RUN SetAuthentication(iThreadHandle,cUserName,cPassword,OUTPUT iReturn).
            RUN CheckForMessages(iThreadHandle,OUTPUT iMsgCount).
            IF iMsgCount NE -1 THEN LEAVE.
            ELSE DYNAMIC-FUNCTION("DoMessage",0,0,"Invalid password","","").
          END.
          ELSE LEAVE.
        END.
      ELSE DO:
        RUN SetAuthentication(iThreadHandle,cUserName,cPassword,OUTPUT iReturn).
        RUN CheckForMessages(iThreadHandle,OUTPUT iMsgCount).
      END.

      IF iMsgCount = -1 THEN DO:
        DYNAMIC-FUNCTION("DoMessage",0,0,"Unsuccessful connect to mail-server: Authentication error for username " + cUserName
                         ,"","").
        bOK = NO.
      END.
    END.
    ELSE 
      DYNAMIC-FUNCTION("DoMessage",0,0,"Failed to connect to mail-server: Failed to set authentication method." + CHR(10) + "Error:" + STRING(iReturn) + " for method " + STRING(iAuthMethod)
                       ,"","").
  END.
  ELSE 
    DYNAMIC-FUNCTION("DoMessage",0,0,"Failed to connect to mail-server: Failed to set POP host settings." + CHR(10) + "Error: " + STRING(iReturn) + " for mailserver " + cMailServer + ", port " + STRING(iPort)
                     ,"","").
END.
ELSE DO:
  DYNAMIC-FUNCTION("DoMessage",0,0,"Failed to connect to mail-server: Failed to create handle for email thread."
                   ,"","").
  bOK = NO.
  APPLY "close" TO THIS-PROCEDURE.
END.

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
DEF INPUT PARAM ihProcHandle AS HANDLE NO-UNDO.

IF ihProcHandle = hParent THEN
  APPLY "close" TO THIS-PROCEDURE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SuspendEmailListener C-Win 
PROCEDURE SuspendEmailListener :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ibSuspend AS LOG NO-UNDO.

bSuspend = ibSuspend.

IF ibSuspend THEN bFirstSuspend = YES.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION checkIfMailIsStored C-Win 
FUNCTION checkIfMailIsStored RETURNS LOGICAL
  ( INPUT ibCheckIfStored AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
bCheckIfStored = ibCheckIfStored.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ConvertHtmlToText C-Win 
FUNCTION ConvertHtmlToText RETURNS LOGICAL
  ( INPUT ibConvHtmlToText AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
bConvHtmlToText = ibConvHtmlToText.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION DeleteEmail C-Win 
FUNCTION DeleteEmail RETURNS LOGICAL
  ( INPUT iiMsgId AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR iReturn AS INT NO-UNDO.
RUN DeleteMessage(iThreadHandle,iiMsgId,OUTPUT iReturn).
IF iReturn = 0 THEN DO:
  FIND FIRST ttEmail 
       WHERE ttEmail.iMsgId = iiMsgId
       NO-ERROR.
  IF AVAIL ttEmail THEN DELETE ttEmail.

  FIND FIRST ttNewEmail 
       WHERE ttNewEmail.iMsgId = iiMsgId
       NO-ERROR.
  IF AVAIL ttNewEmail THEN DELETE ttNewEmail.

  FOR EACH ttEmail WHERE ttEmail.iMsgId > iiMsgId:
    ttEmail.iMsgId = ttEmail.iMsgId - 1.
  END.
  FOR EACH ttNewEmail WHERE ttNewEmail.iMsgId > iiMsgId:
    ttNewEmail.iMsgId = ttNewEmail.iMsgId - 1.
  END.

  RETURN YES.
END.

RETURN NO.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getEmailServer C-Win 
FUNCTION getEmailServer RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN cMailServer.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getEmailTT C-Win 
FUNCTION getEmailTT RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN httEmail.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getEmailUser C-Win 
FUNCTION getEmailUser RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN cUserName.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getLastLoadedDocIds C-Win 
FUNCTION getLastLoadedDocIds RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN cLastLoadedDocIds.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTTEmailBuffer C-Win 
FUNCTION getTTEmailBuffer RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
RETURN httEmailBuffer.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTTNewEmailBuffer C-Win 
FUNCTION getTTNewEmailBuffer RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
RETURN httNewEmailBuffer.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION OpenAttachment C-Win 
FUNCTION OpenAttachment RETURNS LOGICAL
  ( INPUT iiMsgId          AS INT,
    INPUT icAttachmentList AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  icAttachmentList should be ; separated
------------------------------------------------------------------------------*/
DEF VAR ix        AS INT NO-UNDO.
DEF VAR mBlob     AS MEMPTR  NO-UNDO.  
DEF VAR iSuccess  AS INT     NO-UNDO.


DEF BUFFER bttEmail FOR ttEmail.

FIND FIRST bttEmail
     WHERE bttEmail.iMsgId = iiMsgId
     NO-ERROR.
IF NOT AVAIL bttEmail THEN RETURN NO.

SET-SIZE(mBlob) = 8.

IF AVAIL bttEmail THEN DO ix = 1 TO NUM-ENTRIES(bttEmail.cAttachments,";"):
  IF LOOKUP(ENTRY(ix,bttEmail.cAttachments,";"),icAttachmentList,";") > 0 THEN DO:
    RUN SaveRecievedAttachment(iThreadHandle
                              ,ix       /*Attachment index*/
                              ,1        /*Save type. 1 = file, 2 = blob.*/
                              ,SESSION:TEMP-DIRECTORY + ENTRY(ix,bttEmail.cAttachments,";")
                              ,mBlob
                              ,0
                              ,OUTPUT iSuccess
                              ).
    IF iSuccess = 0 THEN
      DYNAMIC-FUNCTION("setWebDoc","",SESSION:TEMP-DIRECTORY + ENTRY(ix,bttEmail.cAttachments,";")).
  END.
END.

SET-SIZE(mBlob) = 0.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SaveMailAsHTML C-Win 
FUNCTION SaveMailAsHTML RETURNS CHARACTER
  ( INPUT iiMsgId AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cBody       AS CHAR NO-UNDO.
DEF VAR cReturnChar AS CHAR NO-UNDO.
DEF VAR iSuccess    AS INT  NO-UNDO.
DEF VAR cLF         AS CHAR NO-UNDO.
DEF VAR cFileName   AS CHAR NO-UNDO.

DEF BUFFER bttEmail FOR ttEmail.

FIND FIRST bttEmail
     WHERE bttEmail.iMsgId = iiMsgId
     NO-ERROR.
IF AVAIL bttEmail THEN DO:
    
  IF bttEmail.iMsgType = 1 THEN  /* Text */
     ASSIGN cLF   = CHR(10).
/*             cBody = REPLACE(bttEmail.cBody,cReturnChar,"<div></div>") */
/*             .                                                         */
  ELSE 
    ASSIGN cLF = "<br>"
/*            cBody = bttEmail.cBody */
           .

  cFileName = SESSION:TEMP-DIRECTORY + bttEmail.cFileName.
        
  IF cFileName = "" THEN RETURN "".

  OUTPUT STREAM s1 TO VALUE(cFileName).
  PUT STREAM s1 UNFORMATTED 
      (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Dato: " ELSE "Date: ") 
         + STRING(bttEmail.dDateSent) + " " + STRING(bttEmail.iTimeSent,"HH:MM") + cLF
    + (IF bttEmail.iMsgType = 2 THEN "<strong>" ELSE cLF)
      + bttEmail.cSubject 
      + (IF bttEmail.iMsgType = 2 THEN "</strong>" ELSE cLF) + cLF
    + (IF bttEmail.cFrom NE "" THEN (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Fra: " ELSE "From: ") + bttEmail.cFrom + cLF ELSE "") 
    + (IF bttEmail.cTo NE "" THEN (IF DYNAMIC-FUNCTION("Scandinavian") THEN "Til: " ELSE "  To: ") + bttEmail.cTo + cLF ELSE "") 
    + (IF bttEmail.cCc NE "" THEN (IF DYNAMIC-FUNCTION("Scandinavian") THEN " CC: " ELSE "  CC: ") + bttEmail.cCc + cLF ELSE "")
    + (IF bttEmail.iMsgType = 2 THEN "<hr>" ELSE cLF)
    + (IF bttEmail.iMsgType = 2 AND R-INDEX(bttEmail.cBody,"</html>") > 0 THEN SUBSTR(bttEmail.cBody,1,R-INDEX(bttEmail.cBody,"</html>") + 7) ELSE bttEmail.cBody)
      .
  OUTPUT STREAM s1 CLOSE.

/*    RUN GenerateHTMLFromMail(ttEmail.cFrom,                */
/*                             ttEmail.cTo,                  */
/*                             ttEmail.cSubject,             */
/*                             "Attachments",                */
/*                             cBody,                        */
/*                             "T:\prowrk\Mec\Template.htm", */
/*                             "T:\prowrk\Mec\email.htm",0,  */
/*                             INTEGER(ttEmail.dDateSent),   */
/*                             ttEmail.iTimeSent * 1000,     */
/*                             OUTPUT iSuccess).             */

END.
  
RETURN cFileName.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setChrPrLineHtmlToTxt C-Win 
FUNCTION setChrPrLineHtmlToTxt RETURNS LOGICAL
  ( INPUT iiChrPrLineHtmlToTxt AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
iChrPrLineHtmlToTxt = iiChrPrLineHtmlToTxt.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setDocLoadContext C-Win 
FUNCTION setDocLoadContext RETURNS LOGICAL
  ( INPUT icDocLoadContext AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
cDocLoadContext = icDocLoadContext.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setMailServerProperties C-Win 
FUNCTION setMailServerProperties RETURNS LOGICAL
  ( INPUT icMailServer AS CHARACTER,
    INPUT iiPort       AS INTEGER,
    INPUT icUserName   AS CHARACTER,
    INPUT icPassword   AS CHARACTER,
    INPUT iiAuthMethod AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

IF icMailServer NE "" THEN
  cMailServer = icMailServer.

ASSIGN iPort       = iiPort
       cUserName   = icUserName
       cPassword   = icPassword
       iAuthMethod = iiAuthMethod
       .

IF iiPort = 0 THEN iiPort = 110.
IF iiAuthMethod = 0 THEN iAuthMethod = 1.
  
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setSaveBodyMethod C-Win 
FUNCTION setSaveBodyMethod RETURNS LOGICAL
  ( INPUT icMethod AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose: Set the timer method 
    Notes:  
------------------------------------------------------------------------------*/

cMethod = icMethod.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setUploadAttAsync C-Win 
FUNCTION setUploadAttAsync RETURNS LOGICAL
  ( INPUT ibUploadAttAsync AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
bUploadAttAsync = ibUploadAttAsync.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION UploadMailToDb C-Win 
FUNCTION UploadMailToDb RETURNS LOGICAL
  ( INPUT iiMsgId      AS INT,
    INPUT icEntity     AS CHAR,
    INPUT icEntityId   AS CHAR,
    INPUT icEntityText AS CHAR,
    INPUT ibDeleteMail AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cFileList AS CHAR    NO-UNDO.
DEF VAR mBlob     AS MEMPTR  NO-UNDO.  
DEF VAR iSuccess  AS INT     NO-UNDO.


DEF BUFFER bttEmail FOR ttEmail.

FIND FIRST bttEmail 
     WHERE bttEmail.iMsgId = iiMsgId
     NO-ERROR.
IF AVAIL bttEmail THEN DO:
  
  cFileList = SaveMailAsHTML(bttEmail.iMsgId). 

  IF cFileList = "" THEN DO:
    MESSAGE "Error in mail definition" SKIP
            "No temporary file name spesified"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN NO.
  END.

  SET-SIZE(mBlob) = 8.

  DO ix = 1 TO NUM-ENTRIES(bttEmail.cAttachments,";"):
    RUN SaveRecievedAttachment(iThreadHandle
                              ,ix       /*Attachment index*/
                              ,1        /*Save type. 1 = file, 2 = blob.*/
                              ,SESSION:TEMP-DIRECTORY + ENTRY(ix,bttEmail.cAttachments,";")
                              ,mBlob
                              ,0
                              ,OUTPUT iSuccess
                              ).
    IF iSuccess = 0 THEN
      cFileList = cFileList + (IF cFileList NE "" THEN ";" ELSE "") + SESSION:TEMP-DIRECTORY + ENTRY(ix,bttEmail.cAttachments,";").
  END.
  IF cFileList NE "" THEN DO:

    DYNAMIC-FUNCTION("setDocLoadFileDate",bttEmail.dDateSent,bttEmail.iTimeSent) NO-ERROR.
    DYNAMIC-FUNCTION("setDocLoadParam","linkdocs").

    IF DYNAMIC-FUNCTION("LoadDocs",cFileList,icEntity,icEntityId,icEntityText) THEN DO:
      cLastLoadedDocIds = DYNAMIC-FUNCTION("getOutParam").
      IF ibDeleteMail THEN
        DeleteEmail(iiMsgId).  
    END.
    ELSE ASSIGN iSuccess          = -1
                cLastLoadedDocIds = ""
                .
  END.
END.
  
SET-SIZE(mBlob) = 0.

RETURN iSuccess = 0.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

