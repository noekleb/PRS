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

&SCOPED-DEFINE MAILBOXLIB "ThreadMailBox.dll"
DEF VAR hThreadHandle   AS INT NO-UNDO.
DEF VAR counter         AS INT.
DEF VAR iSize           AS MEMPTR NO-UNDO.
DEF VAR iMsgCount       AS INT NO-UNDO.
DEF VAR bSingleMsg      AS LOGICAL INIT FALSE.

DEF VAR iReturn       AS INT    NO-UNDO.
DEF VAR hPanel        AS HANDLE NO-UNDO.

DEF VAR cMailServer   AS CHAR NO-UNDO.
DEF VAR iPort         AS INT  NO-UNDO.
DEF VAR cUserName     AS CHAR NO-UNDO.
DEF VAR cPassword     AS CHAR NO-UNDO.
DEF VAR iAuthMethod   AS INT  NO-UNDO.

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
    DEF OUTPUT PARAMETER iAttachCount AS HANDLE TO LONG.
    DEF OUTPUT PARAMETER cBody    AS CHAR.
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS brwInbox tbInbox rectEmailBodyPanel cSubject ~
cFrom cTo cCc cBcc cAttachments 
&Scoped-Define DISPLAYED-OBJECTS cSubject cFrom cTo cCc cBcc cAttachments 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE cAttachments AS CHARACTER FORMAT "X(256)":U 
     LABEL "Attachments" 
     VIEW-AS FILL-IN 
     SIZE 94 BY 1 NO-UNDO.

DEFINE VARIABLE cBcc AS CHARACTER FORMAT "X(256)":U 
     LABEL "BCC" 
     VIEW-AS FILL-IN 
     SIZE 94 BY 1 NO-UNDO.

DEFINE VARIABLE cCc AS CHARACTER FORMAT "X(256)":U 
     LABEL "CC" 
     VIEW-AS FILL-IN 
     SIZE 94 BY 1 NO-UNDO.

DEFINE VARIABLE cFrom AS CHARACTER FORMAT "X(256)":U 
     LABEL "From" 
     VIEW-AS FILL-IN 
     SIZE 94 BY 1 NO-UNDO.

DEFINE VARIABLE cSubject AS CHARACTER FORMAT "X(256)":U 
     LABEL "Subject" 
     VIEW-AS FILL-IN 
     SIZE 94 BY 1 NO-UNDO.

DEFINE VARIABLE cTo AS CHARACTER FORMAT "X(256)":U 
     LABEL "To" 
     VIEW-AS FILL-IN 
     SIZE 94 BY 1 NO-UNDO.

DEFINE RECTANGLE brwInbox
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 108 BY 8.1.

DEFINE RECTANGLE rectEmailBodyPanel
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 94 BY 8.57.

DEFINE RECTANGLE tbInbox
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 9.8 BY .91.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     cSubject AT ROW 10.76 COL 13 COLON-ALIGNED
     cFrom AT ROW 11.86 COL 13 COLON-ALIGNED
     cTo AT ROW 12.95 COL 13 COLON-ALIGNED
     cCc AT ROW 14.05 COL 13 COLON-ALIGNED
     cBcc AT ROW 15.14 COL 13 COLON-ALIGNED
     cAttachments AT ROW 16.24 COL 13 COLON-ALIGNED
     brwInbox AT ROW 2.43 COL 2
     tbInbox AT ROW 1.24 COL 3
     rectEmailBodyPanel AT ROW 17.43 COL 15
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 110 BY 25.38.

DEFINE FRAME FRAME-A
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 98 ROW 8.14
         SIZE 11 BY 2
         TITLE "Frame A".


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
         HEIGHT             = 25.38
         WIDTH              = 110.8
         MAX-HEIGHT         = 25.52
         MAX-WIDTH          = 115
         VIRTUAL-HEIGHT     = 25.52
         VIRTUAL-WIDTH      = 115
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
ASSIGN FRAME FRAME-A:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR FRAME FRAME-A
                                                                        */
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


&Scoped-define SELF-NAME FRAME-A
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FRAME-A C-Win
ON A OF FRAME FRAME-A /* Frame A */
DO:
  DEFINE VARIABLE cAttachments AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cBcc         AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cBody        AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cCC          AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cFrom        AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cReplyTo     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cSubject     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cTo          AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i            AS INTEGER   NO-UNDO.
  DEFINE VARIABLE ia           AS INTEGER   NO-UNDO.
  DEFINE VARIABLE iAttachCount AS INTEGER   NO-UNDO.
  DEFINE VARIABLE iPriority    AS INTEGER   NO-UNDO.

  ASSIGN cFrom        = FILL(" ",255)
         cTo          = FILL(" ",255)
         cBcc         = FILL(" ",255)
         cCC          = FILL(" ",255)
         cReplyTo     = FILL(" ",255)
         cSubject     = FILL(" ",255)
         cAttachments = FILL(" ",255)
         cBody        = FILL(" ",GET-LONG(iSize,5))
         .

  /*NOTE: cBody is prefilled with a size that is calculated by the dll.
          You must do this to ensure that the body will fit into the 
          variable.  "iSize 5" was passed into by the RequestMessageAsync
          procedure, thus it's value is know before the call to 
          GetMessageContent.                                              */

DO WITH FRAME DEFAULT-frame:
  RUN GetMessageContent(hThreadHandle,
                        OUTPUT cFrom, OUTPUT cTo, OUTPUT cBcc,
                        OUTPUT cCC,   OUTPUT cReplyTo, OUTPUT cSubject,
                        OUTPUT cAttachments, OUTPUT iPriority,
                        OUTPUT iAttachCount, OUTPUT cBody, OUTPUT i).

  MESSAGE PROGRAM-NAME(1) SKIP
          cFrom
          VIEW-AS ALERT-BOX.
  hFieldMap:BUFFER-CREATE().
  ASSIGN hFieldMap:BUFFER-FIELD("cSubject"):BUFFER-VALUE     = cSubject
         hFieldMap:BUFFER-FIELD("cFrom"):BUFFER-VALUE        = cFrom
         hFieldMap:BUFFER-FIELD("cTo"):BUFFER-VALUE          = cTo
         hFieldMap:BUFFER-FIELD("cCc"):BUFFER-VALUE          = cCc
         hFieldMap:BUFFER-FIELD("cBcc"):BUFFER-VALUE         = cBcc
         hFieldMap:BUFFER-FIELD("cAttachments"):BUFFER-VALUE = cAttachments
         hFieldMap:BUFFER-FIELD("cBody"):BUFFER-VALUE        = cBody
         hFieldMap:BUFFER-FIELD("iAttachCount"):BUFFER-VALUE = iAttachCount
         .
/*

   /*IF bSingleMsg = FALSE THEN*/
        ASSIGN fMsgNum:SCREEN-VALUE = STRING(counter).
        ASSIGN fSize:SCREEN-VALUE = STRING(GET-LONG(iSize,1))
               fBodySize:SCREEN-VALUE = STRING(GET-LONG(iSize,5)).
   /*Manage enabling of navigation buttons.*/
   IF bSingleMsg = TRUE AND counter < iMsgCount THEN
       ASSIGN bnNext:SENSITIVE = TRUE.
   IF bSingleMsg = TRUE AND counter > 1 THEN
       ASSIGN bnPrevious:SENSITIVE = TRUE.
   IF bSingleMsg = TRUE THEN
       ASSIGN bnGetMsgs:SENSITIVE = TRUE
              bnCheckMsgs:SENSITIVE = TRUE
              bnDelete:SENSITIVE = TRUE.
   IF bSingleMsg = TRUE AND LENGTH(TRIM(cAttachments)) > 0 THEN
       ASSIGN bnAttachment:SENSITIVE = TRUE.
*/
   /*We only issue a continuous message when commanded from 
     GetMessages button. */
   IF counter < iMsgCount AND bSingleMsg = FALSE THEN DO:
       ASSIGN counter = counter + 1.
       RUN RequestMessageAsync(hThreadHandle,counter,iSize,OUTPUT ia).
   END.
   ELSE IF counter = iMsgCount AND bSingleMsg = FALSE THEN DO:
       RUN CloseConnection(hThreadHandle,OUTPUT ia).
/*        ASSIGN bnGetMsgs:SENSITIVE = TRUE  */
/*               bnPrevious:SENSITIVE = TRUE */
/*               bnNext:SENSITIVE = FALSE.   */
       RUN InvokeMethod(hBrowse,"OpenQuery").
   END.
       

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
  RUN enable_UI.

  SET-SIZE(iSize) = 8.
  RUN CreateMailBox(FRAME frame-A:HWND,OUTPUT hThreadHandle).
  HIDE FRAME frame-a.  

  ASSIGN cMailServer = "mail.chemistry.no"
         iPort       = 110
         cUserName   = "arkiv@chemistry.no"
         cPassword   = "James"
         iAuthMethod = 1
         .

  RUN SetPopHostSettings(hThreadHandle,cMailServer,iPort,OUTPUT iReturn).
  RUN SetAuthType(hThreadHandle,iAuthMethod,OUTPUT iReturn).
  RUN SetAuthentication(hThreadHandle,cUserName,cPassword,OUTPUT iReturn).
/*   RUN CheckForMessages(hThreadHandle,OUTPUT iMsgCount).  */
  

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AccountRecord C-Win 
PROCEDURE AccountRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

RUN JboxJlwDAccountSettings.w (INPUT-OUTPUT cMailServer,
                               INPUT-OUTPUT iPort,
                               INPUT-OUTPUT cUserName,
                               INPUT-OUTPUT cPassword,
                               INPUT-OUTPUT iAuthMethod,
                               OUTPUT bOk).

IF bOk THEN DO:
  RUN SetPopHostSettings(hThreadHandle,cMailServer,iPort,OUTPUT iReturn).
  RUN SetAuthType(hThreadHandle,iAuthMethod,OUTPUT iReturn).
  RUN SetAuthentication(hThreadHandle,cUserName,cPassword,OUTPUT iReturn).

  MESSAGE PROGRAM-NAME(1) SKIP
          iReturn
          VIEW-AS ALERT-BOX.
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
  DISPLAY cSubject cFrom cTo cCc cBcc cAttachments 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE brwInbox tbInbox rectEmailBodyPanel cSubject cFrom cTo cCc cBcc 
         cAttachments 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getMailRecord C-Win 
PROCEDURE getMailRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iMsgCount AS INT NO-UNDO.

DEFINE VARIABLE cAttachments AS CHARACTER NO-UNDO.
DEFINE VARIABLE cBcc         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cBody        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCC          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFrom        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cReplyTo     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSubject     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTo          AS CHARACTER NO-UNDO.
DEFINE VARIABLE i            AS INTEGER   NO-UNDO.
DEFINE VARIABLE ia           AS INTEGER   NO-UNDO.
DEFINE VARIABLE iAttachCount AS INTEGER   NO-UNDO.
DEFINE VARIABLE iPriority    AS INTEGER   NO-UNDO.


/* RUN CloseConnection(hThreadHandle,OUTPUT iReturn). */

RUN CheckForMessages(hThreadHandle,OUTPUT iMsgCount).

/* MESSAGE PROGRAM-NAME(1) SKIP     */
/*         "iMsgCount: " iMsgCount  */
/*         VIEW-AS ALERT-BOX.       */
IF iMsgCount > 0 THEN DO:
  counter = 1.
  IF counter <= iMsgCount THEN DO:
     ASSIGN bSingleMsg = TRUE.
     ASSIGN counter = counter + 1.
     RUN RequestMessageAsync(hThreadHandle,counter,iSize,OUTPUT iReturn).

     ASSIGN cFrom        = FILL(" ",255)
            cTo          = FILL(" ",255)
            cBcc         = FILL(" ",255)
            cCC          = FILL(" ",255)
            cReplyTo     = FILL(" ",255)
            cSubject     = FILL(" ",255)
            cAttachments = FILL(" ",255)
            cBody        = FILL(" ",GET-LONG(iSize,5))
            .

     RUN GetMessageContent(hThreadHandle,
                           OUTPUT cFrom, OUTPUT cTo, OUTPUT cBcc,
                           OUTPUT cCC,   OUTPUT cReplyTo, OUTPUT cSubject,
                           OUTPUT cAttachments, OUTPUT iPriority,
                           OUTPUT iAttachCount, OUTPUT cBody, OUTPUT i).

     MESSAGE PROGRAM-NAME(1) SKIP
             cFrom
             VIEW-AS ALERT-BOX.
  END.

/*
  RUN OpenConnection(hThreadHandle,OUTPUT iReturn).

  ASSIGN counter = 1.
  RUN RequestMessageAsync(hThreadHandle,counter,iSize,OUTPUT iReturn).
  IF iReturn = -1 THEN DO:
      RUN CloseConnection(hThreadHandle,OUTPUT iReturn).
      MESSAGE "Could not complete operation because" SKIP
              " of invalid Msg Number passed in.".
  END.
*/

  MESSAGE PROGRAM-NAME(1) SKIP
          counter
          VIEW-AS ALERT-BOX.

  RUN InvokeMethod(hBrowse,"OpenQuery").
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
DYNAMIC-FUNCTION("CreateStatusBar",THIS-PROCEDURE:CURRENT-WINDOW,"",0,YES,?).

DO WITH FRAME {&FRAME-NAME}:
  hBrowse = DYNAMIC-FUNCTION("NewBrowse"
           ,brwInbox:HANDLE
           ,100
           ,""
           ,"temp-table"
            + ";cSubject|CHARACTER|x(60)"
            + ";cFrom|CHARACTER|x(40)"
            + ";cTo|CHARACTER|x(40)"
            + ";cCc|CHARACTER|x(40)"
            + ";cBcc|CHARACTER|x(40)"
            + ";cAttachments|CHARACTER|x(40)"
            + ";cBody|CHARACTER|x(256)"
            + ";iPriority|INTEGER|>>9"
            + ";iAttachCount|INTEGER|>>9"
           ,"WHERE false"
           ,"").

  hBrowse:SEPARATORS = NO.
  hBrowse:HELP = "Inbox".

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"shadedRows","yes").

  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap"
          ,hBrowse:QUERY
          ,FRAME {&FRAME-NAME}:HANDLE
          ,"",""
          ,"cSubject,cFrom,cTo,cCc,cBcc,cAttachments",""
          ,"").

  DYNAMIC-FUNCTION("CreateObjectLink",hFieldMap,hBrowse).

  hToolbar = DYNAMIC-FUNCTION("NewToolBar"
            ,tbInbox:HANDLE
            ,"File"
            ,"getMail;Get mail;Get mail;;bmp\mail.bmp¤enable"
           + ",fileMessage;File message;File message;;bmp\save16e.bmp"
           + ",|Settings"
            ,"maxborder").

  DYNAMIC-FUNCTION("CreateObjectLink",hToolbar,hBrowse).

  DYNAMIC-FUNCTION("NewMenuBand",WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hToolbar,"placeholder1"))
          ,"Account;Account settings"
          ,"").

  RUN JBoxJlwSupIe.w PERSIST SET hPanel.
  RUN InitializeObject IN hPanel (rectEmailBodyPanel:HANDLE,"").
  SUBSCRIBE TO "InvalidateHandle" IN hPanel.
END.


DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,200,200,0,0).

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
DEF INPUT PARAM ihChild AS HANDLE NO-UNDO.

IF ihChild = hPanel THEN APPLY "close" TO THIS-PROCEDURE.

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

