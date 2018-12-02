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
  DEF VAR icMobilNr AS CHAR NO-UNDO INIT "9092 3875".
  DEF VAR icSubject AS CHAR NO-UNDO INIT "Emne".
  DEF VAR icMessage AS CHAR NO-UNDO INIT "Ny test fra Skotex".
  DEF VAR obOk      AS LOG  NO-UNDO.
  DEF VAR ocMessage AS CHAR NO-UNDO.
&ELSE
  DEF INPUT  PARAM icMobilNr AS CHAR NO-UNDO.
  DEF INPUT  PARAM icSubject AS CHAR NO-UNDO.
  DEF INPUT  PARAM icMessage AS CHAR NO-UNDO.
  DEF OUTPUT PARAM obOk      AS LOG  NO-UNDO.
  DEF OUTPUT PARAM ocMessage AS CHAR NO-UNDO.
&ENDIF

/* Local Variable Definitions ---                                       */

DEF VAR bOk         AS LOG    NO-UNDO.
DEF VAR ix          AS INT    NO-UNDO.
DEF VAR hBrowse     AS HANDLE NO-UNDO.
DEF VAR hQuery      AS HANDLE NO-UNDO.
DEF VAR hToolbar    AS HANDLE NO-UNDO.
DEF VAR hFieldMap   AS HANDLE NO-UNDO.

DEF VAR cMailServer      AS CHAR NO-UNDO.
DEF VAR cAuthorize       AS CHAR NO-UNDO.
DEF VAR cAuthType        AS CHAR NO-UNDO.   
DEF VAR cMailUser        AS CHAR NO-UNDO.
DEF VAR cMailPwd         AS CHAR NO-UNDO.
DEF VAR cMailProgram     AS CHAR NO-UNDO.
DEF VAR cSmsDomain       AS CHAR NO-UNDO.
DEF VAR cSMSReplyTo      AS CHAR NO-UNDO.
DEF VAR cSMSProvider     AS CHAR NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fiEmne edSMS fiChars fiNumMsg fiTil ~
fiProvider fiSvarTil fiDomain btnSend btnCancel tbVisLogg 
&Scoped-Define DISPLAYED-OBJECTS fiEmne edSMS fiChars fiNumMsg fiTil ~
fiProvider fiSvarTil fiDomain tbVisLogg 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCancel 
     LABEL "&Avbryt" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnSend AUTO-GO 
     LABEL "Send" 
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

DEFINE VARIABLE fiEmne AS CHARACTER FORMAT "X(256)":U 
     LABEL "Emne" 
     VIEW-AS FILL-IN 
     SIZE 52 BY 1 NO-UNDO.

DEFINE VARIABLE fiNumMsg AS INTEGER FORMAT "->,>>>>>9":U INITIAL 0 
     LABEL "Antall meldinger" 
     VIEW-AS FILL-IN 
     SIZE 7.2 BY 1 TOOLTIP "Antall meldinger" NO-UNDO.

DEFINE VARIABLE fiProvider AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 28 BY 1 NO-UNDO.

DEFINE VARIABLE fiSvarTil AS CHARACTER FORMAT "X(256)":U 
     LABEL "Svar til" 
     VIEW-AS FILL-IN 
     SIZE 21.2 BY 1 NO-UNDO.

DEFINE VARIABLE fiTil AS CHARACTER FORMAT "X(256)":U 
     LABEL "Til" 
     VIEW-AS FILL-IN 
     SIZE 21.2 BY 1 NO-UNDO.

DEFINE VARIABLE tbVisLogg AS LOGICAL INITIAL no 
     LABEL "Vis loggfil" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     fiEmne AT ROW 1.48 COL 8 COLON-ALIGNED
     edSMS AT ROW 2.91 COL 2.6 NO-LABEL
     fiChars AT ROW 9.38 COL 11.8 COLON-ALIGNED
     fiNumMsg AT ROW 9.38 COL 37 COLON-ALIGNED
     fiTil AT ROW 10.57 COL 11.8 COLON-ALIGNED
     fiProvider AT ROW 10.57 COL 33 COLON-ALIGNED NO-LABEL
     fiSvarTil AT ROW 11.67 COL 11.8 COLON-ALIGNED
     fiDomain AT ROW 11.67 COL 33 COLON-ALIGNED NO-LABEL
     btnSend AT ROW 12.81 COL 32.8
     btnCancel AT ROW 12.81 COL 47.8
     tbVisLogg AT ROW 12.91 COL 13.8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 63.2 BY 13.29
         DEFAULT-BUTTON btnSend CANCEL-BUTTON btnCancel.


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
         TITLE              = "Send SMS"
         HEIGHT             = 13.24
         WIDTH              = 62.8
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
  ASSIGN fiChars  = LENGTH(fiEmne:SCREEN-VALUE) + LENGTH(edSMS:SCREEN-VALUE)
         fiNumMsg = TRUNC(fiChars / 161,0) + 1
         .
  DISP fiChars fiNumMsg WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiEmne
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiEmne C-Win
ON VALUE-CHANGED OF fiEmne IN FRAME DEFAULT-FRAME /* Emne */
DO:
  ASSIGN fiChars  = LENGTH(fiEmne:SCREEN-VALUE) + LENGTH(edSMS:SCREEN-VALUE)
         fiNumMsg = TRUNC(fiChars / 161,0) + 1
         .
  DISP fiChars fiNumMsg WITH FRAME {&FRAME-NAME}.
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

/* MESSAGE "För tillfället stoppad" SKIP    */
/*         "Försök igen om en stund" SKIP   */
/*         "mvh Kenneth, Polygon, 41223320" */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK.   */
/*                                          */
/* RETURN.                                  */

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.

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
  DISPLAY fiEmne edSMS fiChars fiNumMsg fiTil fiProvider fiSvarTil fiDomain 
          tbVisLogg 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE fiEmne edSMS fiChars fiNumMsg fiTil fiProvider fiSvarTil fiDomain 
         btnSend btnCancel tbVisLogg 
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
  cReturn = DYNAMIC-FUNCTION("getFieldList","SysPara;Beskrivelse;Parameter1",
                             "WHERE SysHId = 50 AND SysGr = 50 AND ParaNr < 10 BY ParaNr").
  IF cReturn = "" OR NUM-ENTRIES(cReturn,"|") < 8 THEN DO:
    DYNAMIC-FUNCTION("DoMessage",0,0,"Parameteroppsett mangler for å sende SMS","","").
    APPLY "close" TO THIS-PROCEDURE.
    RETURN.
  END.
  DO ix = 1 TO NUM-ENTRIES(cReturn,"|") BY 2:
    CASE ENTRY(ix,cReturn,"|"):
      WHEN "Mailhub"     THEN cMailServer  = ENTRY(ix + 1,cReturn,"|").
      WHEN "DoAuth"      THEN cAuthorize   = ENTRY(ix + 1,cReturn,"|").
      WHEN "AuthType"    THEN cAuthType    = ENTRY(ix + 1,cReturn,"|").
      WHEN "User"        THEN cMailUser    = ENTRY(ix + 1,cReturn,"|").
      WHEN "Password"    THEN cMailPwd     = ENTRY(ix + 1,cReturn,"|").
      WHEN "Mailprogram" THEN cMailProgram = ENTRY(ix + 1,cReturn,"|").
      WHEN "SMSDomain"   THEN cSmsDomain   = ENTRY(ix + 1,cReturn,"|").
      WHEN "SMSProvider" THEN cSmsProvider = ENTRY(ix + 1,cReturn,"|").
      WHEN "SMSReplyTo"  THEN cSmsReplyTo  = ENTRY(ix + 1,cReturn,"|").
    END CASE.
  END.
  IF cSMSReplyTo  = "" THEN cSMSReplyTo = cMailUser.

  ASSIGN icMobilnr               = REPLACE(icMobilnr,' ','')
         fiEmne:SCREEN-VALUE     = icSubject
         fiTil:SCREEN-VALUE      = REPLACE(icMobilnr," ","")
         fiSvarTil:SCREEN-VALUE  = ENTRY(1,cSMSReplyTo,"@")
         fiProvider:SCREEN-VALUE = "@" + cSMSProvider
         fiDomain:SCREEN-VALUE   = "@" + cSmsDomain
         edSMS:SCREEN-VALUE      = icMessage
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
DEF VAR cReturn   AS CHAR NO-UNDO.
DEF VAR cLogFile  AS CHAR NO-UNDO.
DEFINE VARIABLE cc AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cCommandstring AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cLogfil AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cSMSbodyfil AS CHARACTER   NO-UNDO.
DO WITH FRAME {&FRAME-NAME}:
  IF cMailProgram = "SN_SMS" THEN DO:
    cLogfil = "C:\tmp\sendsms_" + USERID("skotex") + ".log".
    OUTPUT TO VALUE(cLogfil).
    OUTPUT CLOSE.
    cSMSbodyfil = "C:\tmp\smsbody_" + USERID("skotex") + ".log".
    OUTPUT TO VALUE(cSMSbodyfil).
    OUTPUT CLOSE.
    OUTPUT TO VALUE(cSMSbodyfil).
    PUT UNFORMATTED (IF TRIM(edSMS:SCREEN-VALUE) NE ? THEN TRIM(edSMS:SCREEN-VALUE) ELSE " ") SKIP.
    OUTPUT CLOSE.
/*     cCommandstring = "c:\polygon\prs\cmd\sendEmail.exe -q"   + ' '  + */
    cCommandstring = "c:\appdir\se\cmd\sendEmail.exe -q"   + ' '  +
                     "-f info@polygon.se -t"        + ' "'  +
                     TRIM(fiTil:SCREEN-VALUE) + fiProvider:SCREEN-VALUE     + '" '  +
                     "-s smtp.office365.com:587" + ' '  +
                     "-xu info@polygon.se"        + ' '  +
                     "-xp Uddeva11a -u"              + ' "' +
                     (IF fiEmne:SCREEN-VALUE NE ? THEN REPLACE(fiEmne:SCREEN-VALUE,'"'," ") ELSE " ")  + 
                     '"  -o message-file=' + cSMSbodyfil + ' -l ' + cLogfil.

/* MESSAGE cCommandstring                 */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK. */
/* OUTPUT TO "c:\tmp\smsmail.bat".      */
/* PUT UNFORMATTED cCommandstring SKIP. */
/* OUTPUT CLOSE.                        */
      
      OS-COMMAND SILENT VALUE(cCommandstring).
      INPUT FROM VALUE(cLogfil).
      IMPORT UNFORMATTED cc.
      INPUT CLOSE.
      IF cc MATCHES "*successfully*" THEN
          bOk = TRUE.
      IF bOk THEN cReturn = "SMS".
      IF tbVisLogg:CHECKED THEN DO:
        IF SEARCH(cLogfil) NE ? THEN DO:
          FILE-INFO:FILE-NAME = SEARCH(cLogfil).
          OS-COMMAND NO-WAIT notepad VALUE(FILE-INFO:FULL-PATHNAME).          
        END.
      END.
  END.
  ELSE IF cMailProgram = "blat" THEN DO:
    RUN JBoxSendBlatMail.p ("To;" + TRIM(fiTil:SCREEN-VALUE) + fiProvider:SCREEN-VALUE,
                            TRIM(fiSvarTil:SCREEN-VALUE) + fiDomain:SCREEN-VALUE,
                            "",
                            IF fiEmne:SCREEN-VALUE NE ? THEN fiEmne:SCREEN-VALUE ELSE "",
                            TRIM(edSMS:SCREEN-VALUE),
                            "",
                            cMailServer,
                            cMailUser,
                            cMailPwd,
                            "",
                            "",
                            "",
                            "",
                            NO,
                            tbVisLogg:CHECKED, /* Vis logg */
                            ?,
                            OUTPUT bOk,
                            OUTPUT cReturn
                            ).
    IF NOT bOk THEN
      DYNAMIC-FUNCTION("DoMessage",0,0,cReturn,"","").
  END.
  ELSE DO:
/*     bOK = DYNAMIC-FUNCTION("runProc","jbserv_call_smtpmail.p",                          */
/*                         cMailServer                                                     */
/*                 + "|" + TRIM(fiTil:SCREEN-VALUE) + fiProvider:SCREEN-VALUE    /* Til */ */
/*                 + "|" + TRIM(fiSvarTil:SCREEN-VALUE) + fiDomain:SCREEN-VALUE  /* Fra */ */
/*                       + "|||||" /* |CC|BCC|Attach|localfiles| */                        */
/*                       + fiEmne:SCREEN-VALUE                                             */
/*                 + "|" + edSMS:SCREEN-VALUE                                              */
/*                       + "||||" /* |cont.type|bodytype|importance| */                    */
/*                       + (IF cAuthorize = "1" THEN "YES" ELSE "NO")                      */
/*                 + "|" + cAuthType                                                       */
/*                 + "|" + cMailUser                                                       */
/*                 + "|" + cMailPwd                                                        */
/*                 + "|" + cMailProgram                                                    */
/*                        ,?).                                                             */

      RUN prssmtpmailv5_7a.p (
        /*mailhub    */   cMailServer /* cSMTPserver */,
        /*EmailTo    */   TRIM(fiTil:SCREEN-VALUE) + fiProvider:SCREEN-VALUE /* cMailReceiver */,
        /*EmailFrom  */   TRIM(fiSvarTil:SCREEN-VALUE) + fiDomain:SCREEN-VALUE /* cMailSender */,
        /*EmailCC    */   '',
        /*Attachments*/   "",
        /*LocalFiles */   "",
        /*Subject    */   fiEmne:SCREEN-VALUE /* cMailSubject */,
        /*Body       */   edSMS:SCREEN-VALUE  /* cMailBody    */,
        /*MIMEHeader */   "" /* cMailContentType */,
        /*BodyType   */   "",
        /*Importance */   0,
        /*L_DoAUTH   */   FALSE,
        /*C_AuthType */   "" /* cMailAuthType */,
        /*C_User     */   cMailUser,
        /*C_Password */   cMailPwd,
        /*oSuccessful*/  OUTPUT bOk,
        /*vMessage   */  OUTPUT cReturn) NO-ERROR.

/*     RUN jbserv_call_smtpmail.p (                                                       */
/*                        cMailServer                                                     */
/*                + "|" + TRIM(fiTil:SCREEN-VALUE) + fiProvider:SCREEN-VALUE    /* Til */ */
/*                + "|" + TRIM(fiSvarTil:SCREEN-VALUE) + fiDomain:SCREEN-VALUE  /* Fra */ */
/*                      + "|||||" /* |CC|BCC|Attach|localfiles| */                        */
/*                      + fiEmne:SCREEN-VALUE                                             */
/*                + "|" + edSMS:SCREEN-VALUE                                              */
/*                      + "||||" /* |cont.type|bodytype|importance| */                    */
/*                      + (IF cAuthorize = "1" THEN "YES" ELSE "NO")                      */
/*                + "|" + cAuthType                                                       */
/*                + "|" + cMailUser                                                       */
/*                + "|" + cMailPwd                                                        */
/*                + "|" + cMailProgram                                                    */
/*                       ,?                                                               */
/*                       ,""                                                              */
/*                       ,OUTPUT cReturn                                                  */
/*                       ,OUTPUT bOk                                                      */
/*                       ).                                                               */

    IF bOk THEN cReturn = REPLACE(cReturn,"Email","SMS").
    DYNAMIC-FUNCTION("DoMessage",0,0,cReturn,"","").
    IF tbVisLogg:CHECKED THEN DO:
      IF SEARCH("socketemail.log") NE ? THEN DO:
        FILE-INFO:FILE-NAME = SEARCH("socketemail.log").
        OS-COMMAND NO-WAIT notepad VALUE(FILE-INFO:FULL-PATHNAME).          
      END.
      ELSE DYNAMIC-FUNCTION("DoMessage",0,0,"Finner ikke socketmail.log","","").
    END.
  END.
END.

IF bOk THEN DO:
  ASSIGN obOk      = YES
         ocMessage = "SMS " + STRING(TODAY) + ", " + STRING(TIME,"HH:MM") + " " + fiTil:SCREEN-VALUE + CHR(10)
                   + fiEmne:SCREEN-VALUE + CHR(10)
                   + edSMS:SCREEN-VALUE
                   . 
/*   MESSAGE ocMessage                      */
/*       VIEW-AS ALERT-BOX INFO BUTTONS OK. */
  APPLY "close" TO THIS-PROCEDURE.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

