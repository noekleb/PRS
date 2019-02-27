&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEF INPUT  PARAM icRecipientList    AS CHAR   NO-UNDO.
DEF INPUT  PARAM icFrom             AS CHAR   NO-UNDO.
DEF INPUT  PARAM icReplyTo          AS CHAR   NO-UNDO.
DEF INPUT  PARAM icSubject          AS CHAR   NO-UNDO.
DEF INPUT  PARAM icBody             AS CHAR   NO-UNDO.
DEF INPUT  PARAM icAttachments      AS CHAR   NO-UNDO.
DEF INPUT  PARAM icMailServer       AS CHAR   NO-UNDO.
DEF INPUT  PARAM icMailServerUser   AS CHAR   NO-UNDO.
DEF INPUT  PARAM icMailServerPwd    AS CHAR   NO-UNDO.
DEF INPUT  PARAM icEventType        AS CHAR   NO-UNDO. /* If blank no event is created */
DEF INPUT  PARAM icEventExtraText   AS CHAR   NO-UNDO. /* Event text will be email subject */
DEF INPUT  PARAM icContextRowidList AS CHAR   NO-UNDO. /* Link info: <table>;<rowid>,<table>;<rowid>,<table>.. */
                                                       /* If used the list must correspond with icRecipientList */
DEF INPUT  PARAM icContextKeyFields AS CHAR   NO-UNDO. /* Key fields for event link: <table>;<key fields>,<table>;<Key.. */
DEF INPUT  PARAM ibAutoSend         AS LOG    NO-UNDO.
DEF INPUT  PARAM ibViewLogFile      AS LOG    NO-UNDO.
DEF INPUT  PARAM ihResBuffer        AS HANDLE NO-UNDO.
DEF OUTPUT PARAM obOk               AS LOG    NO-UNDO.
DEF OUTPUT PARAM ocReturn           AS CHAR   NO-UNDO.

DEF VAR cCompanyArchiveEmail  AS CHAR   NO-UNDO.
DEF VAR cCompanyMailServer    AS CHAR   NO-UNDO.
DEF VAR cCompanySendFromEmail AS CHAR   NO-UNDO.
DEF VAR cCompanyReplyToEmail  AS CHAR   NO-UNDO.
DEF VAR cEmailUserName        AS CHAR   NO-UNDO.
DEF VAR hResQuery             AS HANDLE NO-UNDO.
DEF VAR hMenuItemSlettVedlegg AS HANDLE NO-UNDO.
DEF VAR ix                    AS INT    NO-UNDO.

DEF STREAM sToFile.
DEF STREAM sCcFile.
DEF STREAM sBccFile.
DEF STREAM sBatFile.

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

DEF TEMP-TABLE ttMailLog 
    FIELD cLogText AS CHAR
    FIELD iLine    AS INT
    .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-ProcessBlatLog) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ProcessBlatLog Procedure 
FUNCTION ProcessBlatLog RETURNS CHARACTER
  ( INPUT icMailLog AS CHAR,
    INPUT iiAttempt AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
DEF VAR cEmailAccountInfo AS CHAR NO-UNDO.

IF icMailServer = "" THEN
  icMailServer = DYNAMIC-FUNCTION("getFieldValues","JBoxSysParam",
                                  "WHERE bActive AND cSysParamName = 'CompanyMailServer' AND iJBoxCompanyId = " + DYNAMIC-FUNCTION("getCompany"),
                                  "cSysParamCharValue").
IF icMailServer = "" OR icMailServer = ? THEN DO:
  cEmailAccountInfo = DYNAMIC-FUNCTION("getFieldValues","JBoxEmailAccount",
                                                 "WHERE bActive AND cAccountName = 'SMTP mail'(Codemaster)",
                                                 "cMailServer,cUserName,cPassword").
  IF cEmailAccountInfo NE ? THEN 
    ASSIGN icMailServer     = ENTRY(1,cEmailAccountInfo,"|")
           icMailServerUser = ENTRY(2,cEmailAccountInfo,"|")
           icMailServerPwd  = ENTRY(3,cEmailAccountInfo,"|")
           .
END.
ELSE IF icMailServerUser = "" THEN DO:
  icMailServerUser = DYNAMIC-FUNCTION("getFieldValues","JBoxGenCode",
                                     "WHERE iJBoxCompanyId = " + DYNAMIC-FUNCTION("getCompany")
                                    + " AND cCodeType = 'EmailUserName'"
                                    + " AND cDescription = '" + DYNAMIC-FUNCTION("getASuserId") + "'",
                                     "cCodeValue").
  IF icMailServerUser = ? OR icMailServerUser = "" THEN
    icMailServerUser = DYNAMIC-FUNCTION("getFieldValues","JBoxSysParam",
                                 "WHERE bActive AND cSysParamName = 'CompanyMailUser' AND iJBoxCompanyId = " + DYNAMIC-FUNCTION("getCompany"),
                                 "cSysParamCharValue").
  IF icMailServerUser = ? THEN
    icMailServerUser = "".

  icMailServerPwd = DYNAMIC-FUNCTION("getFieldValues","JBoxGenCode",
                                     "WHERE iJBoxCompanyId = " + DYNAMIC-FUNCTION("getCompany")
                                    + " AND cCodeType = 'EmailUserPwd'"
                                    + " AND cDescription = '" + DYNAMIC-FUNCTION("getASuserId") + "'",
                                     "cCodeValue").
  IF icMailServerPwd = ? OR icMailServerPwd = "" THEN
    icMailServerPwd = DYNAMIC-FUNCTION("getFieldValues","JBoxSysParam",
                                 "WHERE bActive AND cSysParamName = 'CompanyMailPwd' AND iJBoxCompanyId = " + DYNAMIC-FUNCTION("getCompany"),
                                 "cSysParamCharValue").
  IF icMailServerPwd = ? THEN
    icMailServerPwd = "".
END.

IF icFrom = "" THEN
  icFrom = DYNAMIC-FUNCTION("getFieldValues","JBoxSysParam",
                            "WHERE bActive AND cSysParamName = 'CompanySendFromEmail' AND iJBoxCompanyId = " + DYNAMIC-FUNCTION("getCompany"),
                            "cSysParamCharValue").

IF icReplyTo = "" THEN
  icReplyTo = DYNAMIC-FUNCTION("getFieldValues","JBoxSysParam",
                               "WHERE bActive AND cSysParamName = 'CompanyReplyToEmail' AND iJBoxCompanyId = " + DYNAMIC-FUNCTION("getCompany"),
                               "cSysParamCharValue").
IF icReplyTo = ? THEN icReplyTo = "".

IF NOT VALID-HANDLE(ihResBuffer) THEN DO:
  ihResBuffer = BUFFER ttEmail:HANDLE.
  DO ix = 1 TO NUM-ENTRIES(icRecipientList):
    ihResBuffer:BUFFER-CREATE().
    ihResBuffer:BUFFER-FIELD("cAttachments"):BUFFER-VALUE = icAttachments.

    IF NUM-ENTRIES(ENTRY(ix,icRecipientList),";") > 1 THEN
      ASSIGN ihResBuffer:BUFFER-FIELD("cSendAs"):BUFFER-VALUE = ENTRY(1,ENTRY(ix,icRecipientList),";")
             ihResBuffer:BUFFER-FIELD("cEmail"):BUFFER-VALUE  = ENTRY(2,ENTRY(ix,icRecipientList),";")
             .
    ELSE
      ASSIGN ihResBuffer:BUFFER-FIELD("cSendAs"):BUFFER-VALUE = "To"
             ihResBuffer:BUFFER-FIELD("cEmail"):BUFFER-VALUE  = ENTRY(ix,icRecipientList)
             .
    IF NUM-ENTRIES(icContextRowidList) = NUM-ENTRIES(icRecipientList) THEN 
      ASSIGN ihResBuffer:BUFFER-FIELD("cSourceTable"):BUFFER-VALUE = ENTRY(1,ENTRY(ix,icContextRowidList),";")
             ihResBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE    = ENTRY(2,ENTRY(ix,icContextRowidList),";")
             .
  END.
END.

CREATE QUERY hResQuery.
hResQuery:SET-BUFFERS(ihResBuffer).

RUN CreateEmail.
IF ocReturn BEGINS "OS-ERROR" THEN
  RUN CreateEmail.

obOK = ocReturn = "".
DELETE OBJECT hResQuery NO-ERROR.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-CreateEmail) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CreateEmail Procedure 
PROCEDURE CreateEmail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR ix           AS INT  NO-UNDO.
DEF VAR cAdrList     AS CHAR NO-UNDO.
DEF VAR cAttachList  AS CHAR NO-UNDO.
DEF VAR cToFile      AS CHAR NO-UNDO.
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
DEF VAR iResult      AS INT  NO-UNDO.
DEF VAR cEventHdrId  AS CHAR NO-UNDO.
DEF VAR iOsErr       AS INT  NO-UNDO.

FILE-INFO:FILE-NAME = SEARCH("blat.exe").
IF FILE-INFO:FULL-PATHNAME = ? THEN DO:
  ocReturn = "Could not find Blat.exe mail program".
  RETURN.
END.
ELSE cBlatExe = '"' + FILE-INFO:FULL-PATHNAME + '"'.

IF icMailServer = ? THEN DO:
  ocReturn = IF DYNAMIC-FUNCTION("Scandinavian") THEN
               "Kan ikke sende epost direkte pga ukjent navn på epost-server"
             ELSE
               "Cannot send directly due to unknown address for email server".
  RETURN.
END.
IF icMailServerUser = ? THEN DO:
  ocReturn = IF DYNAMIC-FUNCTION("Scandinavian") THEN
               "Kan ikke sende epost direkte pga ukjent brukernavn for epost-server"
             ELSE
               "Cannot send directly due to unknown user name for email server".
  RETURN.
END.


IF icEventType NE "" AND icContextKeyFields NE "" THEN DO:
  IF NOT DYNAMIC-FUNCTION("runProc","jbadmin_make_eventlog.p",
                                                icEventType + "|"
                                              + "|||||"
                                              + icContextKeyFields + "|"
                                              + icEventExtraText
                                                ,ihResBuffer) THEN DO:
    ocReturn = DYNAMIC-FUNCTION("getTransactionMessage").
    RETURN.
  END.
  cEventHdrId = DYNAMIC-FUNCTION("getTransactionMessage").
END.

cBodyFile = SESSION:TEMP-DIR + "mailBody.txt".
OUTPUT TO VALUE(cBodyFile).
PUT UNFORMATTED icBody.
OUTPUT CLOSE.

cSubjectFile = SESSION:TEMP-DIR + "mailSubject.txt".
OUTPUT TO VALUE(cSubjectFile).
PUT UNFORMATTED icSubject + (IF cEventHdrId NE "" THEN " [" + cEventHdrId + "]" ELSE "").
OUTPUT CLOSE.

ASSIGN cBatFile = SESSION:TEMP-DIR + "blatmail.bat"
       cMailLog = SESSION:TEMP-DIR + "maillog.txt"
       ix       = 0
       .

IF ibAutoSend THEN 
  OUTPUT STREAM sBatFile TO VALUE(cBatFile).
ELSE DO:
  ASSIGN cToFile  = SESSION:TEMP-DIR + "mailTo.txt"
         cCcFile  = SESSION:TEMP-DIR + "mailCc.txt"
         cBccFile = SESSION:TEMP-DIR + "mailBcc.txt"
         .
  OUTPUT STREAM sToFile  TO VALUE(cToFile).
  OUTPUT STREAM sCcFile  TO VALUE(cCcFile).
  OUTPUT STREAM sBccFile TO VALUE(cBccFile).
END.

hResQuery:QUERY-PREPARE("FOR EACH " + ihResBuffer:NAME).
hResQuery:QUERY-OPEN().
hResQuery:GET-FIRST().

REPEAT WHILE NOT hResQuery:QUERY-OFF-END:
  ix = ix + 1.
  IF ibAutosend THEN DO:
    PUT STREAM sBatFile UNFORMATTED
        cBlatExe + " " + cBodyFile + " " 
      + "-server " + icMailServer + " "
      + "-f " + (IF icMailServerUser NE "" THEN icMailServerUser ELSE icFrom) + " "
      + (IF icMailServerPwd NE "" THEN "-u " + icMailServerUser + " -pw " + icMailServerPwd + " " ELSE "")
      + (IF icFrom NE "" THEN "-from " + icFrom + " " ELSE "")
      + (IF icReplyTo NE "" THEN "-replyto " + icReplyTo + " " ELSE "")
      + "-log '" + cMailLog + "' -timestamp "
      + (IF icSubject NE "" THEN "-sf " + cSubjectFile + " " ELSE "")
      + "-" + ihResBuffer:BUFFER-FIELD("cSendAs"):BUFFER-VALUE + " "
      + ihResBuffer:BUFFER-FIELD("cEmail"):BUFFER-VALUE + " "
      + (IF ihResBuffer:BUFFER-FIELD("cAttachments"):BUFFER-VALUE NE "" THEN "-attach " + REPLACE(ihResBuffer:BUFFER-FIELD("cAttachments"):BUFFER-VALUE,";",",") ELSE "")
      .
  END.
  ELSE DO:
    IF ihResBuffer:BUFFER-FIELD("cAttachments"):BUFFER-VALUE NE "" THEN DO ix = 1 TO NUM-ENTRIES(ihResBuffer:BUFFER-FIELD("cAttachments"):BUFFER-VALUE):
      IF NOT CAN-DO(cAttachList,ENTRY(ix,ihResBuffer:BUFFER-FIELD("cAttachments"):BUFFER-VALUE)) THEN 
        cAttachList = cAttachList + (IF cAttachList NE "" THEN "," ELSE "")
                    + ENTRY(ix,ihResBuffer:BUFFER-FIELD("cAttachments"):BUFFER-VALUE).
    END.
    CASE STRING(ihResBuffer:BUFFER-FIELD("cSendAs"):BUFFER-VALUE):
      WHEN "to" THEN DO:
        PUT STREAM sToFile  UNFORMATTED (IF bMailto THEN "," + CHR(10) ELSE "") + ihResBuffer:BUFFER-FIELD("cEmail"):BUFFER-VALUE.
        bMailTo = YES.
      END.
      WHEN "cc" THEN DO:
        PUT STREAM sCcFile  UNFORMATTED (IF bMailCc THEN "," + CHR(10) ELSE "") + ihResBuffer:BUFFER-FIELD("cEmail"):BUFFER-VALUE.
        bMailCc = YES.
      END.
      WHEN "bcc" THEN DO:
        PUT STREAM sBccFile UNFORMATTED (IF bMailBcc THEN "," + CHR(10) ELSE "") + ihResBuffer:BUFFER-FIELD("cEmail"):BUFFER-VALUE.
        bMailBcc = YES.
      END.
    END CASE.
  END.
  hResQuery:GET-NEXT().
END.

IF ibAutoSend THEN 
  OUTPUT STREAM sBatFile CLOSE.  
ELSE DO:
  OUTPUT STREAM sToFile  CLOSE.
  OUTPUT STREAM sCcFile  CLOSE.
  OUTPUT STREAM sBccFile CLOSE.

  OUTPUT STREAM sBatFile TO VALUE(cBatFile).
  PUT STREAM sBatFile UNFORMATTED
      cBlatExe + " " + cBodyFile + " " 
    + "-server " + icMailServer + " "
    + "-f " + (IF icMailServerUser NE "" THEN icMailServerUser ELSE icFrom) + " "
/*     + "-f " + icMailServerUser + " " */
    + (IF icMailServerPwd NE "" THEN "-u " + icMailServerUser + " -pw " + icMailServerPwd + " " ELSE "")
    + (IF icFrom NE "" THEN "-from " + icFrom + " " ELSE "")
    + (IF icReplyTo NE "" THEN "-replyto " + icReplyTo + " " ELSE "")
    + "-log '" + cMailLog + "' -timestamp "
    + (IF bMailTo  THEN "-tf " + cToFile  + " " ELSE "")
    + (IF bMailCc  THEN "-cf " + cCcFile  + " " ELSE "")
    + (IF bMailBcc THEN "-bf " + cBccFile + " " ELSE "")
    + (IF bMailBcc AND NOT bMailCc AND NOT bMailTo THEN "-ur " ELSE "")
    + (IF icSubject NE "" THEN "-sf " + cSubjectFile + " " ELSE "")
    + (IF cAttachList NE "" THEN "-attach " + cAttachList ELSE "")
      .
  OUTPUT STREAM sBatFile CLOSE.
END.  
SESSION:SET-WAIT-STATE("General").
OS-COMMAND SILENT VALUE(cBatFile).
iOsErr = OS-ERROR.
SESSION:SET-WAIT-STATE("").

IF ibViewLogFile THEN DO:
  IF SEARCH(cMailLog) NE ? THEN
    OS-COMMAND NO-WAIT notepad VALUE(cMailLog).
END.
ELSE DO:

  ocReturn = ProcessBlatLog(cMailLog,1).
  IF ocReturn = "retry" THEN DO:
    SESSION:SET-WAIT-STATE("General").
    OS-COMMAND SILENT VALUE(cBatFile).
    iOsErr = OS-ERROR.
    SESSION:SET-WAIT-STATE("").
    ocReturn = ProcessBlatLog(cMailLog,2).
  END.
  IF iOsErr NE 0 THEN
    ocReturn = ocReturn + CHR(10) + "OS-ERROR: " + STRING(iOsErr).

END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-ProcessBlatLog) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ProcessBlatLog Procedure 
FUNCTION ProcessBlatLog RETURNS CHARACTER
  ( INPUT icMailLog AS CHAR,
    INPUT iiAttempt AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cLogToday    AS CHAR NO-UNDO.
DEF VAR cLogLine     AS CHAR NO-UNDO.
DEF VAR ix           AS INT  NO-UNDO.
DEF VAR bStartLogImp AS LOG  NO-UNDO.
DEF VAR bStartDelLog AS LOG  NO-UNDO.
DEF VAR bBlatError   AS LOG  NO-UNDO.
DEF VAR bTimeOut     AS LOG  NO-UNDO.
DEF VAR cReturn      AS CHAR NO-UNDO.

IF SEARCH(icMailLog) = ? THEN RETURN "".

ASSIGN cLogToday = STRING(YEAR(TODAY)) + "." + STRING(MONTH(TODAY),"99") + "." + STRING(DAY(TODAY),"99")
       ix        = 0.
INPUT FROM VALUE(icMailLog).
REPEAT:
  IMPORT UNFORMATTED cLogLine.
  IF cLogLine BEGINS cLogToday THEN bStartLogImp = YES.
  IF bStartLogImp THEN DO:
    CREATE ttMailLog.
    ASSIGN ix                 = ix + 1
           ttMailLog.cLogText = cLogLine
           ttMailLog.iLine    = ix.
  END.
END.
INPUT CLOSE.

FOR EACH ttMailLog BY ttMailLog.iLine DESC:
  IF bStartDelLog OR ttMailLog.cLogText = "" THEN DELETE ttMailLog.
  ELSE IF ttMailLog.cLogText MATCHES "*Start of Session*" THEN bStartDelLog = YES.
END.

FOR EACH ttMailLog BY ttMailLog.iLine:
  IF (NOT ttMailLog.cLogText BEGINS cLogToday 
     AND NOT ttMailLog.cLogText BEGINS "Blat" 
     AND NOT ttMailLog.cLogText BEGINS "32-bit" 
     AND NOT ttMailLog.cLogText BEGINS "64-bit") OR
     ttMailLog.cLogText MATCHES "*): Error:*" 
     THEN
    bBlatError = YES.
  IF ttMailLog.cLogText MATCHES "*timed out*" THEN
    bTimeOut = YES.
  DISP ttMailLog.iLine
       ttMailLog.cLogText FORMAT "x(150)"
       WITH WIDTH 200.
END.

IF bBlatError THEN
  FOR EACH ttMailLog BY ttMailLog.iLine:
    cReturn = cReturn + ttMailLog.cLogText + CHR(10).
  END.
  
IF bTimeOut AND iiAttempt = 1 THEN cReturn = "retry".

RETURN cReturn.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

