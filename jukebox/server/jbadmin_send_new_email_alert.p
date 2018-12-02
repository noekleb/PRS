/* Send email alert message
   Created: 02-dec-12 by brynjar@chemistry.no
   NOTE: Must be run in Client-Server mode..
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR   NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR   NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR   NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG    NO-UNDO.

DEFINE VAR hQuery            AS HANDLE NO-UNDO.
DEFINE VAR cUserId           AS CHAR   NO-UNDO.
DEFINE VAR ix                AS INT    NO-UNDO.
DEFINE VAR cBody             AS CHAR   NO-UNDO.
DEFINE VAR cFrom             AS CHAR   NO-UNDO.
DEFINE VAR cTo               AS CHAR   NO-UNDO.
DEFINE VAR cSubject          AS CHAR   NO-UNDO.
DEFINE VAR iProcessStepId    AS INT    NO-UNDO.
DEFINE VAR iProcessTriggerId AS INT    NO-UNDO.
DEFINE VAR cAttachments      AS CHAR   NO-UNDO.

ASSIGN cUserId    = DYNAMIC-FUNCTION("getASuserId" IN SOURCE-PROCEDURE)
       .

DO ix = 1 TO NUM-ENTRIES(icParam,";"):
  CASE ENTRY(1,ENTRY(ix,icParam,";"),"|"):
    WHEN "processStep"    THEN iProcessStepId    = INTEGER(ENTRY(2,ENTRY(ix,icParam,";"),"|")) NO-ERROR.
    WHEN "processTrigger" THEN iProcessTriggerId = INTEGER(ENTRY(2,ENTRY(ix,icParam,";"),"|")) NO-ERROR.
  END CASE.
END.

FOR EACH JBoxProcessStepParam NO-LOCK
    WHERE JBoxProcessStepParam.iJBoxProcessStepId = iProcessStepId
      AND (JBoxProcessStepParam.iJBoxProcessRelId = iProcessTriggerId OR JBoxProcessStepParam.iJBoxProcessRelId = 0)
    :
  CASE JBoxProcessStepParam.cParameterName:
    WHEN "MailSubject" THEN cSubject = JBoxProcessStepParam.cParameterText.
    WHEN "MailTo"      THEN cTo      = cTo + (IF cTo NE "" THEN "," ELSE "") + JBoxProcessStepParam.cParameterText.
  END CASE.
END.

IF cTo = "" THEN DO:
  ocReturn = "No recipients defined".
  RETURN.
END.


CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).

hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME).
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  FIND FIRST JBoxEmailInbox NO-LOCK
       WHERE JBoxEmailInbox.iJBoxEmailInboxId = INTEGER(ihBuffer:BUFFER-FIELD("iJBoxEmailInboxId"):BUFFER-VALUE)
       NO-ERROR.
  IF AVAIL JBoxEmailInbox THEN DO:

    FIND FIRST JBoxEmailAccount NO-LOCK
         OF JBoxEmailInbox
         NO-ERROR.
    IF AVAIL JBoxEmailAccount THEN DO:
      cFrom = JBoxEmailAccount.cEmailAddress.
      IF OPSYS = "WIN32" THEN DO:
        cAttachments = "".
        FOR EACH JBoxDocRel NO-LOCK
            WHERE JBoxDocRel.cContext = "JBoxEmailInbox"
              AND JBoxDocRel.cEntityId = STRING(JBoxEmailInbox.iJBoxEmailInboxId)
           ,FIRST JBoxDocument FIELDS(cFileName cFullPathName) NO-LOCK
                  OF JBoxDocRel
            BY JBoxDocRel.iJBoxDocumentId DESC:

          IF SEARCH(JBoxDocument.cFullPathName) NE ? THEN DO:
            IF JBoxDocument.cFullPathName MATCHES "*.gz" AND SEARCH("gzip.exe") NE ? THEN DO:
              OS-COMMAND SILENT VALUE(SEARCH("gzip.exe") + ' -df "' + JBoxDocument.cFullPathName + '"').
              cAttachments = cAttachments + (IF cAttachments NE "" THEN "," ELSE "") + SUBSTR(JBoxDocument.cFullPathName,1,LENGTH(JBoxDocument.cFullPathName) - 3).
            END.
            ELSE
              cAttachments = cAttachments + (IF cAttachments NE "" THEN "," ELSE "") + JBoxDocument.cFullPathName.
          END.
          ELSE IF SEARCH(SESSION:TEMP-DIR + JBoxDocument.cFileName) NE ? THEN
            cAttachments = cAttachments + (IF cAttachments NE "" THEN "," ELSE "") + SESSION:TEMP-DIR + JBoxDocument.cFileName.
        END.

        RUN JBoxSendBlatMail.p (
            cTo,
            JBoxEmailAccount.cEmailAddress,
            "",
            cSubject,
            "Fra: " + JBoxEmailInbox.cDisplayFrom + CHR(10)
          + "Emne: " + JBoxEmailInbox.cSubject,
            cAttachments,
            JBoxEmailAccount.cMailServer,
            JBoxEmailAccount.cEmailAddress,
            JBoxEmailAccount.cPassword,
            "",
            "",
            "",
            "",
            NO,
            NO,
            ?,
            OUTPUT obOK,
            OUTPUT ocReturn
            ).
      END.
    END.
  END.
  hQuery:GET-NEXT().
END.
  
  /*
    DEF INPUT  PARAM icRecipientList    AS CHAR   NO-UNDO.
    DEF INPUT  PARAM icFrom             AS CHAR   NO-UNDO.
    DEF INPUT  PARAM icReplyTo          AS CHAR   NO-UNDO.
    DEF INPUT  PARAM icSubject          AS CHAR   NO-UNDO.
    DEF INPUT  PARAM icBody             AS CHAR   NO-UNDO.
    DEF INPUT  PARAM icAttachments      AS CHAR   NO-UNDO.
    DEF INPUT  PARAM icMailServer       AS CHAR   NO-UNDO.
    DEF INPUT  PARAM icEmailUserName    AS CHAR   NO-UNDO.
    DEF INPUT  PARAM icEmailPwd         AS CHAR   NO-UNDO.
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
    */


DELETE OBJECT hQuery.

obOk = ocReturn = "".                                                    

