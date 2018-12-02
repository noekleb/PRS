/* Create entries in JBoxEmailInbox
   Created: 02-dec-11 by brynjar@chemistry.no
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihDataSet   AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEFINE VAR hBuffEmail        AS HANDLE NO-UNDO.
DEFINE VAR hBuffAttach       AS HANDLE NO-UNDO.
DEFINE VAR hBuffInboxEntries AS HANDLE NO-UNDO.
DEFINE VAR hQryEmail         AS HANDLE NO-UNDO.
DEFINE VAR hQryAttach        AS HANDLE NO-UNDO.
DEFINE VAR cDocIdList        AS CHAR   NO-UNDO.
DEFINE VAR cUserId           AS CHAR   NO-UNDO.
DEFINE VAR iAccountId        AS INT    NO-UNDO.
DEFINE VAR cLogFile          AS CHAR   NO-UNDO.


ASSIGN hBuffEmail        = ihDataSet:GET-BUFFER-HANDLE(1)
       hBuffAttach       = ihDataSet:GET-BUFFER-HANDLE(2)
       hBuffInboxEntries = ihDataSet:GET-BUFFER-HANDLE(3)
       cUserId           = DYNAMIC-FUNCTION("getASuserId" IN SOURCE-PROCEDURE)
       iAccountId        = INTEGER(ENTRY(1,icParam,"|"))
       .

IF NUM-ENTRIES(icParam,"|") > 1 THEN
  cLogFile = ENTRY(2,icParam,"|").

FUNCTION getAsUserId RETURNS CHARACTER ( ):    
  RETURN  cUserId.
END FUNCTION.

FUNCTION CreLog RETURNS LOGICAL (INPUT icLogText AS CHAR ):    
  IF cLogFile NE "" THEN DO:
    OUTPUT TO VALUE(cLogFile) APPEND.
    PUT UNFORMATTED TODAY " " STRING(TIME,"hh:mm:ss") " " icLogText  SKIP.
    OUTPUT CLOSE.
    RETURN YES.
  END.
  RETURN NO.
END FUNCTION.

CREATE QUERY hQryEmail.
hQryEmail:SET-BUFFERS(hBuffEmail).

CREATE QUERY hQryAttach.
hQryAttach:SET-BUFFERS(hBuffAttach).

hQryEmail:QUERY-PREPARE("FOR EACH " + hBuffEmail:NAME).
hQryEmail:QUERY-OPEN().
hQryEmail:GET-FIRST().
REPEAT WHILE NOT hQryEmail:QUERY-OFF-END:  
  FIND FIRST JBoxEmailInbox NO-LOCK
       WHERE JBoxEmailInbox.cFrom     = hBuffEmail:BUFFER-FIELD("cFrom"):BUFFER-VALUE
         AND JBoxEmailInbox.dDateSent = DATE(hBuffEmail:BUFFER-FIELD("dDateSent"):BUFFER-VALUE)
         AND JBoxEmailInbox.iTimeSent = INTEGER(hBuffEmail:BUFFER-FIELD("iTimeSent"):BUFFER-VALUE)
         AND JBoxEmailInbox.iJBoxEmailAccountId = INTEGER(icParam)
       NO-ERROR.
  IF NOT AVAIL JBoxEmailInbox THEN DO:
    CREATE JBoxEmailInbox.
    JBoxEmailInbox.iJBoxEmailAccountId = iAccountId.
    BUFFER JBoxEmailInbox:BUFFER-COPY(hBuffEmail) NO-ERROR.

    IF NOT ERROR-STATUS:ERROR THEN DO:

      hBuffInboxEntries:BUFFER-CREATE().
      hBuffInboxEntries:BUFFER-FIELD("iJBoxEmailInboxId"):BUFFER-VALUE = JBoxEmailInbox.iJBoxEmailInboxId.

      hQryAttach:QUERY-PREPARE("FOR EACH " + hBuffAttach:NAME + " WHERE iMsgId = " + STRING(hBuffEmail:BUFFER-FIELD("iMsgId"):BUFFER-VALUE)).
      hQryAttach:QUERY-OPEN().
      hQryAttach:GET-FIRST().
      REPEAT WHILE NOT hQryAttach:QUERY-OFF-END:
        CREATE JBoxDocument.
        ASSIGN JBoxDocument.cDescription    = hBuffAttach:BUFFER-FIELD("cFileName"):BUFFER-VALUE
               JBoxDocument.cFileName       = hBuffAttach:BUFFER-FIELD("cFileName"):BUFFER-VALUE
               JBoxDocument.cFullPathName   = hBuffAttach:BUFFER-FIELD("cFullPathName"):BUFFER-VALUE
               JBoxDocument.cFileType       = hBuffAttach:BUFFER-FIELD("cFileType"):BUFFER-VALUE
               JBoxDocument.blDocument      = hBuffAttach:BUFFER-FIELD("blAttachment"):BUFFER-VALUE
               JBoxDocument.iDocSize        = hBuffAttach:BUFFER-FIELD("iAttachmentSize"):BUFFER-VALUE
               JBoxDocument.dFileCreateDate = hBuffAttach:BUFFER-FIELD("dAttachCreated"):BUFFER-VALUE
               JBoxDocument.iFileCreateTime = hBuffAttach:BUFFER-FIELD("iAttachCreTime"):BUFFER-VALUE
               JBoxDocument.dFileModDate    = hBuffAttach:BUFFER-FIELD("dAttachModified"):BUFFER-VALUE
               JBoxDocument.iFileModTime    = hBuffAttach:BUFFER-FIELD("iAttachModTime"):BUFFER-VALUE
               JBoxDocument.dCreated        = TODAY
               JBoxDocument.cCreatedBy      = cUserId
               cDocIdList                   = cDocIdList + (IF cDocIdList NE "" THEN "," ELSE "") + STRING(JBoxDocument.iJBoxDocumentId)
               .

        CREATE JBoxDocRel.
        ASSIGN JBoxDocRel.iJBoxDocumentId = JBoxDocument.iJBoxDocumentId
               JBoxDocRel.cContext        = "JBoxEmailInbox"
               JBoxDocRel.cEntityId       = STRING(JBoxEmailInbox.iJBoxEmailInboxId)
               .

        CreLog("Saved attachment " + JBoxDocument.cFileName).
        hQryAttach:GET-NEXT().
      END.

      CREATE JBoxDocument.
      ASSIGN JBoxDocument.cDescription    = JBoxEmailInbox.cSubject
             JBoxDocument.cFileName       = hBuffEmail:BUFFER-FIELD("cFileName"):BUFFER-VALUE
             JBoxDocument.cFullPathName   = hBuffEmail:BUFFER-FIELD("cFullPathName"):BUFFER-VALUE
             JBoxDocument.cFileType       = IF hBuffEmail:BUFFER-FIELD("iMsgType"):BUFFER-VALUE = 1 THEN "txt" ELSE "htm"
             JBoxDocument.blDocument      = hBuffEmail:BUFFER-FIELD("blBody"):BUFFER-VALUE
             JBoxDocument.iDocSize        = hBuffEmail:BUFFER-FIELD("iBodySize"):BUFFER-VALUE
             JBoxDocument.dFileCreateDate = JBoxEmailInbox.dDateSent
             JBoxDocument.iFileCreateTime = JBoxEmailInbox.iTimeSent
             JBoxDocument.dCreated        = TODAY
             JBoxDocument.cCreatedBy      = cUserId
             cDocIdList = STRING(JBoxDocument.iJBoxDocumentId) + (IF cDocIdList NE "" THEN "," ELSE "") + cDocIdList.
             .
      CREATE JBoxDocRel.
      ASSIGN JBoxDocRel.iJBoxDocumentId = JBoxDocument.iJBoxDocumentId
             JBoxDocRel.cContext        = "JBoxEmailInbox"
             JBoxDocRel.cEntityId       = STRING(JBoxEmailInbox.iJBoxEmailInboxId)
             .

      IF NUM-ENTRIES(cDocIdList) > 1 THEN
        RUN jbdoc_cre_doclink.p (cDocIdList).
      cDocIdList = "".
    END.
    ELSE DO:
      ocReturn = "FAILED to create email inbox entries".
      LEAVE.
    END.
  END.
  hQryEmail:GET-NEXT().
END.

DELETE OBJECT hQryEmail.
DELETE OBJECT hQryAttach.

obOk = ocReturn = "".                                                    

