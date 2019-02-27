/* Create file import log record. Called from jbdoc_savedoc.p
   Separation done so document handling can work without file import tables */
   
DEF INPUT   PARAM ihBuffer         AS HANDLE NO-UNDO.
DEF INPUT   PARAM icReturn         AS CHAR   NO-UNDO.
DEF INPUT   PARAM icTargetDBtable  AS CHAR   NO-UNDO.
DEF INPUT   PARAM icFirstKeyString AS CHAR   NO-UNDO.
DEF INPUT   PARAM icLastKeyString  AS CHAR   NO-UNDO.
DEF OUTPUT  PARAM oiImportLogId    AS INT    NO-UNDO.

DEF VAR cUserid AS CHAR NO-UNDO.
cUserid = DYNAMIC-FUNCTION("getAsUserId" IN SOURCE-PROCEDURE) NO-ERROR.

CREATE JBoxFileImportLog.
BUFFER JBoxFileImportLog:HANDLE:BUFFER-COPY(ihBuffer).
ASSIGN JBoxFileImportLog.dCreated                       = TODAY
       JBoxFileImportLog.iCreTime                       = TIME
       JBoxFileImportLog.cCreatedBy                     = cUserid
       JBoxFileImportLog.cErrorMessage                  = icReturn
       JBoxFileImportLog.cTargetDBtable                 = icTargetDBtable
       JBoxFileImportLog.cFirstKeyString                = icFirstKeyString
       JBoxFileImportLog.cLastKeyString                 = icLastKeyString
       JBoxFileImportLog.iJBoxFileImportHeaderId        = INT(ihBuffer:BUFFER-FIELD("cEntityId"):BUFFER-VALUE)
       ihBuffer:BUFFER-FIELD("cContext"):BUFFER-VALUE   = "JBoxFileImportLog"
       ihBuffer:BUFFER-FIELD("cEntityId"):BUFFER-VALUE  = STRING(JBoxFileImportLog.iJBoxFileImportLogId)
       oiImportLogId                                    = JBoxFileImportLog.iJBoxFileImportLogId
       .
