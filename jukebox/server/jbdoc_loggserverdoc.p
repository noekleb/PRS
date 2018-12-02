DEF INPUT  PARAM icFileName  AS CHAR NO-UNDO.
DEF INPUT  PARAM icContext   AS CHAR NO-UNDO.
DEF INPUT  PARAM icEntityId  AS CHAR NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOk        AS LOG  NO-UNDO.

DEF VAR cASuserid      AS CHAR NO-UNDO.

DEF VAR hBuffLoginSess AS HANDLE NO-UNDO.

IF icSessionId NE "validsession" THEN DO:
  CREATE BUFFER hBuffLoginSess FOR TABLE "JBoxLoginSession" NO-ERROR.
  IF NOT ERROR-STATUS:ERROR THEN DO:
    IF hBuffLoginSess:FIND-FIRST("WHERE cSessionId = '" + icSessionId + "'",NO-LOCK) THEN
      cASuserId = hBuffLoginSess:BUFFER-FIELD("cJBoxUserId"):BUFFER-VALUE.
  END.
  ELSE cASuserId = "unknown".
END.
ELSE cASuserId = USERID(LDBNAME(1)).

FILE-INFO:FILE-NAME = icFileName.
CREATE JBoxDocument.
ASSIGN JBoxDocument.cFileName       = IF OPSYS = "unix" THEN SUBSTR(icFileName,R-INDEX(icFileName,"/") + 1) ELSE SUBSTR(icFileName,R-INDEX(icFileName,"\") + 1)
       JBoxDocument.cFullPathName   = FILE-INFO:FULL-PATHNAME
       JBoxDocument.cFileType       = SUBSTR(icFileName,R-INDEX(icFileName,".") + 1)
       JBoxDocument.cDescription    = icContext
       JBoxDocument.iDocSize        = FILE-INFO:FILE-SIZE
       JBoxDocument.dFileCreateDate = FILE-INFO:FILE-CREATE-DATE
       JBoxDocument.iFileCreateTime = FILE-INFO:FILE-CREATE-TIME
       JBoxDocument.dFileModDate    = FILE-INFO:FILE-MOD-DATE
       JBoxDocument.iFileModTime    = FILE-INFO:FILE-MOD-TIME
       JBoxDocument.cCreatedBy      = cASuserId
       JBoxDocument.dCreated        = TODAY
       .
COPY-LOB FROM FILE FILE-INFO:FULL-PATHNAME TO OBJECT JBoxDocument.blDocument.

CREATE JBoxDocRel.
ASSIGN JBoxDocRel.cContext        = icContext
       JBoxDocRel.cEntityId       = icEntityId
       JBoxDocRel.iJBoxDocumentId = JBoxDocument.iJBoxDocumentId
       .


