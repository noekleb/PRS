/* get documents for an entity 
   The temp-table is defined on the client
   Created: 14.12.10 by brynjar@chemistry.no
----------------------------------------------------------------------------- */
DEF INPUT  PARAM icParam         AS CHAR   NO-UNDO.
DEF INPUT  PARAM ihBuffer        AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId     AS CHAR   NO-UNDO.
DEF OUTPUT PARAM ocReturn        AS CHAR   NO-UNDO.
DEF OUTPUT PARAM obOk            AS LOG    NO-UNDO.

DEF VAR cEntity       AS CHAR   NO-UNDO.
DEF VAR cEntityId     AS CHAR   NO-UNDO.
DEF VAR cQuery        AS CHAR   NO-UNDO.
DEF VAR hQuery        AS HANDLE NO-UNDO.
DEF VAR cDocRevList   AS CHAR   NO-UNDO.
DEF VAR bOk           AS LOG    NO-UNDO.
DEF VAR hDocRevBuffer AS HANDLE NO-UNDO.
DEF VAR cOtherRel     AS CHAR   NO-UNDO.
DEF VAR cOtherRelKeys AS CHAR   NO-UNDO.

DEFINE BUFFER bDocRel FOR JBoxDocRel.

CREATE BUFFER hDocRevBuffer FOR TABLE "JBoxDocRev" NO-ERROR.

IF NUM-ENTRIES(icParam,"|") > 1 THEN
  cQuery = "cContext = '" + ENTRY(1,icParam,"|") + "' AND cEntityID = '" + ENTRY(2,icParam,"|") + "'".
ELSE
  cQuery = icParam.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(BUFFER JBoxDocRel:HANDLE).
hQuery:QUERY-PREPARE("FOR EACH JBoxDocRel NO-LOCK WHERE " + cQuery) NO-ERROR.
IF ERROR-STATUS:GET-MESSAGE(1) = "" THEN DO:
  hQuery:QUERY-OPEN() NO-ERROR.
  hQuery:GET-FIRST().
  REPEAT WHILE NOT hQuery:QUERY-OFF-END:
    FOR FIRST JBoxDocument FIELDS(cCreatedBy cDescription cFileName cFileType cFullPathName cModifiedBy dCreated dFileCreateDate dFileModDate dModified iDocSize iFileCreateTime iFileModTime iJBoxDocumentId)
        NO-LOCK
        WHERE JBoxDocument.iJBoxDocumentId = INTEGER(hQuery:GET-BUFFER-HANDLE(1):BUFFER-FIELD("iJBoxDocumentId"):BUFFER-VALUE)
        :
      FIND FIRST JBoxDocLink NO-LOCK
           WHERE JBoxDocLink.iFromDocumentId = JBoxDocument.iJBoxDocumentId
           NO-ERROR.
      ihBuffer:BUFFER-CREATE().
      ihBuffer:BUFFER-COPY(hQuery:GET-BUFFER-HANDLE(1)).
      ihBuffer:BUFFER-COPY(BUFFER JBoxDocument:HANDLE).
      ASSIGN ihBuffer:BUFFER-FIELD("cFileCreateTime"):BUFFER-VALUE = STRING(ihBuffer:BUFFER-FIELD("iFileCreateTime"):BUFFER-VALUE,"hh:mm:ss")
             ihBuffer:BUFFER-FIELD("cFileModTime"):BUFFER-VALUE = STRING(ihBuffer:BUFFER-FIELD("iFileModTime"):BUFFER-VALUE,"hh:mm:ss")
             ihBuffer:BUFFER-FIELD("DocRowident"):BUFFER-VALUE = STRING(ROWID(JBoxDocument))
             .

      IF AVAIL JBoxDocLink THEN
        ihBuffer:BUFFER-COPY(BUFFER JBoxDocLink:HANDLE,"dCreated,cCreatedBy").
      ELSE 
        ASSIGN ihBuffer:BUFFER-FIELD("iToDocumentId"):BUFFER-VALUE = JBoxDocument.iJBoxDocumentId
               ihBuffer:BUFFER-FIELD("iFromDocumentId"):BUFFER-VALUE = JBoxDocument.iJBoxDocumentId
               .

      FOR EACH bDocRel NO-LOCK
          WHERE bDocRel.iJBoxDocumentId = INTEGER(hQuery:GET-BUFFER-HANDLE(1):BUFFER-FIELD("iJBoxDocumentId"):BUFFER-VALUE)
            AND bDocRel.cContext NE hQuery:GET-BUFFER-HANDLE(1):BUFFER-FIELD("cContext"):BUFFER-VALUE
            AND bDocRel.cContext NE "JBoxEmailInbox"
          :
        IF NOT CAN-DO(ocReturn,bDocRel.cContext) THEN 
          ASSIGN cOtherRel     = cOtherRel + (IF cOtherRel NE "" THEN "," ELSE "") + bDocRel.cContext
                 cOtherRelKeys = cOtherRelKeys + (IF cOtherRelKeys NE "" THEN "¤" ELSE "") + bDocRel.cEntityId
                 .
      END.
      ASSIGN ihBuffer:BUFFER-FIELD("cOtherLinks"):BUFFER-VALUE    = cOtherRel
             ihBuffer:BUFFER-FIELD("cOtherLinkKeys"):BUFFER-VALUE = cOtherRelKeys
             cOtherRel     = ""
             cOtherRelKeys = ""
             .

      IF VALID-HANDLE(hDocRevBuffer) THEN DO:
        RUN jbdoc_getdocrevisions.p (STRING(JBoxDocument.iJBoxDocumentId),?,icSessionId,OUTPUT cDocRevList,OUTPUT bOk).
        IF bOk THEN
          ASSIGN ihBuffer:BUFFER-FIELD("cDocRevList"):BUFFER-VALUE = ENTRY(1,cDocRevList,"|")
                 ihBuffer:BUFFER-FIELD("cLockInfo"):BUFFER-VALUE   = ENTRY(2,cDocRevList,"|")
                 .
      END.

    END.
    hQuery:GET-NEXT().
  END.
  obOk = YES.
END.
ELSE ocReturn = ERROR-STATUS:GET-MESSAGE(1).

DELETE OBJECT hQuery NO-ERROR.
IF VALID-HANDLE(hDocRevBuffer) THEN
  DELETE OBJECT hDocRevBuffer.

