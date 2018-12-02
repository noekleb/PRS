/* Get info on documents (to decide on chunking before download)
   Created 06.04.08 by Brynjar Hasle.
   
----------------------------------------------------------------------------------------------------*/
            
DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO. 
DEF INPUT  PARAM icParam      AS CHAR NO-UNDO.
DEF OUTPUT PARAM TABLE-HANDLE hTempTable.
DEF OUTPUT PARAM ocReturn     AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK         AS LOG NO-UNDO INIT TRUE.

DEF VAR hBufferDocRel   AS HANDLE NO-UNDO.
DEF VAR hBufferDocument AS HANDLE NO-UNDO.
DEF VAR httBuffer       AS HANDLE NO-UNDO.
DEF VAR hQuery          AS HANDLE NO-UNDO.
DEF VAR hBuffDocRev     AS HANDLE NO-UNDO.

{incl/validatesession.i}

/* This is an example of how you can suffer by not being consistent with
   delimiters.. */
IF icParam BEGINS "JBoxPackage," THEN
  icParam = ENTRY(1,icParam) + "|" + ENTRY(2,icParam).
/* ------------ end suffering.. */

CREATE TEMP-TABLE hTempTable.
hTempTable:CREATE-LIKE("JBoxDocument").
hTempTable:TEMP-TABLE-PREPARE("ttDocInfo").

hBufferDocRel   = BUFFER JBoxDocRel:HANDLE.
hBufferDocument = BUFFER JBoxDocument:HANDLE.

httBuffer = hTempTable:DEFAULT-BUFFER-HANDLE.


IF ENTRY(1,icParam,"|") NE "JBoxDocument" AND ENTRY(1,icParam,"|") NE "JBoxDocRev" THEN DO:
  CREATE QUERY hQuery.
  hQuery:SET-BUFFERS(hBufferDocRel,hBufferDocument).

  hQuery:QUERY-PREPARE("FOR EACH JBoxDocRel NO-LOCK WHERE cContext = '" 
                          + ENTRY(1,icParam,"|") + "' AND cEntityId = '" + ENTRY(2,icParam,"|") 
                          + "', FIRST JBoxDocument OF JBoxDocRel NO-LOCK"
                          + (IF NUM-ENTRIES(icParam,"|") = 3 THEN 
                              IF ENTRY(3,icParam,"|") BEGINS "!" THEN
                                " WHERE NOT cFileName BEGINS '" + SUBSTR(ENTRY(3,icParam,"|"),2) + "'"
                              ELSE
                                " WHERE cFileName BEGINS '" + ENTRY(3,icParam,"|") + "'"
                             ELSE "")
                          + " BY JBoxDocRel.iJBoxDocumentId").
  hQuery:QUERY-OPEN().
  hQuery:GET-FIRST().
  
  /* If no doc was found for the filename look for the small_ version: */
  IF hQuery:QUERY-OFF-END AND NUM-ENTRIES(icParam,"|") = 3 THEN DO:
    hQuery:QUERY-CLOSE().
    hQuery:QUERY-PREPARE("FOR EACH JBoxDocRel NO-LOCK WHERE cContext = '" 
                            + ENTRY(1,icParam,"|") + "' AND cEntityId = '" + ENTRY(2,icParam,"|") 
                            + "', FIRST JBoxDocument OF JBoxDocRel NO-LOCK"
/*                            + " WHERE cFileName BEGINS 'small_'"*/
                            + " BY JBoxDocRel.iJBoxDocumentId").
    hQuery:QUERY-OPEN().
    hQuery:GET-FIRST().
  END.

  REPEAT WHILE NOT hQuery:QUERY-OFF-END:
    httBuffer:BUFFER-CREATE().
    httBuffer:BUFFER-COPY(hBufferDocument,"blDocument").
     
    hQuery:GET-NEXT().
  END.
END.
ELSE IF ENTRY(1,icParam,"|") = "JBoxDocument" THEN DO:
  CREATE QUERY hQuery.
  hQuery:SET-BUFFERS(hBufferDocument).

  hQuery:QUERY-PREPARE("FOR EACH JBoxDocument NO-LOCK WHERE iJBoxDocumentId = " 
                          + ENTRY(2,icParam,"|")).
  hQuery:QUERY-OPEN().
  hQuery:GET-FIRST().

  IF hBufferDocument:AVAIL THEN DO:
    httBuffer:BUFFER-CREATE().
    httBuffer:BUFFER-COPY(hBufferDocument,"blDocument").
  END.
END.
ELSE IF ENTRY(1,icParam,"|") = "JBoxDocRev" THEN DO:
  CREATE BUFFER hBuffDocRev FOR TABLE "JBoxDocRev" NO-ERROR.
  IF NOT ERROR-STATUS:ERROR THEN DO:
    hBuffDocRev:FIND-FIRST("WHERE iJBoxDocumentId = " + ENTRY(2,icParam,"|")
                         + " AND iDocRevNo = " + ENTRY(3,icParam,"|"),NO-LOCK) NO-ERROR.
    IF hBuffDocRev:AVAIL THEN DO:
      hBufferDocument:FIND-FIRST("WHERE iJBoxDocumentId = " + ENTRY(2,icParam,"|"),NO-LOCK) NO-ERROR.
      httBuffer:BUFFER-CREATE().
      httBuffer:BUFFER-COPY(hBuffDocRev,"",
                            "cRevCreatedBy,cCreatedBy"
                          + ",cRevDescription,cDescription"
                          + ",dRevCreated,dCreated"
                          + ",iDocSizeRev,iDocSize"
                          + ",iFileModTime,iRevCreTime").
      ASSIGN httBuffer:BUFFER-FIELD("cFileName"):BUFFER-VALUE = "JBoxDocRev-" + TRIM(ENTRY(2,icParam,"|")) + "-" + TRIM(ENTRY(3,icParam,"|"))
                                                              + "." + hBufferDocument:BUFFER-FIELD("cFileType"):BUFFER-VALUE
                                                              + (IF hBufferDocument:BUFFER-FIELD("cFileName"):BUFFER-VALUE MATCHES "*.gz" THEN ".gz" ELSE "")
             httBuffer:BUFFER-FIELD("cFileType"):BUFFER-VALUE = hBufferDocument:BUFFER-FIELD("cFileType"):BUFFER-VALUE
             .
    END.      
  END.
  DELETE OBJECT hBuffDocRev NO-ERROR.
END.

DELETE OBJECT hTempTable.
DELETE OBJECT hQuery NO-ERROR.

