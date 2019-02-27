/* Get documents for a context (table) and entityid (primary key for the table) 
   Created fall 2004 by Brynjar Hasle.
   
   Modified 25.01.06 by Brynjar Hasle:
          - Added convetion to get a specific document:
            If context = "JBoxDocument" the entityid should be the documentid (iJBoxDocumentId)
            to get a specific document 
          06.04.08 by brynjar@chemistry.no
          - Added possibility to retrieve batches of documents by interpreting the 4th element of 
            the icParam string
          18.06.14 by Brynjar
          - Added function for loading from disc  
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
DEF VAR hOrgDocQuery    AS HANDLE NO-UNDO.
DEF VAR hOrgDocRelBuff  AS HANDLE NO-UNDO.
DEF VAR hOrgDocBuffer   AS HANDLE NO-UNDO.
DEF VAR hBuffDocRev     AS HANDLE NO-UNDO.
DEF VAR ix              AS INT    NO-UNDO.

{incl/validatesession.i}

DEF TEMP-TABLE ttFileBlob NO-UNDO
    FIELD blDocument AS BLOB
    FIELD iDocSize   AS INT
    .

/* This is an example of how you can suffer by not being consistent with
   delimiters.. */
IF icParam BEGINS "JBoxPackage," THEN
  icParam = ENTRY(1,icParam) + "|" + ENTRY(2,icParam).
/* ------------ end suffering.. */

CREATE TEMP-TABLE hTempTable.
hTempTable:CREATE-LIKE("JBoxDocument").
hTempTable:TEMP-TABLE-PREPARE("ttDoc").

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
                              /* Get the large or small versjons of the document (picture): */
                              (IF ENTRY(3,icParam,"|") BEGINS "!" THEN
                                 " WHERE NOT cFileName BEGINS '" + SUBSTR(ENTRY(3,icParam,"|"),2) + "'"
                               ELSE
                                 " WHERE cFileName BEGINS '" + ENTRY(3,icParam,"|") + "'")

                             ELSE IF NUM-ENTRIES(icParam,"|") = 4 THEN 

                              ((IF ENTRY(3,icParam,"|") BEGINS "!" THEN
                                  " WHERE NOT cFileName BEGINS '" + SUBSTR(ENTRY(3,icParam,"|"),2) + "'"
                                ELSE IF ENTRY(3,icParam,"|") NE "" THEN
                                  " WHERE cFileName BEGINS '" + ENTRY(3,icParam,"|") + "'"
                                ELSE " WHERE true")
                               /* Get a batch of documents that are all under the size limit for chunking: */
                             + (IF ENTRY(4,icParam,"|") NE "" THEN
                                  " AND JBoxDocument.iJBoxDocumentId GT " + ENTRY(1,ENTRY(4,icParam,"|"),"¤")
                                + " AND JBoxDocument.iJBoxDocumentId LE " + ENTRY(2,ENTRY(4,icParam,"|"),"¤")
                                + " AND JBoxDocument.iDocSize LE " + ENTRY(3,ENTRY(4,icParam,"|"),"¤")
                                ELSE ""))

                             ELSE "")
                          + " BY JBoxDocRel.iJBoxDocumentId").

  hQuery:QUERY-OPEN().
  hQuery:GET-FIRST().
  
  /* If no doc was found for the filename look for the small_ version: */
  IF hQuery:QUERY-OFF-END AND NUM-ENTRIES(icParam,"|") GE 3 THEN DO:
    hQuery:QUERY-CLOSE().
    hQuery:QUERY-PREPARE("FOR EACH JBoxDocRel NO-LOCK WHERE cContext = '" 
                            + ENTRY(1,icParam,"|") + "' AND cEntityId = '" + ENTRY(2,icParam,"|") 
                            + "', FIRST JBoxDocument OF JBoxDocRel NO-LOCK"
/*                            + " WHERE cFileName BEGINS 'small_" + ENTRY(3,icParam,"|") + "'"*/
                            + " BY JBoxDocRel.iJBoxDocumentId").
    hQuery:QUERY-OPEN().
    hQuery:GET-FIRST().
  END.

  /* When asking for the small_ version of pictures also return the doc info (but not the blob) for the corresponding large ones: */
  IF NUM-ENTRIES(icParam,"|") GE 3 AND ENTRY(3,icParam,"|") = "small_" THEN DO:
    CREATE BUFFER hOrgDocRelBuff FOR TABLE "JBoxDocRel".
    CREATE BUFFER hOrgDocBuffer  FOR TABLE "JBoxDocument".
    CREATE QUERY hOrgDocQuery.
    hOrgDocQuery:SET-BUFFERS(hOrgDocRelBuff,hOrgDocBuffer).
  END.

  REPEAT WHILE NOT hQuery:QUERY-OFF-END:
    httBuffer:BUFFER-CREATE().
    httBuffer:BUFFER-COPY(hBufferDocument).
   
    IF VALID-HANDLE(hOrgDocQuery) THEN DO:  /* Grab (just) doc info for the large doc (picture) */
      hOrgDocQuery:QUERY-PREPARE("FOR EACH JBoxDocRel NO-LOCK WHERE cContext = '" 
                              + ENTRY(1,icParam,"|") + "' AND cEntityId = '" + ENTRY(2,icParam,"|") 
                              + "', FIRST JBoxDocument OF JBoxDocRel NO-LOCK"
                              + " WHERE cFileName = '" 
                              + SUBSTR(hBufferDocument:BUFFER-FIELD("cFileName"):BUFFER-VALUE,7) + "'"
                                ).
      hOrgDocQuery:QUERY-OPEN().
      hOrgDocQuery:GET-FIRST().
      IF hOrgDocBuffer:AVAIL THEN
        httBuffer:BUFFER-COPY(hOrgDocBuffer,"blDocument,iJBoxDocumentId").
    END.
    
    IF LENGTH(httBuffer::blDocument) = ? AND (SEARCH(httBuffer::cFileName) NE ? OR SEARCH(REPLACE(httBuffer::cFileName,"\","/")) NE ?) THEN DO:
      CREATE ttFileBlob.
      IF SEARCH(httBuffer::cFileName) NE ? THEN
        FILE-INFO:FILE-NAME = SEARCH(httBuffer::cFileName).
      ELSE
        FILE-INFO:FILE-NAME = SEARCH(REPLACE(httBuffer::cFileName,"\","/")).
      COPY-LOB FROM FILE FILE-INFO:FULL-PATHNAME TO OBJECT ttFileBlob.blDocument.
      ttFileBlob.iDocSize = FILE-INFO:FILE-SIZE.
      httBuffer:BUFFER-COPY(BUFFER ttFileBlob:HANDLE).
    END.
    ELSE IF LENGTH(httBuffer::blDocument) = ? THEN 
      MESSAGE "File not found: " httBuffer::cFileName " (searches also " REPLACE(httBuffer::cFileName,"\","/") " )" SKIP. 
  
    hQuery:GET-NEXT().
  END.
END.
ELSE IF ENTRY(1,icParam,"|") = "JBoxDocument" THEN DO:
/*   CREATE QUERY hQuery.                 */
/*   hQuery:SET-BUFFERS(hBufferDocument). */
/*   hQuery:QUERY-PREPARE("FOR FIRST JBoxDocRel NO-LOCK WHERE iJBoxDocumentId = " */
/*                           + ENTRY(2,icParam,"|")                               */
/*                           + ",FIRST JBoxDocument OF JBoxDocRel NO-LOCK"        */
/*                             ).                                                 */
/*   hQuery:QUERY-OPEN().                                                         */
/*   hQuery:GET-FIRST().                                                          */

  DO ix = 2 TO NUM-ENTRIES(icParam,"|"):
    hBufferDocument:FIND-FIRST("WHERE iJBoxDocumentId = " + ENTRY(ix,icParam,"|"),NO-LOCK) NO-ERROR.
    IF hBufferDocument:AVAIL THEN DO:
      httBuffer:BUFFER-CREATE().
      httBuffer:BUFFER-COPY(hBufferDocument).
      IF LENGTH(httBuffer::blDocument) = ? AND (SEARCH(httBuffer::cFileName) NE ? OR SEARCH(REPLACE(httBuffer::cFileName,"\","/")) NE ?) THEN DO:
        CREATE ttFileBlob.
        IF SEARCH(httBuffer::cFileName) NE ? THEN
          FILE-INFO:FILE-NAME = SEARCH(httBuffer::cFileName).
        ELSE
          FILE-INFO:FILE-NAME = SEARCH(REPLACE(httBuffer::cFileName,"\","/")).
        COPY-LOB FROM FILE FILE-INFO:FULL-PATHNAME TO OBJECT ttFileBlob.blDocument.
        ttFileBlob.iDocSize = FILE-INFO:FILE-SIZE.
        httBuffer:BUFFER-COPY(BUFFER ttFileBlob:HANDLE).
      END.
    END.
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
                            "blDocumentRev,blDocument"
                          + ",cRevCreatedBy,cCreatedBy"
                          + ",cRevDescription,cDescription"
                          + ",dRevCreated,dCreated"
                          + ",iDocSizeRev,iDocSize"
                          + ",iFileModTime,iRevCreTime")
                            .
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
DELETE OBJECT hOrgDocQuery NO-ERROR.
DELETE OBJECT hOrgDocBuffer NO-ERROR.
DELETE OBJECT hOrgDocRelBuff NO-ERROR.

