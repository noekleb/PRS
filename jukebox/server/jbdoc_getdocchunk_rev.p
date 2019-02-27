/* Get a chunk of a document - to get by timeout issues and possible bug in the Java classes used by Aia
   Created 06.04.08 by brynjar@chemistry.no
   
----------------------------------------------------------------------------------------------------*/            
DEF INPUT  PARAM       icSessionId   AS CHAR NO-UNDO. 
DEF INPUT  PARAM       iiDocumentId  AS INT  NO-UNDO.
DEF INPUT  PARAM       iiRevNo       AS INT  NO-UNDO.
DEF INPUT  PARAM       iiChunkSize   AS INT  NO-UNDO.
DEF INPUT  PARAM       iiStartChunk  AS INT  NO-UNDO.
DEF OUTPUT PARAM TABLE-HANDLE hTempTable.
DEF OUTPUT PARAM       oiDocSize     AS INT  NO-UNDO.
DEF OUTPUT PARAM       ocFileName    AS CHAR NO-UNDO.
DEF OUTPUT PARAM       ocReturn      AS CHAR NO-UNDO.
DEF OUTPUT PARAM       obOK          AS LOG  NO-UNDO.

DEF VAR iPiece        AS INTEGER     NO-UNDO.

{incl/validatesession.i}

DEF TEMP-TABLE ttChunk NO-UNDO
    FIELD blChunk AS BLOB.
hTempTable = BUFFER ttChunk:HANDLE:TABLE-HANDLE.

FIND FIRST JBoxDocRev NO-LOCK
     WHERE JBoxDocRev.iJBoxDocumentId = iiDocumentId
       AND JBoxDocRev.iDocRevNo       = iiRevNo
     NO-ERROR.
IF NOT AVAIL JBoxDocRev THEN DO:
  ocReturn = "Error when finding document revision " + STRING(iiDocumentId) + " - " + STRING(iiRevNo).
  RETURN.
END.
FIND JBoxDocument NO-LOCK
     WHERE JBoxDocument.iJBoxDocumentId = iiDocumentId
     NO-ERROR.
IF NOT AVAIL JBoxDocument THEN DO:
  ocReturn = "Error when finding document " + STRING(iiDocumentId).
  RETURN.
END.

ASSIGN oiDocSize  = LENGTH(JBoxDocRev.blDocument)
       ocFileName = "JBoxDocRev-" + STRING(iiDocumentId) + "-" + STRING(iiRevNo)
                                                              + "." + JBoxDocument.cFileType
                                                              + (IF JBoxDocument.cFileName MATCHES "*.gz" THEN ".gz" ELSE "").

/* Just asked for the size of the document: */
IF iiStartChunk = 0 THEN DO:
  obOK = YES.
  RETURN.
END.

CREATE ttChunk.
COPY-LOB FROM OBJECT JBoxDocRev.blDocumentRev
         STARTING AT iiStartChunk FOR iiChunkSize
         TO ttChunk.blChunk NO-ERROR.
IF ERROR-STATUS:ERROR THEN 
  ocReturn = ERROR-STATUS:GET-MESSAGE(1).

obOK = ocReturn = "".

DELETE OBJECT hTempTable.
