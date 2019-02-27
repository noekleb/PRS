/* Create revision of document
      
   Created 07.09.11 by brynjar@chemistry.no
*********************************************************************************/       
DEF INPUT  PARAM icParam         AS CHAR   NO-UNDO.
DEF INPUT  PARAM ihBuffer        AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId     AS CHAR   NO-UNDO.
DEF OUTPUT PARAM ocReturn        AS CHAR   NO-UNDO.
DEF OUTPUT PARAM obOk            AS LOG    NO-UNDO.

DEF VAR iDocId      AS INT    NO-UNDO.
DEF VAR iRevNo      AS INT    NO-UNDO.
DEF VAR hBuffDocRev AS HANDLE NO-UNDO.

ASSIGN iDocId      = INTEGER(ihBuffer:BUFFER-FIELD("iJBoxDocumentId"):BUFFER-VALUE)
       hBuffDocRev = BUFFER JBoxDocRev:HANDLE
       .
   
FOR EACH JBoxDocRev NO-LOCK
    WHERE JBoxDocRev.iJBoxDocumentId = iDocId
      AND JBoxDocRev.iDocRevNo GE 0
    BY JBoxDocRev.iDocRevNo DESC
    :
  
  iRevNo = JBoxDocRev.iDocRevNo + 1.
  LEAVE.
END.

hBuffDocRev:BUFFER-CREATE().
ASSIGN hBuffDocRev:BUFFER-FIELD("iJBoxDocumentId"):BUFFER-VALUE = iDocId
       hBuffDocRev:BUFFER-FIELD("iDocRevNo"):BUFFER-VALUE       = iRevNo
       hBuffDocRev:BUFFER-FIELD("cRevDescription"):BUFFER-VALUE = ihBuffer:BUFFER-FIELD("cDescription"):BUFFER-VALUE
       hBuffDocRev:BUFFER-FIELD("blDocumentRev"):BUFFER-VALUE   = ihBuffer:BUFFER-FIELD("blDocument"):BUFFER-VALUE
       hBuffDocRev:BUFFER-FIELD("iDocSizeRev"):BUFFER-VALUE     = ihBuffer:BUFFER-FIELD("iDocSize"):BUFFER-VALUE
       hBuffDocRev:BUFFER-FIELD("cRevCreatedBy"):BUFFER-VALUE   = ihBuffer:BUFFER-FIELD("cCreatedBy"):BUFFER-VALUE
       hBuffDocRev:BUFFER-FIELD("dRevCreated"):BUFFER-VALUE     = ihBuffer:BUFFER-FIELD("dFileModDate"):BUFFER-VALUE
       hBuffDocRev:BUFFER-FIELD("iRevCreTime"):BUFFER-VALUE     = ihBuffer:BUFFER-FIELD("iFileModTime"):BUFFER-VALUE
       .

obOk = ocReturn = "".
