/* Produce list of document revisions
   Revision num (iDocRevNo) < 0 indicates that the document is locked for update
   
   Created 07.09.11 by brynjar@chemistry.no
*********************************************************************************/   
    
DEF INPUT  PARAM icParam         AS CHAR   NO-UNDO.
DEF INPUT  PARAM ihBuffer        AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId     AS CHAR   NO-UNDO.
DEF OUTPUT PARAM ocReturn        AS CHAR   NO-UNDO.
DEF OUTPUT PARAM obOk            AS LOG    NO-UNDO INIT YES.

DEF VAR iDocId        AS INT    NO-UNDO.
DEF VAR cLockString   AS CHAR   NO-UNDO.

iDocId = INTEGER(icParam) NO-ERROR.
IF ERROR-STATUS:ERROR THEN DO:
  obOk = NO.
  RETURN.
END.
   
FOR EACH JBoxDocRev NO-LOCK
    WHERE JBoxDocRev.iJBoxDocumentId = iDocId
      AND JBoxDocRev.iDocRevNo GE 0
    BY JBoxDocRev.iDocRevNo DESC
    :
  
  ocReturn = ocReturn 
           + (IF ocReturn NE "" THEN "," ELSE "")
           + STRING(JBoxDocRev.iDocRevNo) 
           + " - " + STRING(JBoxDocRev.dRevCreated) + " " + STRING(JBoxDocRev.iRevCreTime,"HH:MM:SS")
           + " - " + JBoxDocRev.cRevCreatedBy
           + " - " + REPLACE(JBoxDocRev.cRevDescription,","," ")
           .
END.
IF ocReturn = ? THEN ocReturn = "".

FIND FIRST JBoxDocRev NO-LOCK
     WHERE JBoxDocRev.iJBoxDocumentId = iDocId
       AND JBoxDocRev.iDocRevNo < 0
     NO-ERROR.
IF AVAIL JBoxDocRev THEN
  cLockString = JBoxDocRev.cRevCreatedBy + "|" + STRING(JBoxDocRev.dRevCreated).

ocReturn = ocReturn + "|" + cLockString.
