/* Delete all files in a package
   Parameters:  <iJBoxPackageId> 
         
   Created: 16.06.05 by Brynjar Hasle                  
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR ix              AS INT    NO-UNDO.
DEF VAR iJBoxPackageId  AS INT   NO-UNDO.

iJBoxPackageId = INT(ENTRY(1,icParam,"|")).

DO TRANSACTION:
  FOR EACH JBoxDocRel EXCLUSIVE-LOCK
      WHERE JBoxDocRel.cContex   = "JBoxPackage"
        AND JBoxDocRel.cEntityId = icParam:
    FIND FIRST JBoxDocument OF JBoxDocRel EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL JBoxDocument THEN DO:
      FOR EACH JBoxDocRev EXCLUSIVE-LOCK
          OF JBoxDocument:
        DELETE JBoxDocRev.
      END.
      FOR EACH JBoxDocLink EXCLUSIVE-LOCK
          WHERE JBoxDocLink.iToDocumentId = JBoxDocument.iJBoxDocumentId:
        DELETE JBoxDocLink.
      END.
      DELETE JBoxDocument.  
    END. 
      
    DELETE JBoxDocRel.
  END.
END.

IF ocReturn = "" THEN obOk = TRUE.

