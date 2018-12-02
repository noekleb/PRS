/* Create an entity-link to an exsisting document 
   Parameters: entity-name¤<pipe-separated list of entity ids>¤<comma-separated list of document ids>
   
   Created: 05.01.11 by brynjar@chemistry.no
---------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR ix              AS INT    NO-UNDO.
DEF VAR iy              AS INT    NO-UNDO.
DEF VAR cEntity         AS CHAR   NO-UNDO.
DEF VAR cEntityIdList   AS CHAR   NO-UNDO.
DEF VAR cDocIdList      AS CHAR   NO-UNDO.

ASSIGN cEntity       = ENTRY(1,icParam,"¤")
       cEntityIdList = ENTRY(2,icParam,"¤")
       cDocIdList    = ENTRY(3,icParam,"¤")
       NO-ERROR.

IF ERROR-STATUS:ERROR THEN DO:
  ocReturn = "Invalid parameters for " + PROGRAM-NAME(1).
  RETURN.
END.

DO ix = 1 TO NUM-ENTRIES(cEntityIdList,"|"):
  DO iy = 1 TO NUM-ENTRIES(cDocIdList):
    FIND FIRST JBoxDocRel NO-LOCK
         WHERE JBoxDocRel.iJBoxDocumentId = INTEGER(ENTRY(iy,cDocIdList))
           AND JBoxDocRel.cEntityId       = ENTRY(ix,cEntityIdList,"|")
           AND JBoxDocRel.cContext        = cEntity
         NO-ERROR.
    IF NOT AVAIL JBoxDocRel THEN DO:
      CREATE JBoxDocRel.
      ASSIGN JBoxDocRel.iJBoxDocumentId = INTEGER(ENTRY(iy,cDocIdList))       
             JBoxDocRel.cEntityId       = ENTRY(ix,cEntityIdList,"|")
             JBoxDocRel.cContext        = cEntity                             
             .
    END.
  END.
END.
  
obOK = ocReturn = "".


