/* Update table-to-table link
   Parameters:  <parentid>|<childid-list>

   Created: 14/01/16 by Brynjar
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR ix          AS INT    NO-UNDO.
DEF VAR cPatternId     AS CHAR   NO-UNDO.

cPatternId = ENTRY(1,icParam,"|").

DO TRANSACTION:
  FOR EACH JBoxEventLogPatternType EXCLUSIVE-LOCK
      WHERE JBoxEventLogPatternType.cPatternId = cPatternId:
    IF LOOKUP(STRING(JBoxEventLogPatternType.cEventLogType),icParam,"|") = 0 THEN
      DELETE JBoxEventLogPatternType.
  END.
  DO ix = 2 TO NUM-ENTRIES(icParam,"|"):
    FIND JBoxEventLogPatternType
         WHERE JBoxEventLogPatternType.cPatternId = cPatternId
           AND JBoxEventLogPatternType.cEventLogType  = ENTRY(ix,icParam,"|")
         NO-LOCK NO-ERROR.
    IF NOT AVAIL JBoxEventLogPatternType THEN DO:
      CREATE JBoxEventLogPatternType.
      ASSIGN JBoxEventLogPatternType.cPatternId    = cPatternId
             JBoxEventLogPatternType.cEventLogType = ENTRY(ix,icParam,"|")
             .
    END.
  END.
END.

IF ocReturn = "" THEN obOk = TRUE.

