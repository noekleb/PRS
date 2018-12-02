/* Create Customer
   Parameters:  New buffer
                Fields (comma-separated)
                Values (pipe-separated)
   
   Created: 08.08.2007 Geir Otto                  
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM hBuffer     AS HANDLE NO-UNDO.
DEF INPUT  PARAM icFields    AS CHAR NO-UNDO.
DEF INPUT  PARAM icValues    AS CHAR NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO. 

DEF VAR iId  AS INT NO-UNDO.
DEF VAR iId2 AS INT NO-UNDO.

DEF BUFFER bTable FOR HappyHourPeriod.

ASSIGN 
 iId = hBuffer:BUFFER-FIELD('HapHourId'):BUFFER-VALUE
.

DO ON ERROR UNDO, RETURN:
/*   FOR LAST bTable WHERE bTable.HapHourId = iId NO-LOCK: LEAVE. END. */
  FOR LAST bTable WHERE bTable.HapHourPerId GT 0 NO-LOCK USE-INDEX HapHourPerId: LEAVE. END.
  IF AVAIL bTable THEN
    iId2 = bTable.HapHourPerId + 10.
  ELSE 
    iId2 = 10.
  hBuffer:BUFFER-FIELD("HapHourPerId"):BUFFER-VALUE = iId2.
END.

