/* Get list of connected databases
   Parameters: None
   
   Created: 17.06.11 by Brynjar Hasle                  
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO INIT YES.

DEF VAR ix           AS INT    NO-UNDO.

DO ix = 1 TO 20:
  IF LDBNAME(ix) = ? THEN LEAVE.
  ELSE ocReturn = ocReturn + (IF ocReturn NE "" THEN "," ELSE "") + LDBNAME(ix).
END.


