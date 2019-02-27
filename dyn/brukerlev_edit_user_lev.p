/* Update user-lev link
   Parameters:  <userid>|<levlist> 
      
            + <ROWID>|<List of rowid's>
         OR + <List of primary keys >
         
         OR temp-table containing CustNum 
   
   Created: 18.06.08 by Brynjar Hasle                  
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR ix          AS INT    NO-UNDO.
DEF VAR cUserId     AS CHAR   NO-UNDO.

cUserId = ENTRY(1,icParam,"|").

DO TRANSACTION:
  FOR EACH BrukerLev EXCLUSIVE-LOCK
      WHERE BrukerLev.Bruker = cUserId:
    IF LOOKUP(STRING(BrukerLev.LevNr),icParam,"|") = 0 THEN
      DELETE BrukerLev.
  END.
  DO ix = 2 TO NUM-ENTRIES(icParam,"|"):
    FIND BrukerLev 
         WHERE BrukerLev.Bruker = cUserId
           AND BrukerLev.LevNr  = INT(ENTRY(ix,icParam,"|"))
         NO-LOCK NO-ERROR.
    IF NOT AVAIL BrukerLev THEN DO:
      CREATE BrukerLev.
      ASSIGN BrukerLev.Bruker    = cUserId
             BrukerLev.LevNr = INT(ENTRY(ix,icParam,"|"))
             .
    END.
  END.
END.

IF ocReturn = "" THEN obOk = TRUE.

