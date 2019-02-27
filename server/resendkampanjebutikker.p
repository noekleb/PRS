/* 
Update functiongroup to functiongroupset
   Parameters:  <userid>|<grouplist> 
      
            + <ROWID>|<List of rowid's>
         OR + <List of primary keys >
         
         OR temp-table containing CustNum 

def input  param iphBuffer    as handle no-undo.
def input  param ipcFieldList as char   no-undo.
def input  param ipcValues    as char   no-undo.
def input  param ipcSessionid as char   no-undo.
def output param opcReturn    as char   no-undo.
   
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR ix          AS INT    NO-UNDO.
DEF VAR cId         AS CHAR   NO-UNDO.
def var bh          as handle no-undo.

assign 
  icParam = trim(icParam,"|")
  cId     = ENTRY(1,icParam,"|")
.
DO TRANSACTION:
  DO ix = 2 TO NUM-ENTRIES(icParam,"|"):
    FIND KampanjeButikker 
         WHERE KampanjeButikker.KampId = DEC(cId)
           AND KampanjeButikker.Butik  = INT(entry(ix,icParam,"|"))
         NO-ERROR.
    IF  AVAIL KampanjeButikker THEN 
    DO:
      ASSIGN KampanjeButikker.MottattDato = ?
             KampanjeButikker.MottattKl = ""
             KampanjeButikker.Resultat  = ""
             KampanjeButikker.SendtDato = ?
             KampanjeButikker.SendtTid  = 0
             .
    END.
  END.
END.

IF ocReturn = "" THEN obOk = TRUE.

