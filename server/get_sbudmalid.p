/* Parametere:  
   Opprettet: 14.11.2008 - GOO                  
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.
    
DEF VAR trgLoop    AS INT NO-UNDO.
def var trgMalId as int no-undo.

FIND LAST SBudMalHode NO-LOCK NO-ERROR.

IF AVAILABLE SBudMalHode THEN
    trgMalId = SBudMalHode.MalId + 1.
ELSE 
    trgMalId = 1.
IF trgMalId > 99999999 THEN
LOOPEN:
DO trgLoop = 1 TO 99999999:
    IF NOT CAN-FIND(SBudMalHode WHERE
                    SBudMalHode.MalId = trgLoop) THEN
    DO:
      trgMalId = trgLoop.
      LEAVE LOOPEN.
    END.
END. /* LOOPEN */

IF NOT CAN-FIND(SBudMalHode WHERE SBudMalHode.MalId = trgMalId) THEN
    ASSIGN
    obOk     = TRUE
    ocReturn = STRING(trgMalId).
ELSE
    ASSIGN
    obOk = FALSE
    ocReturn = "".
