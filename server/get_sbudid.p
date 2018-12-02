/* Parametere:  
   Opprettet: 14.11.2008 - GOO                  
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.
    
DEF VAR trgSBudId AS INT NO-UNDO.
DEFINE VARIABLE trgLoop AS INTEGER NO-UNDO.

FIND LAST SBudHode NO-LOCK NO-ERROR.

IF AVAILABLE SBudHode THEN
    trgSBudId = SBudHode.SBudId + 1.
ELSE 
    trgSBudId = 1.
IF trgSBudId > 99999999 THEN
LOOPEN:
DO trgLoop = 1 TO 99999999:
    IF NOT CAN-FIND(SBudHode WHERE
                    SBudHode.SBudId = trgLoop) THEN
    DO:
      trgSBudId = trgLoop.
      LEAVE LOOPEN.
    END.
END. /* LOOPEN */

IF NOT CAN-FIND(SBudHode WHERE SBudHode.SBudId = trgSBudId) THEN
    ASSIGN
    obOk     = TRUE
    ocReturn = STRING(trgSBudId).
ELSE
    ASSIGN
    obOk = FALSE
    ocReturn = "".
