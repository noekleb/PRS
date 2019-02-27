/* Parametere:  
   Opprettet: 14.11.2008 - GOO                  
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.
    
DEF VAR trgLoop    AS INT NO-UNDO.
def var trgTelleNr as int no-undo.

FIND LAST TelleHode NO-LOCK NO-ERROR.

IF AVAILABLE TelleHode THEN
    trgTelleNr = TelleHode.TelleNr + 1.
ELSE 
    trgTelleNr = 1.
IF trgTelleNr > 99999999 THEN
LOOPEN:
DO trgLoop = 1 TO 99999999:
    IF NOT CAN-FIND(TelleHode WHERE
                    TelleHode.TelleNr = trgLoop) THEN
    DO:
      trgTelleNr = trgLoop.
      LEAVE LOOPEN.
    END.
END. /* LOOPEN */

IF NOT CAN-FIND(TelleHode WHERE TelleHode.TelleNr = trgTelleNr) THEN
    ASSIGN
    obOk     = TRUE
    ocReturn = STRING(trgTelleNr).
ELSE
    ASSIGN
    obOk = FALSE
    ocReturn = "".
