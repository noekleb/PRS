TRIGGER PROCEDURE FOR WRITE OF OvBuffer.

DEFINE BUFFER trgOvBuffer FOR OvBuffer.

IF OvBuffer.LinjeNr = 0 THEN 
DO:
  FIND LAST trgOvbuffer NO-LOCK WHERE 
    trgOvbuffer.BuntNr = OvBuffer.BuntNr USE-INDEX BuntLinjeNr NO-ERROR.
  IF AVAILABLE trgOvBuffer THEN 
    OvBuffer.LinjeNr = trgOvBuffer.LinjeNr + 1.
  ELSE 
    OvBuffer.LinjeNr = 1.
  
END.

ASSIGN
  SkoTex.OvBuffer.EDato      = TODAY
  SkoTex.OvBuffer.ETid       = TIME
  SkoTex.OvBuffer.BrukerId   = USERID("skotex").



