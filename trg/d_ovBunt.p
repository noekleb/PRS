TRIGGER PROCEDURE FOR DELETE OF OvBunt.

DEF BUFFER trgOvBuffer FOR Ovbuffer.

FOR EACH trgOvBuffer EXCLUSIVE-LOCK WHERE 
  trgOvBuffer.BuntNr = OvBunt.BuntNr:
    DELETE trgOvbuffer.
END.
