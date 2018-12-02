TRIGGER PROCEDURE FOR WRITE OF OvBuffer.


assign
  SkoTex.OvBuffer.EDato      = today
  SkoTex.OvBuffer.ETid       = time
  SkoTex.OvBuffer.BrukerId   = userid("skotex").



