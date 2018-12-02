TRIGGER PROCEDURE FOR CREATE OF OvBuffer.

assign
  SkoTex.OvBuffer.RegistrertDato = today
  SkoTex.OvBuffer.RegistrertTid  = time
  SkoTex.OvBuffer.RegistrertAV   = userid("skotex").


