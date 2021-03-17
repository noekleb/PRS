TRIGGER PROCEDURE FOR CREATE OF SkoTex.Bilderegister.

assign
  SkoTex.BildeRegister.RegistrertDato = today
  SkoTex.BildeRegister.RegistrertTid  = time
  SkoTex.BildeRegister.RegistrertAV   = userid("skotex").

