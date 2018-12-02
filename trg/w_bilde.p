TRIGGER PROCEDURE FOR WRITE OF SkoTex.Bilderegister.

assign
  SkoTex.BildeRegister.EDato = today
  SkoTex.BildeRegister.ETid  = time
  SkoTex.BildeRegister.Brukerid = userid("skotex").
