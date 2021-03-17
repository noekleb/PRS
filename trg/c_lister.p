TRIGGER PROCEDURE FOR CREATE OF SkoTex.Lister.

assign
  SkoTex.Lister.RegistrertDato = today
  SkoTex.Lister.RegistrertTid  = time
  SkoTex.Lister.RegistrertAV   = userid("skotex").


