TRIGGER PROCEDURE FOR CREATE OF SkoTex.Butiker.

assign
  SkoTex.Butiker.RegistrertDato = today
  SkoTex.Butiker.RegistrertTid  = time
  SkoTex.Butiker.RegistrertAV   = userid("skotex").


