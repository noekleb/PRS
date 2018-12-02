TRIGGER PROCEDURE FOR CREATE OF SkoTex.ekstbutiker.

assign
  SkoTex.ekstbutiker.RegistrertDato = today
  SkoTex.ekstbutiker.RegistrertTid  = time
  SkoTex.ekstbutiker.RegistrertAV   = userid("skotex").


