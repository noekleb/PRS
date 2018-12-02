TRIGGER PROCEDURE FOR CREATE OF SkoTex.Prisprofil.

assign
  SkoTex.Prisprofil.RegistrertDato = today
  SkoTex.Prisprofil.RegistrertTid  = time
  SkoTex.Prisprofil.RegistrertAV   = userid("skotex").


