TRIGGER PROCEDURE FOR CREATE OF SkoTex.Valuta.

assign
  SkoTex.Valuta.ValDatum       = today
  SkoTex.Valuta.RegistrertDato = today
  SkoTex.Valuta.RegistrertTid  = time
  SkoTex.Valuta.RegistrertAV   = userid("skotex").


