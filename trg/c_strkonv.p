TRIGGER PROCEDURE FOR CREATE OF StrKonv.

assign
  StrKonv.RegistrertDato = today
  StrKonv.RegistrertTid  = time
  StrKonv.RegistrertAV   = userid("skotex").


