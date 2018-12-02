TRIGGER PROCEDURE FOR CREATE OF Region.

assign
  Region.RegistrertDato = today
  Region.RegistrertTid  = time
  Region.RegistrertAV   = userid("skotex").


