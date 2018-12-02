TRIGGER PROCEDURE FOR CREATE OF vpiBilderegister.

assign
  vpiBildeRegister.RegistrertDato = today
  vpiBildeRegister.RegistrertTid  = time
  vpiBildeRegister.RegistrertAV   = userid("skotex").

