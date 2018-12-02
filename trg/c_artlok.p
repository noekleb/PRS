TRIGGER PROCEDURE FOR CREATE OF ArtLok.

assign
  ArtLok.RegistrertDato = today
  ArtLok.RegistrertTid  = time
  ArtLok.RegistrertAV   = userid("skotex").


