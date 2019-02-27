TRIGGER PROCEDURE FOR CREATE OF PakkeLinje.
assign
  PakkeLinje.RegistrertDato = today
  PakkeLinje.RegistrertTid  = time
  PakkeLinje.RegistrertAV   = userid("skotex").


