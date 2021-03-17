TRIGGER PROCEDURE FOR CREATE OF MesseForButikk.

assign
  MesseForButikk.RegistrertDato = today
  MesseForButikk.RegistrertTid  = time
  MesseForButikk.RegistrertAV   = userid("skotex").


