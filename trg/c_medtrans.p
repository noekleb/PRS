TRIGGER PROCEDURE FOR CREATE OF MedTrans.

assign
  MedTrans.RegistrertDato = today
  MedTrans.RegistrertTid  = time
  MedTrans.RegistrertAV   = userid("skotex").


