TRIGGER PROCEDURE FOR CREATE OF KundeBetTrans.

assign
  KundeBetTrans.RegistrertDato = today
  KundeBetTrans.RegistrertTid  = time
  KundeBetTrans.RegistrertAV   = userid("skotex").


