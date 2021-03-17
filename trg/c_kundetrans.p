TRIGGER PROCEDURE FOR CREATE OF KundeTrans.


assign
  KundeTrans.RegistrertDato = today
  KundeTrans.RegistrertTid  = time
  KundeTrans.RegistrertAV   = userid("skotex").


