TRIGGER PROCEDURE FOR CREATE OF MedlemsType.
assign
  MedlemsType.RegistrertDato = today
  MedlemsType.RegistrertTid  = time
  Medlemstype.RegistrertAV   = userid("skotex").


