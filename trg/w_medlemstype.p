TRIGGER PROCEDURE FOR WRITE OF MedlemsType.
assign
  Medlemstype.EDato = today
  Medlemstype.ETid  = time
  Medlemstype.BrukerId   = userid("skotex").


