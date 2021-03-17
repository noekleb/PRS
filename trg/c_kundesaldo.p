TRIGGER PROCEDURE FOR CREATE OF KundeSaldo.


assign
  KundeSaldo.EDato = today
  KundeSaldo.ETid  = time
  KundeSaldo.BrukerId = userid("skotex").


