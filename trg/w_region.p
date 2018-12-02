TRIGGER PROCEDURE FOR WRITE OF Region.

assign
  Region.EDato    = today
  Region.ETid     = time
  Region.BrukerId = userid("skotex").


