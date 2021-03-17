TRIGGER PROCEDURE FOR WRITE OF SkoTex.Lister.

assign
  SkoTex.Lister.EDato    = today
  SkoTex.Lister.ETid     = time
  SkoTex.Lister.BrukerId = userid("skotex").


