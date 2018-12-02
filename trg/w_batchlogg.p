TRIGGER PROCEDURE FOR WRITE OF SkoTex.BatchLogg.

assign
  SkoTex.BatchLogg.EDato    = today
  SkoTex.BatchLogg.ETid     = time
  SkoTex.BatchLogg.BrukerId = userid("skotex").


