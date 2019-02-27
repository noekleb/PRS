TRIGGER PROCEDURE FOR WRITE OF ArtLok.

assign
  ArtLok.EDato    = today
  ArtLok.ETid     = time
  ArtLok.Brukerid = userid("skotex").


