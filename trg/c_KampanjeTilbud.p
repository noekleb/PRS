TRIGGER PROCEDURE FOR CREATE OF KampanjeTilbud.

def var trgInt as INT no-undo.

{trg\c_w_trg.i &Fil=SkoTex.KampanjeTilbud &TYPE=C}

LOOPEN:
DO WHILE TRUE:
    RUN trg/genkampanjetilbid.p (OUTPUT trgInt).
    assign
      KampanjeTilbud.KampTilbId = trgInt
      NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN
        LEAVE LOOPEN.
END.


