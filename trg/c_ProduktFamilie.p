TRIGGER PROCEDURE FOR CREATE OF ProduktFamilie.
/*{trg/gensingleid.i SkoTex.ProduktFamilie ProdFamId}*/

def var trgDec as DEC no-undo.

{trg\c_w_trg.i &Fil=SkoTex.ProduktFamilie &TYPE=C}

LOOPEN:
DO WHILE TRUE:
    RUN trg/genprodfamid.p (OUTPUT trgdec).
    IF CAN-FIND(ProduktFamilie WHERE
                ProduktFamilie.ProdFamId = int(trgDec)) THEN
        NEXT loopen.
    ELSE DO:
        assign
          ProduktFamilie.ProdFamId = trgDec
          NO-ERROR.
        LEAVE LOOPEN.
    END.
END.


