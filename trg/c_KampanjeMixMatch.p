TRIGGER PROCEDURE FOR CREATE OF KampanjeMixMatch.
    
def var trgDec as DEC no-undo.

/*{trg/gensingleid.i SkoTex.KampanjeMixMatch KampId}*/
{trg\c_w_trg.i &Fil=SkoTex.KampanjeMixMatch &TYPE=C}

LOOPEN:
DO WHILE TRUE:
    RUN trg/genkampanjemixmatch_kampid.p (OUTPUT trgDec).
    assign
      KampanjeMixMatch.KampId = trgDec
      NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN
        LEAVE LOOPEN.
END.



