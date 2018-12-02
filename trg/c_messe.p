TRIGGER PROCEDURE FOR CREATE OF Messe.

    def var trgMesseNr as DEC no-undo.
    DEF VAR trgButikkNr LIKE Butiker.butik NO-UNDO.

    {trg\c_w_trg.i &Fil=SkoTex.Messe &TYPE=C}

    LOOPEN:
    DO WHILE TRUE:
        RUN trg/genmessenr.p (OUTPUT trgMesseNr,OUTPUT trgButikkNr).
        assign
          Messe.MesseNr  = trgMesseNr
          NO-ERROR.
        IF ERROR-STATUS:ERROR = FALSE THEN
            LEAVE LOOPEN.
    END. /* LOOPEN */




