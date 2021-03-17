TRIGGER PROCEDURE FOR WRITE OF VareBokHode.


    def var trgVareBokNr as DEC no-undo.
    DEF VAR trgButikkNr  LIKE Butiker.butik NO-UNDO.

    {trg\c_w_trg.i &Fil=SkoTex.VareBokHode &TYPE=W}

    IF VareBokHode.VareBokNr = 0 THEN
    LOOPEN:
    DO WHILE TRUE:
        RUN trg/genvareboknr.p (OUTPUT trgVareBokNr,OUTPUT trgButikkNr).
        assign
          VareBokHode.VareBokNr  = trgVareBokNr
          NO-ERROR.
        IF ERROR-STATUS:ERROR = FALSE THEN
            LEAVE LOOPEN.
    END. /* LOOPEN */


