TRIGGER PROCEDURE FOR WRITE OF VareBehHode.


    def var trgVareBehNr as DEC no-undo.
    DEF VAR trgButikkNr  LIKE Butiker.butik NO-UNDO.

    {trg\c_w_trg.i &Fil=SkoTex.VareBehHode &TYPE=W}

    IF VareBehHode.VareBehNr = 0 THEN
    LOOPEN:
    DO WHILE TRUE:
        RUN trg/genVareBehnr.p (OUTPUT trgVareBehNr,OUTPUT trgButikkNr).
        assign
          VareBehHode.VareBehNr  = trgVareBehNr
          NO-ERROR.
        IF ERROR-STATUS:ERROR = FALSE THEN
            LEAVE LOOPEN.
    END. /* LOOPEN */


