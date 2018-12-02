TRIGGER PROCEDURE FOR WRITE OF PrintLogg.


    def var trgLoggNr as DEC no-undo.
    DEF VAR trgButikkNr  LIKE Butiker.butik NO-UNDO.

    {trg\c_w_trg.i &Fil=SkoTex.PrintLogg &TYPE=W}

    IF PrintLogg.LoggNr = 0 THEN
    LOOPEN:
    DO WHILE TRUE:
        RUN trg/genprintloggnr.p (OUTPUT trgLoggNr,OUTPUT trgButikkNr).
        assign
          PrintLogg.LoggNr  = trgLoggNr
          NO-ERROR.
        IF ERROR-STATUS:ERROR = FALSE THEN
            LEAVE LOOPEN.
    END. /* LOOPEN */


