TRIGGER PROCEDURE FOR CREATE OF Kupong.

def var trgDec as DEC no-undo.

{trg\c_w_trg.i &Fil="SkoTex.Kupong" &Type="C"}

LOOPEN:
DO WHILE TRUE:
    RUN trg/genkupongid (OUTPUT trgdec).
    IF CAN-FIND(Kupong WHERE
                Kupong.KupongId = int(trgDec)) THEN
        NEXT loopen.
    ELSE DO:
        assign
          Kupong.KupongId = trgDec
          NO-ERROR.
        LEAVE LOOPEN.
    END.
END.



