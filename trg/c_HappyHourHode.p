TRIGGER PROCEDURE FOR CREATE OF HappyHourHode.

def var trgDec as DEC no-undo.

/*{trg/gensingleid.i SkoTex.HappyHourHode HapHourId}*/
{trg\c_w_trg.i &Fil=SkoTex.HappyHourHode &TYPE=C}

LOOPEN:
DO WHILE TRUE:
    RUN trg/genhappyhaourid.p (OUTPUT trgdec).
    IF CAN-FIND(HappyHourHode WHERE
                HappyHourHode.HapHourId = int(trgDec)) THEN
        NEXT loopen.
    ELSE DO:
        assign
          HappyHourHode.HapHourId = trgDec
          NO-ERROR.
        LEAVE LOOPEN.
    END.
END.



