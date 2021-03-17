
DEF VAR wSum AS DEC NO-UNDO.

FOR EACH KundeTrans NO-LOCK WHERE
    Kundetrans.kundenr = 200000088:

    wSum = wSum + (IF KundeTrans.Antall < 0
                     THEN (KundeTrans.Pris - KundeTrans.RabKr
                           - KundeTrans.SubTotalRAb) * -1
                     ELSE (KundeTrans.Pris - KundeTrans.RabKr
                           - KundeTrans.SubTotalRAb)).
END.
DISPLAY 
    wSum.
