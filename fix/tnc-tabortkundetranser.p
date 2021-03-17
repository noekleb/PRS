CURRENT-WINDOW:WIDTH = 200.
    
FOR EACH Kunde NO-LOCK WHERE
    Kunde.KundeNR > 200000082 AND
    CAN-FIND(FIRST Medlem WHERE
                 Medlem.KundeNr = Kunde.KundeNR):

    DISPLAY kunde.kundenr.

    FOR EACH KundeTrans OF Kunde:
        IF NOT CAN-FIND(FIRST MedTrans WHERE
                        MEdTrans.Butik   = Kunde.Butik AND
                        MEdTrans.TransNr = KundeTrans.TransNr AND
                        MEdTrans.SeqNR   = KundeTrans.SeqNr)
                        THEN
        DO:
            PAUSE 0.
            DISPLAY
            Kunde.KundeNR
            KundeTrans.Butik
            KundeTrans.TransNR
            KundeTrans.SeqNr
            KundeTrans.KassaNr
            KundeTrans.Dato
            KundeTrans.BongId
            KundeTrans.BongLinje
            KundeTrans.TTId
            KundeTrans.ANtall
            KundeTrans.Pris
            WITH WIDTH 198
            .
            /*DELETE KundeTrans.*/
        END.
    END.

END.
