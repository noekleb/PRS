DEF VAR pcTekst AS CHAR NO-UNDO.

CURRENT-WINDOW:WIDTH = 140.
FOR EACH KundeBetTRans EXCLUSIVE-LOCK WHERE
    /*KundeBetTrans.KundeNr = 200000098 AND*/
    KundeBetTrans.Dato < 12/14/2001 AND
    KundeBEtTrans.BongId > 0
    BREAK 
    BY KundeBetTrans.Butik
    BY KundeBetTrans.KassaNr
    BY KundeBEtTrans.Dato
    BY KundeBetTrans.BongId:

    IF CAN-FIND(FIRST KundeTrans WHERE
                KundeTrans.KundeNr = KundeBetTrans.KundeNr AND
                KundeTrans.Butik = KundeBEtTrans.Butik AND
                KundeTrans.KassaNr = KundeBetTrans.KassaNr AND
                KundeTrans.Dato    = KundeBetTrans.DAto AND
                KundeTrans.BongId  = KundeBEtTrans.BongId AND
                KundeTrans.Antall  < 0) THEN
    DO:
        ASSIGN
            KundeBetTrans.Belop = abs(KundeBetTrans.Belop) * -1
            pcTekst = "*"
            .
    END.
    ELSE
        pcTekst = "".

    FIND TransType NO-LOCK WHERE
        TransType.TTId = KundeBetTrans.TTId.

    PAUSE 0.
    DISPLAY KundeBetTrans.ttid
        TransType.Beskrivelse FORMAT "x(10)"
        KundeBetTrans.Butik
        KundeBetTRans.KassaNr
        KundeBetTrans.BongId
        KundeBetTrans.Dato
        KundeBetTrans.Belop
        KundeBetTrans.KundeNR
        pcTekst FORMAT "x(2)"
        WITH WIDTH 138
        .
END.
