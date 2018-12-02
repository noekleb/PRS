DEF VAR lFraKundeNr AS DEC FORMAT ">>>>>>>>>>>>9" NO-UNDO.
DEF VAR lTilKundeNr AS DEC FORMAT ">>>>>>>>>>>>9" NO-UNDO.

UPDATE
    lFraKundeNr
    lTilKundeNr
    .

FOR EACH BongHode WHERE 
    BongHode.KundeNr = lFraKundeNr:

    ASSIGN
        BongHode.KundeNR = lTilKundeNr
        .
END.
FOR EACH KundeTrans WHERE
    KundeTrans.KundeNr = lFraKundeNr:

    ASSIGN
        KundeTrans.KundeNR = lTilKundeNr
        .
END.

FOR EACH KundeBetTrans WHERE
    KundeBetTrans.KundeNr = lFraKundeNr:

    ASSIGN
        KundeBetTrans.KundeNR = lTilKundeNr
        .
END.

FIND Kunde WHERE
    Kunde.KundeNr = lFraKundeNr.
FOR EACH KundeKort OF Kunde:
    DELETE KundeKort.
END.

DELETE Kunde.

