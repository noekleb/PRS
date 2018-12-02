CURRENT-WINDOW:WIDTH = 300.

FOR EACH Butiker NO-LOCK WHERE
    CAN-FIND(FIRST BongHode WHERE 
             BongHode.butikkNr = Butiker.butik):
    IF CAN-DO('15,16,20',STRING(butiker.butik)) THEN
        NEXT.

    DISPLAY
        butiker.butik
        butiker.butNamn FORMAT "x(30)"

        .

    FOR EACH BongHode NO-LOCK WHERE
        BongHode.ButikkNr = Butiker.Butik AND
        BongHode.Dato >= 06/11/2015:

        IF BongHode.KundENr = 0 THEN
            NEXT.

        FIND Kunde NO-LOCK WHERE
            Kunde.KundeNr = BongHode.KundeNr NO-ERROR.
        DISPLAY
            bongHode.KasseNr
            BongHode.Dato
            BongHode.KundeNr
            Kunde.Navn
        WITH WIDTH 300.
    END.
END.
