FOR EACH KORdreHode WHERE Opphav = 10:

    IF Telefon <> '' AND MobilTlf = '' THEN
    DO:
        MobilTlf = Telefon.
        /*
        DISPLAY
            KordreHode.KOrdre_Id
            Opphav
            Telefon
            MobilTlf
            .
        */
    END.
END.
