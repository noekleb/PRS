FOR EACH ArtBas WHERE Inn_Dato = ? EXCLUSIVE-LOCK:
    FIND FIRST BestHode NO-LOCK WHERE
        BestHode.ArtikkelNr = ArtBas.ArtikkelNr AND
        BestHode.BestStat >= 5 NO-ERROR.
    IF AVAILABLE BestHode THEN
    DO:
        FIND FIRST BestHLev OF BestHode NO-LOCK NO-ERROR.
        /*
        DISPLAY ArtBas.Inn_Dato
                BestHode.BestNr
                BestHode.BestillingsDato
                BestHLEv.LevertDato WHEN AVAILABLE BestHLev.
        */
        IF AVAILABLE BestHLev THEN
            ASSIGN ArtBas.Inn_Dato = BestHLev.LevertDato.
    END.
END.
