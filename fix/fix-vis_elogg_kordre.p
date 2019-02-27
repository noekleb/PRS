CURRENT-WINDOW:WIDTH = 350.

FOR EACH ELogg /*WHERE tabellnavn = 'KOrdreHode'*/:
   
    FIND KOrdreHode NO-LOCK WHERE
        KORdreHode.KOrdre_Id = DEC(ELogg.Verdier) no-error.

    DO:
        DISPLAY
            ELogg.TabellNavn
            ELogg.EksterntSystem
            ELogg.Verdier
            ELogg.EDato
            STRING(ELogg.ETid,"HH:MM:SS")

            WITH WIDTH 350.
        /*
        IF AVAILAB KORdrEHode THEN
        DISPLAY 
            KORdreHode.KORdre_Id
            KOrdreHode.Levstatus
            KOrdreHode.FakturertDato
            STRING(KOrdreHode.FAkturertTid,"HH:MM:SS")
            KOrdreHode.RegistrertDato
            KORdreHode.EDato
            STRING(KORdreHode.ETid,"HH:MM:SS")
        WITH WIDTH 350.
        */
    END.
END.

/*
OUTPUT TO 'GurresListe.csv'.
FOR EACH KOrdreHode WHERE
    KORdreHode.ButikkNr = 15 AND
    KOrdreHode.LevStatus = '50' AND
    KOrdreHode.FakturertDato = TODAY:
    
    FIND ELogg WHERE
        ELogg.TabellNavn = 'KOrdreHode' AND
        ELogg.EksterntSystem = 'WebBut' and
        ELogg.Verdier = STRING(KOrdreHode.KOrdre_Id) NO-ERROR.


    PUT UNFORMATTED 
        KOrdreHode.KOrdre_Id  ';'
        KOrdreHode.EkstOrdreNr  ';'
        KOrdreHode.FakturertDato  ';'
        STRING(KOrdreHode.FakturertTid,"HH:MM:SS") ';'
        (IF AVAILABLE ELogg THEN ELogg.TabellNavn ELSE '') ';'
        (IF AVAILABLE ELogg THEN ELogg.EksterntSystem ELSE '') ';'
        (IF AVAILABLE ELogg THEN ELogg.Verdier ELSE '') 

    SKIP.
END.
*/
