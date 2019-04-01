CURRENT-WINDOW:WIDTH = 350.
FOR EACH KOrdreHode NO-LOCK,
    EACH KOrdreLinje OF KORdreHode NO-LOCK
        WHERE KOrdreLinje.Kode = '7322628610175'
    :

    DISPLAY
        KOrdrEHode.EkstOrdreNr
        KOrdreLinje.Kode
        KOrdreHode.SendingsNr
        KOrdreHode.ReturNr
        KOrdrelinje.Antall
    WITH WIDTH 350.
END.
