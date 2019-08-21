CURRENT-WINDOW:WIDTH = 350.
FOR EACH KOrdreHode WHERE 
    KOrdreHode.EkstOrdreNr MATCHES '500075169*',
    EACH KOrdreLinje OF KOrdreHode:

    DISPLAY
        KORdreHode.RefKORdre_Id
        KOrdreLinje.KORdre_Id
        KOrdreLinje.KOrdreLinjeNr
        KORdreLinje.ReturKodeId
        KOrdreLinje.Returnert
        KOrdreLinje.VareNr
    WITH WIDTH 350. 
END.
