CURRENT-WINDOW:WIDTH = 350.

FIND KORdrEHode WHERE 
    KOrdreHode.KORdre_Id = 1190000010.

FOR EACH KORdreLinje WHERE 
    KOrdreLinje.KOrdre_Id = KORdreHode.KOrdre_Id:


    DISPLAY
        KOrdrEHode.KORdre_Id
        KOrdreHode.ShipmentSendt
        KOrdrEHode.LevStatus
        KOrdreLinje.KOrdre_Id
        KOrdreLinje.KOrdreLinjeNr
        KOrdreLinje.KopiKORdreLinjeNr
        KOrdreLinje.Kode
        KOrdreLinje.Aktiv
        KOrdreLinje.Returnert

    WITH WIDTH 350.
END.
