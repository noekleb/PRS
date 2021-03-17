FOR EACH KOrdreHode NO-LOCK WHERE 
    KOrdreHode.LevStatus = '30',
    EACH KOrdreLinje OF KOrdreHode WHERE 
        NOT KOrdreLinje.VareNr = 'BETALT':

    IF CAN-FIND(ArtBas WHERE 
                    ArtBas.ArtikkelNr = DEC(KOrdreLinje.VareNr)) THEN
    DISPLAY
        KOrdreHode.EkstOrdreNr
        KOrdreHode.LEvStatus
        KOrdreLinje.VareNr
        .

END.
