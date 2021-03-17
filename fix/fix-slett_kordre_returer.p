FOR EACH KORdreHode EXCLUSIVE-LOCK WHERE
    KOrdreHode.EkstOrdreNr BEGINS 'RETUR' AND
    KOrdreHode.EDato <= 12/31/2015:

    FOR EACH KORdreLin OF KOrdreHode:
        DELETE KOrdreLinje.
    END.
    DELETE KORdreHode.
 /*
    DISPLAY
        KOrdreHode.EkstOrdreNr
        KOrdreHode.EDato 
        .
        */
END.
