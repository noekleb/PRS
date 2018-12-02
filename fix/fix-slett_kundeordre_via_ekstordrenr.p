FOR EACH KOrdreHode WHERE EkstOrdreNr = '2408':
    FOR EACH KOrdreLinje OF KOrdreHode:
        DELETE KOrdreLinje.
    END.
    DELETE KORdreHode.
END.
