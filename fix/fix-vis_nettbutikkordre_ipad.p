CURRENT-WINDOW:WIDTH = 350.
FOR EACH KOrdreHode NO-LOCK WHERE 
    KOrdreHode.RegistrertDato >= 01/01/2017,
    FIRST KOrdreLinje OF KOrdreHode NO-LOCK WHERE 
        KOrdreLinje.VareNr = 'BETALT':

    IF NOT can-do('Visa,mastercard',KOrdreLinje.Varetekst) THEN 
        NEXT.

    DISPLAY
        KOrdreHode.RegistrertDato
        KOrdreLinje.VareNr
        KOrdreLinje.Varetekst
        KOrdrEHode.KOrdre_Id
        KOrdreHode.EkstOrdreNr
        KordreHode.ButikkNr
    WITH WIDTH 350.
END.
