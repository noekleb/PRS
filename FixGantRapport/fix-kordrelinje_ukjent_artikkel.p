FOR EACH KORdreLinje WHERE 
    KORdrELinje.registrertDato >= 01/01/2019 AND 
    KORdreLinje.VareNr <> 'BETALT' AND 
    NOT CAN-FIND(artBas WHERE ArtBas.ArtikkelNr = DEC(KORdrELinje.VareNr)),
    FIRST KORdrEHode of KOrdrELinje:
    DISPLAY
        KOrdreLinje.KOrdre_Id
        KOrdreHode.EkstOrdreNr
        KOrdrEHode.LevStatus
        KOrdreLinje.RegistrertDato
        KORdreLinje.VareNr
        .
END.
