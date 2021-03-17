
DEF STREAM Ut.

OUTPUT STREAM Ut TO VALUE('konv\kordrelinjer.csv').
FOR EACH KOrdreLinje NO-LOCK,
    FIRST KORdrEHode OF KOrdreLinje NO-LOCK
    BY KOrdreHode.KOrdre_Id
    BY KOrdreLinje.KOrdreLinjeNr:
    PUT STREAM ut UNFORMATTED
        KOrdreHode.KOrdre_Id ';'
        KOrdreHode.EkstOrdreNr ';'
        KOrdreHode.SendingsNr ';'
        KOrdreLinje.Kode
        SKIP.
END.
