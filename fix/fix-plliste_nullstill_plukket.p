FOR EACH PlListeHode WHERE
    CAN-DO("9,10",STRING(PlListeHode.PlListeId)):
    ASSIGN
        PlListeHode.DatoPlukket   = ?
        PlListeHode.TidPlukket    = 0
        PlListeHode.AntallPlukket = 0
        PlListeHode.OverfortDato  = ?
        PlListeHode.BuntNr        = 0
        .
    FOR EACH PlListeLinje OF PlListeHode:
        ASSIGN
            plListeLinje.AntallPlukket = 0.
    END.

END.
