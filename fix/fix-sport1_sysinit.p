DEF BUFFER bekstvpilev FOR ekstvpilev.
DEF BUFFER bekstvpifil FOR ekstvpifil.

    /* Automatisk import av VPI fra ERP - Sport1HK */
    IF NOT CAN-FIND(EkstVPILev WHERE
                    EkstVPILEv.EkstVPILevNr = 903) THEN
    DO:
        CREATE EkstVPILev.
        ASSIGN
            EkstVPILev.EkstVPILevNr = 903
            EkstVPILev.KortNavn     = "Pricat fra ERP"
            EkstVPILev.Navn         = "Pricat fra ERP"
            EkstVPILev.AktivLev     = FALSE
            .
    END.
    /* VPI Sesongbok */
    IF NOT CAN-FIND(bEkstVPIFil WHERE
                    bEkstVPIFil.EkstVPILevNr = 903 AND
                    bEkstVPIFil.VPIFilNr     = 1) THEN
    DO:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr          = 903 
            bEkstVPIFil.VPIFilNr              = 1
            bEkstVPIFil.VPIFilType            = 2
            bEkstVPIFil.VPIFilBeskrivelse     = "Sesongbok Pricat fra ERP"
            bEkstVPIFil.VPIFilNavn            = "SBA"
            bEkstVPIFil.VPIEkst               = "csv"
            bEkstVPIFil.VPIKatalog            = "c:\home\lindbak\ankommet"
            bEkstVPIFil.VPIInnlesningsrutine  = "xsport1vpiinnles"
            bEkstVPIFil.VPIUtpakkingsrutine   = "xsport1vpiauto"
            bEkstVPIFil.VPIOperator           = 2
            bEkstVPIFil.VPIFilAktiv           = FALSE
            .
        RELEASE bEkstVPIFil.
    END.
    /* VPI Sesongbok */
    IF NOT CAN-FIND(bEkstVPIFil WHERE
                    bEkstVPIFil.EkstVPILevNr = 903 AND
                    bEkstVPIFil.VPIFilNr     = 2) THEN
    DO:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr          = 903 
            bEkstVPIFil.VPIFilNr              = 2
            bEkstVPIFil.VPIFilType            = 2
            bEkstVPIFil.VPIFilBeskrivelse     = "Endringer i sesong Pricat fra ERP"
            bEkstVPIFil.VPIFilNavn            = "AE"
            bEkstVPIFil.VPIEkst               = "csv"
            bEkstVPIFil.VPIKatalog            = "c:\home\lindbak\ankommet"
            bEkstVPIFil.VPIInnlesningsrutine  = "xsport1vpiinnles"
            bEkstVPIFil.VPIUtpakkingsrutine   = "xsport1vpiauto"
            bEkstVPIFil.VPIOperator           = 2
            bEkstVPIFil.VPIFilAktiv           = FALSE
            .
        RELEASE bEkstVPIFil.
    END.

    /* Automatisk innlesning av pakkefil (Sport1 ERP) */
    IF NOT CAN-FIND(EkstVPILev WHERE
                    EkstVPILEv.EkstVPILevNr = 904) THEN
    DO:
        CREATE EkstVPILev.
        ASSIGN
            EkstVPILev.EkstVPILevNr = 904
            EkstVPILev.KortNavn     = "Pricat pakke fra ERP"
            EkstVPILev.Navn         = "Pricat pakke fra ERP"
            EkstVPILev.AktivLev     = FALSE
            .
    END.
    /* VPI */
    IF NOT CAN-FIND(bEkstVPIFil WHERE
                    bEkstVPIFil.EkstVPILevNr = 904 AND
                    bEkstVPIFil.VPIFilNr     = 1) THEN
    DO:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr          = 904 
            bEkstVPIFil.VPIFilNr              = 1
            bEkstVPIFil.VPIFilType            = 2
            bEkstVPIFil.VPIFilBeskrivelse     = "Sesongbok Pakke vare struktur"
            bEkstVPIFil.VPIFilNavn            = "SPK"
            bEkstVPIFil.VPIEkst               = "csv"
            bEkstVPIFil.VPIKatalog            = "c:\home\lindbak\ankommet"
            bEkstVPIFil.VPIInnlesningsrutine  = "xsport1pakkeinnles"
            bEkstVPIFil.VPIUtpakkingsrutine   = "xstdutpakk"
            bEkstVPIFil.VPIOperator           = 2
            bEkstVPIFil.VPIFilAktiv           = FALSE
            .
        RELEASE bEkstVPIFil.
    END.
    IF NOT CAN-FIND(bEkstVPIFil WHERE
                    bEkstVPIFil.EkstVPILevNr = 904 AND
                    bEkstVPIFil.VPIFilNr     = 2) THEN
    DO:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr          = 904 
            bEkstVPIFil.VPIFilNr              = 2
            bEkstVPIFil.VPIFilType            = 2
            bEkstVPIFil.VPIFilBeskrivelse     = "Endringer i sesong Pakke vare struktur"
            bEkstVPIFil.VPIFilNavn            = "APK"
            bEkstVPIFil.VPIEkst               = "csv"
            bEkstVPIFil.VPIKatalog            = "c:\home\lindbak\ankommet"
            bEkstVPIFil.VPIInnlesningsrutine  = "xsport1pakkeinnles"
            bEkstVPIFil.VPIUtpakkingsrutine   = "xstdutpakk"
            bEkstVPIFil.VPIOperator           = 2
            bEkstVPIFil.VPIFilAktiv           = FALSE
            .
        RELEASE bEkstVPIFil.
    END.
    