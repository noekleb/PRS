    /* Opprettelse av import for salgsbudsjett */
    FIND EkstVPIFil WHERE
        EkstVPIFil.EkstVPILevNr = 892 AND
        EkstVPIFil.VPIFilNr     = 6 AND 
        EkstVPIFil.VPIFilType     = 12 NO-ERROR.
    IF NOT AVAILABLE EkstVPIFil THEN
        CREATE EkstVPIFil.
        
    DO:
        ASSIGN
            EkstVPIFil.EkstVPILEvNr         = 892 
            EkstVPIFil.VPIFilNr             = 6
            EkstVPIFil.VPIFilType           = 12
            EkstVPIFil.VPIFilBeskrivelse    = "Varemottak til RFID etikett"
            EkstVPIFil.VPIFilNavn           = "VMOTRFID"
            EkstVPIFil.VPIEkst              = "*"
            EkstVPIFil.VPIKatalog           = 'C:\NSoft\Polygon\PRS\kom\Barex\in'
            EkstVPIFil.VPIInnlesningsrutine = "xbxvmotRFIDinnles"
            EkstVPIFil.VPIUtpakkingsrutine  = "xstdutpakk"
            EkstVPIFil.VPIOperator          = 2
            EkstVPIFil.VPIFilAktiv          = FALSE
            .
    END. /* TRANSACTION */
