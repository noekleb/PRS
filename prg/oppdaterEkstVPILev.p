/* run oppdaterEkstVPILev.p (piEkstVPILEvNr - 1000000, 'Axfood', 100, 'f:\home\lindbak\ankommet\axfood') */

DEF INPUT PARAMETER piEkstVPILevNr LIKE EkstVPILev.EkstVPILevNr NO-UNDO.
DEF INPUT PARAMETER cEDB-System    AS CHAR NO-UNDO.
DEF INPUT PARAMETER iNyLevNr       AS INT  NO-UNDO.
DEF INPUT PARAMETER cKatalog       AS CHAR NO-UNDO.

DEFINE BUFFER bEkstVPIFil FOR EkstVPIFil.
DEFINE BUFFER bEkstVPILev FOR EkstVPILev.

/* HK's import av ukjent vare */
IF piEkstVPILevNr > 0 AND piEkstVPILevNr < 1000000 THEN 
OPPRETT_OPPDATER:
DO TRANSACTION: 
    ASSIGN
      piekstVPILevNr = 1000000 + piEkstVPILevNr.

    /* Korreksjonsmelding fra HK til butikk legges her.             */
    /* Denne leverandøren skal være passiv på HK og aktiv i butikk. */
    FIND bEkstVPILev EXCLUSIVE-LOCK WHERE
         bEkstVPILEv.EkstVPILevNr = piEkstVPILevNr NO-ERROR.
    IF NOT AVAILABLE bEkstVPILev THEN
    DO:
        CREATE bEkstVPILev.
        ASSIGN
            bEkstVPILev.EkstVPILevNr = piEkstVPILevNr
            bEkstVPILev.KortNavn     = "Auto opprettet"
            bEkstVPILev.Navn         = "Auto opprettet"
            bEkstVPILev.AktivLev     = TRUE
            bEkstVPILEv.EDB-System   = cEDB-System
            bEkstVPILEv.LevNr        = iNyLevNr                
            .
        /* Oppretter et datasett */
        IF NOT CAN-FIND(FIRST VPIDatasett WHERE
                        VPIDatasett.EkstVPILevNr = bEkstVPILev.EkstVPILevNr) THEN
        DO:
            CREATE VPIDatasett.
            ASSIGN
                VPIDatasett.EkstVPILevNr = bEkstVPILev.EkstVPILEvNr.
        END.
    END.

    /* Tar bort det som ligger der fra før */
    FOR EACH bEkstVPIFil EXCLUSIVE-LOCK WHERE
        bEkstVPIFil.EkstVPILevNr = piEkstVPILevNr:
        DELETE bEkstVPIFil.
    END.

    IF NOT CAN-FIND(bEkstVPIFil WHERE
                    bEkstVPIFil.EkstVPILevNr = piEkstVPILevNr AND
                    bEkstVPIFil.VPIFilNr     = 1) THEN
    DO:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr          = piEkstVPILevNr 
            bEkstVPIFil.VPIFilNr              = 1
            bEkstVPIFil.VPIFilType            = 2
            bEkstVPIFil.VPIFilBeskrivelse     = "IPS EDI til Pricat"
            bEkstVPIFil.VPIFilNavn            = "ARTIKEL"
            bEkstVPIFil.VPIEkst               = 'EDI'
            bEkstVPIFil.VPIKatalog            = cKatalog
            bEkstVPIFil.VPIInnlesningsrutine  = "xedivpiinnles"
            bEkstVPIFil.VPIUtpakkingsrutine   = "xstdutpakk"
            bEkstVPIFil.VPIOperator           = 2
            bEkstVPIFil.VPIFilAktiv           = FALSE 
            .
        RELEASE bEkstVPIFil.
    END.
    IF NOT CAN-FIND(bEkstVPIFil WHERE
                    bEkstVPIFil.EkstVPILevNr = piEkstVPILevNr AND
                    bEkstVPIFil.VPIFilNr     = 2) THEN
    DO:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr          = piEkstVPILevNr 
            bEkstVPIFil.VPIFilNr              = 2
            bEkstVPIFil.VPIFilType            = 1
            bEkstVPIFil.VPIFilBeskrivelse     = "IPS EDI til Pricat"
            bEkstVPIFil.VPIFilNavn            = "GVPI"
            bEkstVPIFil.VPIEkst               = 'csv'
            bEkstVPIFil.VPIKatalog            = cKatalog
            bEkstVPIFil.VPIInnlesningsrutine  = "xsport1vpiinnles"
            bEkstVPIFil.VPIUtpakkingsrutine   = "xsport1vpiutpakk"
            bEkstVPIFil.VPIOperator           = 2
            bEkstVPIFil.VPIFilAktiv           = FALSE 
            .
        RELEASE bEkstVPIFil.
    END.
END. /* TRANSACTION OPPRETT_OPPDATER */
