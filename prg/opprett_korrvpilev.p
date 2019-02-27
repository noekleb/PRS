DEF INPUT PARAMETER lHK AS LOG NO-UNDO.

DEF VAR pcTypeLst  AS CHAR NO-UNDO.
DEF VAR pcKortLst  AS CHAR NO-UNDO.
DEF VAR pcBeskrLst AS CHAR NO-UNDO.
DEF VAR piType     AS INT  NO-UNDO.
DEF VAR pcBeskr    AS CHAR NO-UNDO.
DEF VAR piLoop     AS INT  NO-UNDO.
DEF VAR pcKort     AS CHAR NO-UNDO.
DEF VAR piEkstVPILevNr AS INT NO-UNDO.
DEF VAR piCL       AS INT  NO-UNDO.
DEF VAR plOk       AS LOG  NO-UNDO.
DEF VAR pVPILevNr  AS INT  NO-UNDO.

DEF BUFFER bEkstVPIFil FOR EkstVPIFil.

{syspara.i 5 1 1 piCl INT}
{syspara.i 22 10 3 pVPILevNr INT}


STRONG_SCOOP:
DO FOR bEkstVPIFil TRANSACTION:

    /* HK's korreksjonslomme i VPI registeret */
    HK-KORR:
    DO: 
        ASSIGN
            piEkstVPILevNr = 1000000
            .

        /* Korreksjonsmelding fra HK til butikk legges her.             */
        /* Denne leverandøren skal være passiv på HK og aktiv i butikk. */
        IF NOT CAN-FIND(EkstVPILev WHERE
                        EkstVPILEv.EkstVPILevNr = piEkstVPILevNr) THEN
        DO:
            CREATE EkstVPILev.
            ASSIGN
                EkstVPILev.EkstVPILevNr = piEkstVPILevNr
                EkstVPILev.KortNavn     = "Korr HK"
                EkstVPILev.Navn         = "Korreksjon fra HK"
                EkstVPILev.AktivLev     = FALSE /*TRUE*/
                .
        END.
        /* VPIFil */
        IF NOT CAN-FIND(bEkstVPIFil WHERE
                        bEkstVPIFil.EkstVPILevNr = piEkstVPILevNr AND
                        bEkstVPIFil.VPIFilNr     = 1) THEN
        DO:
            CREATE bEkstVPIFil.
            ASSIGN
                bEkstVPIFil.EkstVPILEvNr          = piEkstVPILevNr 
                bEkstVPIFil.VPIFilNr              = 1
                bEkstVPIFil.VPIFilType            = 2
                bEkstVPIFil.VPIFilBeskrivelse     = "Korreksjon av lokale artikler"
                bEkstVPIFil.VPIFilNavn            = "KORRHK"
                bEkstVPIFil.VPIEkst               = STRING(piCL)
                bEkstVPIFil.VPIKatalog            = "c:\home\lindbak\ankommet"
                bEkstVPIFil.VPIInnlesningsrutine  = "xhkvpiinnles"
                bEkstVPIFil.VPIUtpakkingsrutine   = "xhkvpiutpakk"
                bEkstVPIFil.VPIOperator           = 2
                bEkstVPIFil.VPIFilAktiv           = FALSE /*NOT lHK*/ /* Passiv på hk, aktiv i butikk */
                .
            RELEASE bEkstVPIFil.
        END.
    END. /* HK-KORR */
    /* HK's import av ukjent vare */
    HK-UKJENT-VARE:
    DO: 
        ASSIGN
            piEkstVPILevNr = pVPILevNr
            piEkstVPILevNr = IF piEkstVPILevNr = 0 THEN 1000900 ELSE piEkstVPILevNr
            .

        /* Korreksjonsmelding fra HK til butikk legges her.             */
        /* Denne leverandøren skal være passiv på HK og aktiv i butikk. */
        IF NOT CAN-FIND(EkstVPILev WHERE
                        EkstVPILEv.EkstVPILevNr = piEkstVPILevNr) THEN
        DO:
            CREATE EkstVPILev.
            ASSIGN
                EkstVPILev.EkstVPILevNr = piEkstVPILevNr
                EkstVPILev.KortNavn     = "Ukjent vare"
                EkstVPILev.Navn         = "Ukjent vare fra ERP"
                EkstVPILev.AktivLev     = TRUE
                .
            /* Oppretter et datasett */
            IF NOT CAN-FIND(FIRST VPIDatasett WHERE
                            VPIDatasett.EkstVPILevNr = EkstVPILev.EkstVPILevNr) THEN
            DO:
                CREATE VPIDatasett.
                ASSIGN
                    VPIDatasett.EkstVPILevNr = EkstVPILev.EkstVPILEvNr.
            END.
        END.
        /* VPIFil */
        IF NOT CAN-FIND(bEkstVPIFil WHERE
                        bEkstVPIFil.EkstVPILevNr = piEkstVPILevNr AND
                        bEkstVPIFil.VPIFilNr     = 1) THEN
        DO:
            CREATE bEkstVPIFil.
            ASSIGN
                bEkstVPIFil.EkstVPILEvNr          = piEkstVPILevNr 
                bEkstVPIFil.VPIFilNr              = 1
                bEkstVPIFil.VPIFilType            = 2
                bEkstVPIFil.VPIFilBeskrivelse     = "Ukjent varer"
                bEkstVPIFil.VPIFilNavn            = "POSVPI"
                bEkstVPIFil.VPIEkst               = STRING(piEkstVPILevNr - 1000000)
                bEkstVPIFil.VPIKatalog            = "c:\home\lindbak\ankommet"
                bEkstVPIFil.VPIInnlesningsrutine  = "xhkvpiinnles"
                bEkstVPIFil.VPIUtpakkingsrutine   = "xhkvpiutpakk"
                bEkstVPIFil.VPIOperator           = 2
                bEkstVPIFil.VPIFilAktiv           = lHK /* Aktiv på hk, passiv i butikk */
                .
            RELEASE bEkstVPIFil.
        END.
    END. /* HK-UKJENT-VARE */

    /* Oppretter en ekst VPI leverandør for hver butikk som er satt opp som sentrallager. */
    /* PÅ HK leses opprettelsesmedlinger fra butikkene inn her.                           */
    /* I butikk er dette den lommen som meldinger til hk posteres i før de sendes.        */
    /* Derfor skal denne leverandøren alltid være aktiv på hk og passiv i butikk.         */
    SENTRALLAGER:
    FOR EACH Butiker NO-LOCK WHERE
        Butiker.Sentrallager = TRUE:

        ASSIGN
            piEkstVPILevNr = 1000000 + Butiker.Butik
            plOk = TRUE
            .

        /* VPI fra butikk */
        IF NOT CAN-FIND(EkstVPILev WHERE
                        EkstVPILEv.EkstVPILevNr = piEkstVPILevNr) THEN
        DO:
            CREATE EkstVPILev.
            ASSIGN
                EkstVPILev.EkstVPILevNr = piEkstVPILevNr
                EkstVPILev.KortNavn     = Butiker.KortNavn
                EkstVPILev.Navn         = Butiker.ButNamn
                EkstVPILev.AktivLev     = TRUE
                .
        END.
        /* VPIFil */
        FIND bEkstVPIFil WHERE
             bEkstVPIFil.EkstVPILevNr = piEkstVPILevNr AND
             bEkstVPIFil.VPIFilNr     = 1 NO-ERROR.
        IF NOT AVAILABLE bEkstVPIFil THEN
        DO:
            CREATE bEkstVPIFil.
            ASSIGN
                bEkstVPIFil.EkstVPILEvNr          = piEkstVPILevNr 
                bEkstVPIFil.VPIFilNr              = 1
                bEkstVPIFil.VPIFilType            = 2
                bEkstVPIFil.VPIFilBeskrivelse     = "Korreksjon av lokale artikler"
                bEkstVPIFil.VPIFilNavn            = "POSVPI"
                bEkstVPIFil.VPIEkst               = STRING(Butiker.Butik)
                bEkstVPIFil.VPIKatalog            = "c:\home\lindbak\ankommet"
                bEkstVPIFil.VPIInnlesningsrutine  = "xhkvpiinnles"
                bEkstVPIFil.VPIUtpakkingsrutine   = "xhkvpiutpakk"
                bEkstVPIFil.VPIOperator           = 2
                bEkstVPIFil.VPIFilAktiv           = lHK /* Aktiv på HK, passiv i butikk */
                .
            RELEASE bEkstVPIFil.
        END.
        /* Korrigerer de som fikk feil Sysinit.p */
        ELSE DO:
            ASSIGN
                bEkstVPIFil.VPIEkst               = STRING(Butiker.Butik)
                .
            RELEASE bEkstVPIFil.
        END.
    END. /* SENTRALLAGER */

    /* Er det ikke angitt på butikken hvem som er sentrallager, må syspara sjekkes. */
    /* Da benyttes syspara 5 1 1 .                                                  */
    IF plOk = FALSE AND piCL > 0 THEN
    FEIL-BRUK-SYSPARA:
    DO:
        FIND Butiker NO-LOCK WHERE
            Butiker.Butik = piCL NO-ERROR.
        IF NOT AVAILABLE Butiker THEN
            LEAVE FEIL-BRUK-SYSPARA.
        ASSIGN
            piEkstVPILevNr = 1000000 + piCL
            plOk = TRUE
            .

        /* VPI fra butikk */
        IF NOT CAN-FIND(EkstVPILev WHERE
                        EkstVPILEv.EkstVPILevNr = piEkstVPILevNr) THEN
        DO:
            CREATE EkstVPILev.
            ASSIGN
                EkstVPILev.EkstVPILevNr = piEkstVPILevNr
                EkstVPILev.KortNavn     = Butiker.KortNavn
                EkstVPILev.Navn         = Butiker.ButNamn
                EkstVPILev.AktivLev     = TRUE
                .
        END.
        /* VPIFil */
        IF NOT CAN-FIND(bEkstVPIFil WHERE
                        bEkstVPIFil.EkstVPILevNr = piEkstVPILevNr AND
                        bEkstVPIFil.VPIFilNr     = 1) THEN
        DO:
            CREATE bEkstVPIFil.
            ASSIGN
                bEkstVPIFil.EkstVPILEvNr          = piEkstVPILevNr 
                bEkstVPIFil.VPIFilNr              = 1
                bEkstVPIFil.VPIFilType            = 2
                bEkstVPIFil.VPIFilBeskrivelse     = "Korreksjon av lokale artikler"
                bEkstVPIFil.VPIFilNavn            = "POSVPI"
                bEkstVPIFil.VPIEkst               = STRING(piCL)
                bEkstVPIFil.VPIKatalog            = "c:\home\lindbak\ankommet"
                bEkstVPIFil.VPIInnlesningsrutine  = "xhkvpiinnles"
                bEkstVPIFil.VPIUtpakkingsrutine   = "xhkvpiutpakk"
                bEkstVPIFil.VPIOperator           = 2
                bEkstVPIFil.VPIFilAktiv           = lHK /* Aktiv på HK, passiv i butikk */
                .
            RELEASE bEkstVPIFil.
        END.
    END. /* FEIL-BRUK-SYSPARA */

    /* Rett opp feil "KORRPOS" */
    FOR EACH bEkstVPIFil EXCLUSIVE-LOCK WHERE
        bEkstVPIFil.EkstVPILEvNr > 1000000:
        IF bEkstVPIFil.VPIFilNavn = "KORRPOS" THEN
            bEkstVPIFil.VPIFilNavn = "POSVPI".
    END.

END. /* STRONG_SCOOP */
