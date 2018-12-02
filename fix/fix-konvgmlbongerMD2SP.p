/* 
Konvertering av bonger fra MegaDisk til StorePoint struktur.
pfFlagg settes lik 99 ettersom datasettene behandles.
*/

DEF BUFFER bufDataSett FOR DataSett.

DATASETT:
FOR EACH DataSett NO-LOCK WHERE
    DataSett.pfFlagg < 99:

    PAUSE 0.
    DISPLAY 
        DataSett.DataSettId
        DataSEtt.ButikkNr
        DataSett.Dato
        .
    /* Konverterer bongene */
    BONGHODE:
    FOR EACH BongHode OF DataSett EXCLUSIVE-LOCK:
        /* Konverterer butikkNr */
        FIND ImpKonv WHERE ImpKonv.EDB-System = "MegaDisk" AND
             ImpKonv.Tabell     = "Butiker" AND
             ImpKonv.EksterntID = TRIM(string(BongHode.ButikkNr)) NO-LOCK NO-ERROR.
        IF AVAILABLE ImpKonv THEN
        DO:
            ASSIGN
                BongHode.ButikkNr = int(ImpKonv.InterntId)
                .
        END.
        BONGLINJE:
        FOR EACH BongLinje EXCLUSIVE-LOCK WHERE
            BongLinje.B_ID = BongHode.B_Id:

            /* Oppdaterer butikknummer - Alle linjer */
            ASSIGN
                BongLinje.ButikkNr = BongHode.ButikkNr
                .
            /* Konverterer strekkoden */
            FIND ImpKonv WHERE ImpKonv.EDB-System = "MegaDisk" AND
                 ImpKonv.Tabell     = "Strekkode" AND
                 ImpKonv.EksterntID = TRIM(string(BongLinje.Strekkode)) NO-LOCK NO-ERROR.
            IF AVAILABLE ImpKonv THEN
            DO:
                /* Lagrer originalverdier før konvertering */
                DO:
                    IF BongLinje.ForKonvertering = "" THEN
                        ASSIGN
                        BongLinje.ForKonvertering = FILL(CHR(1),20)
                        .
                    ASSIGN
                        entry(2,BongLinje.ForKonvertering,CHR(1)) = "Strekkode=" + STRING(BongLinje.Strekkode)
                        .
                END.
                ASSIGN
                    BongLinje.Strekkode = ImpKonv.InterntId
                    .
            END.

            IF CAN-DO("001,003,010",STRING(BongLinje.TTId,"999")) THEN
            BONGLINJEKONV:
            DO:
                /* Lagrer originalverdier før konvertering */
                DO:
                    IF BongLinje.ForKonvertering = "" THEN
                        ASSIGN
                        BongLinje.ForKonvertering = FILL(CHR(1),20)
                        .
                    ASSIGN
                        entry(1,BongLinje.ForKonvertering,CHR(1)) = "VarGr=" + STRING(BongLinje.VareGr)
                        entry(3,BongLinje.ForKonvertering,CHR(1)) = "LevNr=" + STRING(BongLinje.LevNr)
                        .
                END.
                /* Konverterer varegruppen og oppdaterer hovedgruppekobling. */
                /* Konverterer varegruppen TN 8/10-03 */
                FIND ImpKonv WHERE ImpKonv.EDB-System = "MegaDisk" AND
                     ImpKonv.Tabell     = "VarGr"    AND
                     ImpKonv.EksterntID = TRIM(string(BongLinje.VareGr)) NO-LOCK NO-ERROR.
                IF AVAILABLE ImpKonv THEN
                DO:
                    FIND VarGr NO-LOCK WHERE
                        VarGr.Vg = int(ImpKonv.InterntId) NO-ERROR.
                    IF AVAILABLE VarGr THEN
                        FIND HuvGr OF VarGr NO-LOCK NO-ERROR.
                    ASSIGN
                        BongLinje.VareGr         = int(ImpKonv.InterntId)
                        BongLinje.VareGruppeNavn = IF AVAILABLE VarGr
                                                     THEN VarGr.VgBeskr
                                                     ELSE BongLinje.VareGruppeNavn
                        BongLinje.HovedGr        = IF AVAILABLE VarGr
                                                     THEN VarGr.Hg
                                                     ELSE BongLinje.HovedGr
                        BongLinje.HovedGrBeskrivelse = IF AVAILABLE HuvGr
                                                         THEN HuvGr.HgBeskr
                                                         ELSE BongLinje.HovedGrBeskrivelse
                        .
                END.

                /* Konverterer leverandør */
                FIND ImpKonv WHERE ImpKonv.EDB-System = "MegaDisk" AND
                     ImpKonv.Tabell     = "LevBas" AND
                     ImpKonv.EksterntID = TRIM(string(BongLinje.LevNr)) NO-LOCK NO-ERROR.
                IF AVAILABLE ImpKonv THEN
                DO:
                    FIND LevBas NO-LOCK WHERE
                        LevBas.LevNr = int(ImpKonv.InterntId) NO-ERROR.
                    ASSIGN
                        BongLinje.LevNr          = int(ImpKonv.InterntId)
                        BongLinje.LevNavn        = IF AVAILABLE LevBas
                                                     THEN LevBas.LevNamn
                                                     ELSE BongLinje.LevNavn
                        .
                END.
            END. /* BONGLINJEKONV */
        END. /* BONGLINJE */
    END. /* BONGHODE */

    /* Flagger datasett ferdig */
    DO TRANSACTION:
        FIND bufDatasett EXCLUSIVE-LOCK WHERE
            recid(bufDataSett) = RECID(DataSett).
        ASSIGN
            bufDataSett.pfFlagg = 99
            .
        /* Konverterer butikkNr */
        FIND ImpKonv WHERE ImpKonv.EDB-System = "MegaDisk" AND
             ImpKonv.Tabell     = "Butiker" AND
             ImpKonv.EksterntID = TRIM(string(bufDataSett.ButikkNr)) NO-LOCK NO-ERROR.
        IF AVAILABLE ImpKonv THEN
        DO:
            ASSIGN
                bufDataSett.ButikkNr = int(ImpKonv.InterntId)
                .
        END.
        RELEASE bufDataSett.
    END.
END. /* DATASETT */
