DEF VAR plOms       AS DEC  NO-UNDO.
DEF VAR plTmp       AS DEC  NO-UNDO.
DEF VAR plDbKr      AS DEC  NO-UNDO.
DEF VAR plRab       AS DEC  NO-UNDO.
DEF VAR piHg        AS INT  NO-UNDO.
DEF VAR pcTTId      AS CHAR NO-UNDO.
DEF VAR piAntall    AS INT  NO-UNDO.
DEF VAR wMaxAvd     AS INT  NO-UNDO.
DEF VAR piLoop      AS INT  NO-UNDO.
DEF VAR pcAvdListe  AS CHAR NO-UNDO.
DEF VAR plVVAreKost AS DEC NO-UNDO.

DEF VAR piAvdeling AS INT EXTENT 9 NO-UNDO.

ASSIGN
    plVVAreKost = 0
    wMaxAvd   = 9
    .

FIND LAST Avdeling NO-LOCK NO-ERROR.
IF AVAILABLE Avdeling THEN
    ASSIGN
    wMaxAvd       = Avdeling.AvdelingNr
    piAvdeling[9] = Avdeling.AvdelingNr
    pcAvdListe    = STRING(Avdeling.AvdelingNr)
    .
LOOPEN:
FOR EACH Avdeling NO-LOCK:
    ASSIGN
        piLoop = piLoop + 1
        .
    IF piLoop > 8 THEN
        LEAVE LOOPEN.
    ASSIGN
        pcAvdListe         = pcAvdListe + 
                             (IF pcAvdListe <> ""
                                THEN ","
                                ELSE "") + 
                             STRING(Avdeling.AvdelingNr)
        piAvdeling[piLoop] = Avdeling.AvdelingNr
        .

END. /* LOOPEN */

/* Leser transaksjonene og posterer p} dagsrapport/selger.     */
/* Kode 16 er ikke med her, da den ligger innbakt i 02 linjen. */
    /* Henter butikken */
FIND Butiker NO-LOCK WHERE
     Butiker.Butik = BongHode.ButikkNr NO-ERROR.
TRANSRAD:
FOR EACH BongLinje NO-LOCK WHERE
    BongLinje.B_Id = BongHode.B_Id AND
    BongLinje.Makulert = FALSE AND
    CAN-DO("001,012,010",STRING(BongLinje.TTId,"999")):

    /* Non_Sale artikler skal ikke med her. */    
    IF NOT CAN-DO('0',STRING(SjekkNonSale(dec(BongLinje.ArtikkelNr)))) THEN NEXT TRANSRAD.
           
    ASSIGN
        plVVAreKost = BongLinje.VVareKost
        pcTTId     = STRING(BongLinje.TTId,"999")
        .

    /* Henter ArtBas */
    FIND ArtBas NO-LOCK WHERE
      ArtBas.Vg    = BongLinje.VareGr AND
      ArtBas.LopNr = BongLinje.LopeNr NO-ERROR.

    /* Henter kalkyle som inneholder kalkulert "Solgte varers kostpris". */
    IF AVAILABLE ArtBas AND AVAILABLE Butiker THEN
      DO:
        /* Alltid fra Sentrallager. */
        FIND FIRST ArtPris NO-LOCK WHERE
          ArtPris.Profil     = Butiker.Profil    AND
          ArtPris.ArtikkelNr = ArtBas.ArtikkelNr NO-ERROR.
        IF NOT AVAILABLE ArtPris THEN
          FIND FIRST ArtPris NO-LOCK WHERE
            ArtPris.Profil     = clButiker.Profil AND
            ArtPris.ArtikkelNr = ArtBas.ArtikkelNr NO-ERROR.
        /* Henter VVArekost fra Lager filen. */
        FIND Lager NO-LOCK WHERE
          Lager.ArtikkelNR = ArtBas.ArtikkelNR AND
          Lager.Butik      = Butiker.Butik NO-ERROR.
        IF NOT AVAILABLE Lager THEN
          FIND Lager NO-LOCK WHERE
            Lager.ArtikkelNR = ArtBas.ArtikkelNR AND
            Lager.Butik      = clButiker.Butik NO-ERROR.
      END.

    /* VVareKost pr. stk. */
    IF AVAILABLE Lager AND plVVAreKost = 0 THEN
      plVVAreKost = Lager.VVareKost.
    /* I disse tilfellene benyttes artikkelens kalkyle */
    IF AVAILABLE ArtPris AND 
       AVAILABLE ArtBas  AND 
       plVVAreKost = 0 THEN
    DO:
        /* Varekosten i KR hentes fra artikkelens kalkyle */
        IF ArtBas.OPris = FALSE THEN
            plVVAreKost = ArtPris.VareKost[IF ArtPris.Tilbud THEN 2 ELSE 1].
        /* Åpen pris - DB% fra kalkylen bestemmer varekostnaden ut fra salgsprisen. */
        ELSE DO:
            ASSIGN
                /* Omsetning eks. mva */
                plVVAreKost = (IF BongLinje.Antall >= 0
                              THEN (BongLinje.LinjeSum - 
                                    (BongLinje.LinjeRab + BongLinje.SubtotalRab) - 
                                    BongLinje.MvaKr)
                              ELSE (BongLinje.LinjeSum - 
                                    (BongLinje.LinjeRab + BongLinje.SubtotalRab) - 
                                    BongLinje.MvaKr) * -1)
                plVVAreKost = plVVAreKost / ABS(BongLinje.Antall)
                .
            RUN HentVareKost IN h_PrisKo (INPUT ArtBas.ArtikkelNr, 
                                          INPUT BongLinje.ButikkNr, 
                                          INPUT plVVAreKost, 
                                          OUTPUT plVVAreKost).
        END.
    END.

    /* Beregner netto verdi hvis det er ønsket på dagsrapporten. */
    IF iDags_Moms = 0 THEN
    DO:
        ASSIGN
            /* Total rabatt */
            plRab = (IF BongLinje.Antall >= 0
                       THEN (BongLinje.LinjeRab + BongLinje.SubtotalRab)
                       ELSE (BongLinje.LinjeRab + BongLinje.SubtotalRab) * -1)
            /* Omsetning eks mva. */
            plOms = (IF BongLinje.Antall >= 0
                       THEN (BongLinje.LinjeSum - ABS(plRab) - BongLinje.MvaKr)
                       ELSE (BongLinje.LinjeSum - ABS(plRab) - BongLinje.MvaKr) * -1)
            /* Trekker ut Mva fra rabatten. */
            plRab = (100 * plRab   ) / (100 + BongLinje.Mva%)
            /* Setter antall */
            piAntall = BongLinje.Antall
            .
        /* Beregner varekost hvis denne er ukjent. */
        IF plVVAreKost = 0 THEN
          DO:
            IF AVAILABLE ArtBas THEN
              FIND VarGr OF ArtBas NO-LOCK NO-ERROR.
            IF AVAILABLE VarGr THEN
              plVVAreKost = (plOms * VarGr.Kost_Proc) / 100.
          END.
        IF plVVAreKost = ? 
            THEN plVVAreKost = 0.

        /* Beregner DB hvis Artpris er ukjent. */
        IF NOT AVAILABLE ArtPris THEN
          DO:
            ASSIGN
                plDbKr = abs(plOms) * plDbFaktorNetto /* o.30. -  30% om vare ej finnes */
                plDbKr = IF piAntall < 0
                           THEN plDbKr * -1
                           ELSE plDbKr
                .
          END.
        /* Beregner DB */
        ELSE
            ASSIGN
                plDbKr = abs(plOms) - ABS(plVVAreKost /* * piAntall*/)
                plDbKr = IF piAntall < 0
                           THEN plDbKr * -1
                           ELSE plDbKr
                .
   
    END.
    /* Brutto verdi */
    ELSE DO:
      /* Oms. eks mva. */
      ASSIGN
          plRab = (IF BongLinje.Antall >= 0
                     THEN (BongLinje.LinjeRab + BongLinje.SubtotalRab)
                     ELSE (BongLinje.LinjeRab + BongLinje.SubtotalRab) * -1)
          plTmp = (IF BongLinje.Antall >= 0
                     THEN (BongLinje.LinjeSum - abs(plRab) - BongLinje.MvaKr)
                     ELSE (BongLinje.LinjeSum - abs(plRab) - BongLinje.MvaKr) * -1)
          /* Omsetning inkl mva og eksklusive rabatter. */
          plOms = (IF BongLinje.Antall >= 0
                     THEN (BongLinje.LinjeSum - abs(plRab))
                     ELSE (BongLinje.LinjeSum - abs(plRab)) * -1)
          .
      IF plVVAreKost = 0 THEN
        DO:
          IF AVAILABLE ArtBas THEN
            FIND VarGr OF ArtBas NO-LOCK NO-ERROR.
          IF AVAILABLE VarGr THEN
              plVVAreKost = (plTmp * VarGr.Kost_Proc) / 100.
        END.
      IF plVVAreKost = ? THEN
          plVVAreKost = 0.

      /* Beregner DB hvis ikke artpris finnes. */
      /* DB beregnes eksl mva.                 */
      IF NOT AVAILABLE ArtPris THEN
        DO:
          ASSIGN
              plDbKr = abs(plTmp) * plDbFaktorBrutto /* 0.24.  - 30% om vare ej finnes */
              plDbKr = IF BongLinje.Antall < 0
                         THEN plDbKr * -1
                         ELSE plDbKr
              .
        END.
      ELSE
        ASSIGN
            plDbKr = abs(plTmp) - ABS(plVVAreKost /** BongLinje.Antall*/)
            plDbKr = IF BongLinje.Antall < 0
                       THEN plDbKr * -1
                       ELSE plDbKr
            .
    END.

    /* Henter dagsrapport filen */
    FIND dags_rap WHERE dags_rap.butik = BongLinje.ButikkNr AND
                        dags_rap.dato  = BongLinje.dato
                        EXCLUSIVE-LOCK NO-ERROR.

    /* Legger opp posten hvis den ikke finnes. */
    IF NOT AVAILABLE dags_rap THEN
    DO:
        CREATE dags_rap.
        ASSIGN dags_rap.butikk = BongLinje.ButikkNr
               dags_rap.dato   = BongLinje.dato
               dags_rap.mnd    = MONTH(BongLinje.dato)
               dags_rap.aar    = YEAR(BongLinje.dato).
    END.

    /* Bestemmer avdelingen.                                   */
    /* Finnes ikke avdelingen, legges salget p} avd6           */
    /* Hovedgruppe "0" skal ogs} p} diverse.                   */
    FIND vargr WHERE 
        vargr.vg = BongLinje.VareGr NO-LOCK NO-ERROR.
    FIND HuvGr NO-LOCK WHERE
        HuvGr.Hg = VarGr.Hg NO-ERROR.
    IF AVAILABLE HuvGr THEN
        FIND Avdeling OF HuvGr NO-LOCK NO-ERROR.
    IF AVAILABLE Avdeling THEN 
    DO:
        IF CAN-DO(pcAvdListe,STRING(Avdeling.AvdelingNr)) THEN
            piHg = Avdeling.AvdelingNr.
        ELSE
            piHg = wMaxAvd.
    END.
    ELSE 
        piHg = wMaxAvd.

    IF piHg = 0 THEN 
        piHg = wMaxAvd.
    IF piHg > wMaxAvd THEN
        piHg = wMaxAvd.

    /* Gjør posteringene */
    IF piHg = piAvdeling[1] THEN
    DO:
        dags_rap.tb1 = dags_rap.tb1 + plDbKr.
        dags_rap.hg1_oms = dags_rap.hg1_oms + plOms.
    END.
    ELSE IF piHg = piAvdeling[2] THEN
    DO:
        dags_rap.tb2 = dags_rap.tb2 + plDbKr.
        dags_rap.hg2_oms = dags_rap.hg2_oms + plOms.
    END.
    ELSE IF piHg = piAvdeling[3] THEN
    DO:
        dags_rap.tb3 = dags_rap.tb3 + plDbKr.
        dags_rap.hg3_oms = dags_rap.hg3_oms + plOms.
    END.
    ELSE IF piHg = piAvdeling[4] THEN
    DO:
        dags_rap.tb4 = dags_rap.tb4 + plDbKr.
        dags_rap.hg4_oms = dags_rap.hg4_oms + plOms.
    END.
    ELSE IF piHg = piAvdeling[5] THEN
    DO:
        dags_rap.tb5 = dags_rap.tb5 + plDbKr.
        dags_rap.hg5_oms = dags_rap.hg5_oms + plOms.
    END.
    ELSE IF piHg = piAvdeling[6] THEN
    DO:
        dags_rap.tb6 = dags_rap.tb6 + plDbKr.
        dags_rap.hg6_oms = dags_rap.hg6_oms + plOms.
    END.
    ELSE IF piHg = piAvdeling[7] THEN
    DO:
        dags_rap.tb7 = dags_rap.tb7 + plDbKr.
        dags_rap.hg7_oms = dags_rap.hg7_oms + plOms.
    END.
    ELSE IF piHg = piAvdeling[8] THEN
    DO:
        dags_rap.tb8 = dags_rap.tb8 + plDbKr.
        dags_rap.hg8_oms = dags_rap.hg8_oms + plOms.
    END.
    ELSE IF piHg = piAvdeling[9] THEN
    DO:
        dags_rap.tb9 = dags_rap.tb9 + plDbKr.
        dags_rap.hg9_oms = dags_rap.hg9_oms + plOms.
    END.

    /* Retur av varer solgt i andre butikker.                 */
    /* Denne typen retur kjennes ved at de to f|rste siffrene */
    /* i selgernummeret er forskjellig fra butikknummeret.    */
    /* NB: 16 kommer ikke hit lenger. Den er innbakt i 02.    */
    /*     16 transen tas bort ved konvertering av bonger.    */
    IF CAN-DO("012,010,004",pcTTId)      AND
       BongLinje.ReturButikk <> BongLinje.ButikkNr THEN
    RET_KORR:
    DO:
        /* Henter dagsrapport filen */
        FIND dags_rap WHERE dags_rap.butik = BongLinje.ReturButikk AND
                            dags_rap.dato  = BongLinje.dato
                            EXCLUSIVE-LOCK NO-ERROR.

        /* Legger opp posten hvis den ikke finnes. */
        IF NOT AVAILABLE dags_rap THEN
        DO:
            CREATE dags_rap.
            ASSIGN dags_rap.butikk = BongLinje.ReturButikk
                   dags_rap.dato   = BongLinje.dato
                   dags_rap.mnd    = MONTH(BongLinje.dato)
                   dags_rap.aar    = YEAR(BongLinje.dato).
        END.

        /* Posterer korreksjonsbel|pet. */
        ASSIGN 
            dags_rap.retur_korr = dags_rap.retur_korr - plOms
            .
    END. /* RET_KORR */
END. /* TRANSRAD. */

