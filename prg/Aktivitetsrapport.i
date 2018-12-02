/* Lokale variabler */
DEF VAR plOms      AS DEC  NO-UNDO.
DEF VAR plRab      AS DEC  NO-UNDO.
DEF VAR piLoop     AS INT  NO-UNDO.
DEF VAR kvittoant  AS INT  NO-UNDO.
DEF VAR pcTTId     AS CHAR NO-UNDO.
DEF VAR piTid      AS INT  NO-UNDO.
DEFINE VARIABLE wVVarekost AS DECIMAL NO-UNDO.

DEFINE BUFFER bufAkt_Rapp FOR Akt_Rapp.
DEFINE BUFFER bufAktButiker FOR Butiker.

/* Flagg */
DEF VAR ny_flagg AS LOG INITIAL TRUE NO-UNDO.

/* \ker record-scoopet til bufAkt_Rapp. */
AKT:
DO FOR bufAkt_Rapp:

/* Leser transaksjonene og posterer på aktivitetsrapporten. */
TRANSRAD:
FOR EACH BongLinje NO-LOCK WHERE
    BongLinje.B_Id = BongHode.B_Id AND
    BongLinje.Makulert = FALSE AND
    CAN-DO("001,003,010",STRING(BongLinje.TTId,"999")):
    
    /* Non_Sale artikler skal ikke med her. */    
    IF NOT CAN-DO('0',STRING(SjekkNonSale(dec(BongLinje.ArtikkelNr)))) THEN NEXT TRANSRAD.
    
    /* Henter artikkelen. */
    IF DECIMAL(BongLinje.ArtikkelNr) > 0 THEN
    VAREKOST:
    DO: 
      wVVarekost = 0.
      FIND ArtBas NO-LOCK WHERE
        ArtBas.ArtikkelNr = DEC(BongLinje.ArtikkelNr) NO-ERROR.
      IF NOT AVAILABLE ArtBas THEN 
      DO:
        wVVarekost = BongLinje.VVareKost.
        LEAVE VAREKOST.
      END.
      FIND bufAktButiker NO-LOCK WHERE 
        bufAktButiker.Butik = BongLinje.ButikkNr NO-ERROR.
      IF AVAILABLE bufAktButiker THEN 
        FIND FIRST ArtPris OF ArtBas WHERE 
          ArtPris.ProfilNr = bufAktButiker.ProfilNr NO-ERROR.
      IF NOT AVAILABLE ArtPris THEN 
        FIND FIRST ArtPris OF ArtBas NO-LOCK NO-ERROR.
      FIND Lager NO-LOCK WHERE
        Lager.ArtikkelNr = ArtBas.ArtikkelNr AND
        Lager.Butik      = BongLinje.ButikkNr NO-ERROR.
      IF AVAILABLE Lager AND ArtBas.Lager = TRUE AND Lager.LagAnt > 0 THEN 
      DO:
        ASSIGN
          wVVareKost = Lager.VVareKost
          wVVareKost = (IF wVVareKost = ? THEN 0 ELSE wVVareKost).
      END.
      /* Null lager og åpen pris håndtering */
      IF wVVareKost <= 0 OR wVVareKost = ? THEN 
      DO:
        ASSIGN 
        wVVareKost = IF AVAILABLE ArtPris
                     THEN ArtPris.Varekost[IF ArtPris.Tilbud THEN 2 ELSE 1]
                     ELSE wVVareKost.
        wVVareKost = IF wVVAreKost = ? THEN 0 ELSE wVVareKost
        .             
        
      END.      
      /* Har artikkelen åpen pris, skal varekost settes fra kalkylen. */
      IF ArtBas.OPris THEN
        wVVareKost = 0.
    END. /* VAREKOST */
    ELSE wVVarekost = BongLinje.VVareKost.
    
    /* Disse skal ha kalkulert varekost fra normalkalkylen */
    IF CAN-DO('002,005,011',STRING(BongLinje.TTId,"999")) THEN 
      ASSIGN 
        wVVareKost = IF AVAILABLE ArtPris THEN ArtPris.VareKost[1]else wVVareKost
        wVVareKost = (IF wVVareKost = ? THEN 0 ELSE wVVareKost)
        .
    /* Sjekker om varekost er satt.                                       */
    /* Er det ikke satt noen varekost, merkes transaksjonen med feilkode. */
    IF wVVareKost = 0 THEN /* or wBrutto% *** Skal også utføres for brutto% artikkler */
      DO:
        IF VALID-HANDLE(h_PrisKo) THEN
          /* NB: Varekost skal regnes av pris eksklusive rabatter       */
          /* Mva trekkes fra i rutinen som kalles. Fordi hvis det er    */
          /* gitt rabatt, er det feil mva. MvaKr må da beregnes pånytt. */
          RUN HentVareKost IN h_PrisKo (INPUT BongLinje.ArtikkelNr, 
                                        INPUT BongLinje.ButikkNr, 
                                          INPUT (BongLinje.LinjeSum / ABSOLUTE(BongLinje.Antall)) - DECIMAL(BongLinje.MvaKr / ABSOLUTE(BongLinje.Antall)), 
                                        OUTPUT wVVareKost).
          IF wVVarekost = ? THEN wVVareKost = 0.
      END.
    
    ASSIGN
        pcTTId = STRING(BongLinje.TTId,"999")
        /* Postering bare på hele timer. */
        piTid = INTEGER(ENTRY(1,STRING(BongLinje.TransTid,"hh:mm:ss"),":") + '0000' /*
                    entry(2,STRING(BongLinje.TransTid,"hh:mm:ss"),":") + 
                    entry(3,STRING(BongLinje.TransTid,"hh:mm:ss"),":")*/)  
        .

    /* Beregner netto verdi */
    ASSIGN
    plRab = BongLinje.LinjeRab + BongLinje.SubtotalRab
    plOms = BongLinje.LinjeSum - plRab - BongLinje.MvaKr
    plRab = (100 * plRab   ) / (100 + BongLinje.Mva%)
    .
    /* Snur fortegn for gjennkjøpr og reklamasjon. */
    IF CAN-DO("003,004,010",STRING(BongLinje.TTId,"999")) THEN
        ASSIGN
        plRab = plRab * -1
        plOms = plOms * -1
        .

    /* Posterer på rapporten */
    POSTER:
    DO ON ERROR UNDO, RETRY:
        /* Henter aktivitetsposten som det skal posteres på. */
        /* Alle betalingstransaksjoner på en kvittering har  */
        /* samme tidspunkt.                                  */
        /* Henter posten etter aktuelt tidspunkt. */
        FIND FIRST bufAkt_Rapp WHERE 
            bufAkt_Rapp.dato  = BongLinje.dato    AND
            bufAkt_Rapp.butik = BongLinje.butik   AND
            bufAkt_Rapp.kasse = BongLinje.KasseNr AND
            bufAkt_Rapp.tid   = piTid
            EXCLUSIVE-LOCK NO-ERROR.
        /* Legger opp postene */
        IF NOT AVAILABLE bufAkt_Rapp THEN
        DO:
            /* Skapelsen */
            CREATE bufAkt_Rapp.

            /* Tilordner index. */
            ASSIGN bufAkt_Rapp.dato    = BongLinje.dato
                   bufAkt_Rapp.butik   = BongLinje.butik
                   bufAkt_Rapp.kasse   = BongLinje.KasseNr
                   bufAkt_Rapp.tid     = piTid
                   bufAkt_Rapp.tid_txt = SUBSTRING(STRING(piTid,"999999"),1,4)
                   bufAkt_Rapp.uke_dag = WEEKDAY(BongLinje.dato)
                   bufAkt_Rapp.mnd     = MONTH(BongLinje.dato).
        END.

        /* Oppdaterer aktivitetsposten med antall og verdi. */
        ASSIGN 
            bufAkt_Rapp.oms_ant  = bufAkt_Rapp.oms_ant  + BongLinje.Antall
            bufAkt_Rapp.oms_verd = bufAkt_Rapp.oms_verd + plOms
            bufAkt_Rapp.Mva_Kr   = bufAkt_Rapp.Mva_Kr   + (BongLinje.MvaKr * (IF BongLinje.Antall < 0 THEN -1 ELSE 1))
            bufAkt_Rapp.Svk      = bufAkt_Rapp.Svk      + (wVVareKost * (IF BongLinje.Antall < 0 THEN -1 ELSE 1))
            .

        /* Teller returer. */
        IF pcTTId = "010" THEN
        DO:
            /* Fortegn på antall er allerede negativt. */
            ASSIGN bufAkt_Rapp.ant_ret  = bufAkt_Rapp.ant_ret  + BongLinje.Antall
                   bufAkt_Rapp.verd_ret = bufAkt_Rapp.verd_ret - plOms.
        END.

        /* Sumerer antall par p} kvitteringen. */
        kvittoant = kvittoant + BongLinje.Antall.
    END. /* POSTER. */
END. /* TRANSRAD */

/* Posten skal være gjordt tilgjengelig tidligere.       */
/* Nb: Alle poster på en kvittering har samme tidspunkt. */
IF AVAILABLE bufAkt_Rapp THEN
TELLER:
DO:
    /* En gang pr. kvittering oppdateres antall kunder.                     */
    /* Er antall par på kvitteringen lik "0", telles ikke kvitteringen med. */
    /* Er POSTER avsluttet med i = 14, skal ikke kvittering telles med.     */
    IF piLoop <> 14 THEN
    DO:
        /* Antall kunder |kes ved en posistiv kvittering. */
        IF kvittoant > 0 THEN bufAkt_Rapp.ant_kunder = bufAkt_Rapp.ant_kunder + 1.

        /* Og trekkes ned ved en negativ kvittering. */
        ELSE IF kvittoant < 0 THEN
             bufAkt_Rapp.ant_kunder = bufAkt_Rapp.ant_kunder - 1.
    END.

    /* Antall kvitteringer skal alltid telles. */
    bufAkt_Rapp.ant_kvitto = bufAkt_Rapp.ant_kvitto - 1.
END. /* TELLER */
END. /* AKT */

/* Nullstiller flagg for kontroll av poster. */
ny_flagg = FALSE.



