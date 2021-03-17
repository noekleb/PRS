/* Levering av kundeordre
  kordre_retur.p
  
  Parametere: 
   
  Opprettet: 4/12-19                  
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR ix           AS INT    NO-UNDO.
DEF VAR iLnr         AS INT    NO-UNDO.
DEF VAR cLevVareList AS CHAR   NO-UNDO.
DEF VAR fKOrdre_id   AS DEC    NO-UNDO.
DEF VAR bDelLev      AS LOG    NO-UNDO.
DEF VAR fLevAnt      AS DEC    NO-UNDO.
DEF VAR piLinjeNr    AS INT    NO-UNDO.
DEF VAR plArtikkelNr AS DEC    NO-UNDO.

/* For postering av bonger. */
DEF VAR lFilId      AS DEC NO-UNDO.
DEF VAR lDataSettId AS DEC NO-UNDO.
DEF VAR iButikkNr   AS INT NO-UNDO.
DEF VAR iGruppeNr   AS INT NO-UNDO.
DEF VAR iKasseNr    AS INT NO-UNDO.
DEF VAR plB_Id      AS DEC NO-UNDO.
DEF VAR piBongLinje AS INT NO-UNDO.
DEF VAR plLinjesum  AS DEC NO-UNDO.
DEFINE VARIABLE cTekst    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lTotAnt   AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99"NO-UNDO.
DEFINE VARIABLE bSalg AS LOG NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.
DEFINE VARIABLE rKundeordreBehandling AS cls.Kundeordre.KundeordreBehandling NO-UNDO.
rKundeordreBehandling  = NEW cls.Kundeordre.KundeordreBehandling( ) NO-ERROR.
ASSIGN 
  cLogg = 'kordre_retur' + REPLACE(STRING(TODAY),'/','')
  .
rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ).

rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Start.'
    ).

DEFINE BUFFER bufKOrdreLinje FOR KOrdreLinje.
DEFINE BUFFER bufKOrdreHode FOR KOrdreHode.

fKOrdre_id = DEC(ENTRY(1,icParam,";")).

FIND KOrdreHode NO-LOCK
     WHERE KOrdreHode.KOrdre_id = fKOrdre_id
     NO-ERROR.

FIND Kunde WHERE Kunde.KundeNr = KOrdreHode.KundeNr NO-LOCK NO-ERROR.
IF NOT AVAIL Kunde THEN DO:
  ocReturn = "Finner ikke kunde for kundeordre: " + STRING(KOrdreHode.kundenr).
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  ' + ocReturn 
      ).
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      'Slutt.'
      ).

  RETURN.
END.

IF AVAIL KOrdreHode THEN 
DO ON ERROR UNDO, LEAVE TRANSACTION:
  /* Henter butikken */
  FIND Butiker NO-LOCK WHERE
      Butiker.Butik = KOrdreHode.ButikkNr NO-ERROR.

  /* Klargjør bonghode. */
  RUN OpprettFil.
  RUN OpprettDatasett.
  RUN OpprettBongHode.
  FIND BongHode NO-LOCK WHERE
      BongHode.B_Id = plB_Id NO-ERROR.
  ASSIGN 
    ocReturn    = ""
    lTotAnt     = 0
    piBongLinje = 0
    plLinjesum  = 0
    .
  
  RUN behandleAktivlinje.
  RUN behandlePassivlinje.

  /* Vellyket fakturering */
  IF ocReturn = "Retur" THEN 
      DO: 
        FIND CURRENT KOrdreHode EXCLUSIVE-LOCK.
        ASSIGN 
            ocReturn = ""
            KOrdreHode.FakturertDato = TODAY 
            KOrdreHode.FakturertTid  = TIME
            .
        rKundeordreBehandling:setStatusKundeordre( INPUT STRING(KOrdreHode.KOrdre_Id),
                                                   INPUT (IF CAN-FIND(FIRST KOrdreLinje OF KOrdreHode WHERE KORdreLinje.Leveringsdato = ?)
                                                            THEN 40
                                                            ELSE  50
                                                          )
                                                  ).  
        /* Legger opp betalingstransaksjon på bong */
        RUN ferdigBong.
        FIND CURRENT KOrdreHode NO-LOCK.
      END.
  ELSE DO:
    ocReturn = "Ingen varelinje for kundeordre tilgjengelig for retur".
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  ' + ocReturn 
        ).
  END.
END.
ELSE DO:
   ocReturn = "Kundeordre ikke tilgjengelig for retur".
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  ' + ocReturn 
      ).
END.

obOk = TRIM(ocReturn) = "".
rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Slutt.'
    ).

RETURN.




/* **********************  Internal Procedures  *********************** */


PROCEDURE behandleAktivlinje:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

  /* Opprettelse av bonglinjene.                  */
  /* NB: Her leses bare aktive linjer med gyldige varenr. */
  LESKORDRELINJE:
  FOR EACH bufKOrdreLinje OF KOrdreHode NO-LOCK WHERE
      bufKOrdreLinje.Aktiv = TRUE:
      
      /* Skal ikke ha med betalingslinjene her. */
      FIND ArtBas NO-LOCK WHERE 
        ArtBas.ArtikkelNr = DEC(bufKOrdreLinje.VareNr) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN 
        NEXT.
      FIND VarGr OF ArtBas NO-LOCK NO-ERROR.
      
      rStandardFunksjoner:SkrivTilLogg(cLogg,
          '  Behandler aktive linjer.'
          ).
      
      /* Flagger overstyring av TTId til varesalg. */
      bSalg = FALSE.
      
      piBongLinje = piBongLinje + 1.
      
      /* Bare for sikkerhets skyld. */  
      IF AVAILABLE KORdreLinje THEN 
        RELEASE KORdreLinje.
          
      /* Aktive linjer på hvor der ikke er byttet vare, skal det skapes gjenkjøp på.                      */
      IF bufKOrdreLinje.Aktiv = TRUE AND bufKOrdreLinje.KopiKOrdreLinjeNr = 0 THEN 
      DO:  
        /* Og nå henter vi linjen som skal bearbedies. */
        FIND KOrdreLinje EXCLUSIVE-LOCK WHERE 
          KOrdreLinje.KOrdre_ID     = bufKOrdreLinje.KOrdre_Id AND 
          KOrdreLinje.KOrdreLinjeNr = bufKOrdreLinje.KOrdreLinjeNr NO-ERROR.
      END.
      
      /* Men hvis det er byttet vare på linjen og den aktive linjen peker på en linje på den opprinnelige ordren */
      /* Hvor vare IKKE er byttet, skal det opprettes en salgstransaksjon, ikke en returtrans.                   */
      ELSE IF bufKOrdreLinje.Aktiv AND bufKOrdreLinje.KopiKOrdreLinjeNr > 0 THEN 
      DO:
        /* Sjekker opprinnelig linje om det skal genereres salg eller gjenkjøp. */
        FIND KOrdreLinje EXCLUSIVE-LOCK WHERE 
          KOrdreLinje.KOrdre_ID     = KOrdreHode.RefKOrdre_Id AND 
          KOrdreLinje.KOrdreLinjeNr = bufKOrdreLinje.KopiKOrdreLinjeNr NO-ERROR.
        IF AVAILABLE KOrdreLinje AND 
          KOrdreLinje.KopiKOrdreLinjeNr = 0 THEN
            bSalg = TRUE.
        ELSE 
          bSalg = FALSE.
          
        /* Og nå henter vi linjen som skal bearbedies. */
        FIND KOrdreLinje EXCLUSIVE-LOCK WHERE 
          KOrdreLinje.KOrdre_ID     = bufKOrdreLinje.KOrdre_Id AND 
          KOrdreLinje.KOrdreLinjeNr = bufKOrdreLinje.KOrdreLinjeNr NO-ERROR.
      END.

      /* Vi må ha en linje her for å gjøre noe. */
      IF NOT AVAILABLE KOrdreLinje THEN 
        NEXT.

      IF AVAILABLE ArtBas THEN RELEASE ArtBas.
      ASSIGN
          plArtikkelNr = DEC(KOrdreLinje.VareNr) NO-ERROR.
      IF ERROR-STATUS:ERROR = FALSE AND plArtikkelNr > 0 THEN
      DO:
          FIND ArtBas NO-LOCK WHERE
              ArtBas.ArtikkelNr = plArtikkelNr NO-ERROR.
          IF AVAILABLE ArtBas THEN
              FIND FIRST ArtPris OF ArtBas NO-LOCK WHERE
                ArtPris.ProfilNr = Butiker.ProfilNr NO-ERROR.
          IF AVAILABLE ArtBas THEN
              FIND VarGr OF ArtBas NO-LOCK NO-ERROR.
      END.

      ASSIGN
          ocReturn = "Retur"
          .
            
      /* Oppretter bonglinje */
      CREATE_BONGLINJE:
      DO:
          FIND BongLinje EXCLUSIVE-LOCK WHERE
               BongLinje.ButikkNr = BongHode.ButikkNr AND
               BongLinje.GruppeNr = BongHode.GruppeNr AND
               BongLinje.KasseNr  = BongHode.KasseNr  AND
            BongLinje.Dato     = TODAY /*KOrdreLinje.Leveringsdato*/ AND
               BongLinje.BongNr   = BongHode.BongNr   AND
               BongLinje.LinjeNr  = piBongLinje NO-ERROR.
          IF NOT AVAILABLE BongLinje THEN
          DO:
            CREATE BongLinje. /* */
            ASSIGN
                BongLinje.B_Id         = BongHode.B_Id
                BongLinje.ButikkNr     = BongHode.ButikkNr 
                BongLinje.GruppeNr     = BongHode.GruppeNr 
                BongLinje.KasseNr      = BongHode.KasseNr  
                BongLinje.Dato         = TODAY /*KOrdreLinje.Leveringsdato*/    
                BongLinje.TransDato    = TODAY /*KOrdreLinje.Leveringsdato*/
                BongLinje.TransTid     = BongHode.Tid
                BongLinje.BongNr       = BongHode.BongNr   
                BongLinje.LinjeNr      = piBongLinje /*BongLinje*/
                .
          END.
      END. /* CREATE_BONGLINJE */
      
      /* Varelinjer */
      IF AVAILABLE ArtBas THEN 
      BONGLINJE:
      DO:
          /* Hvis sum antall < 0, legges betalingslinjer opp med negativt beløp. */
          lTotAnt = lTotant + KOrdreLinje.Antall.
 
          /* Henter lager og varekost for butikken */
          FIND Lager EXCLUSIVE-LOCK WHERE
            Lager.ArtikkelNr = DEC(KOrdreLinje.VareNr) AND
            Lager.Butik      = KOrdreHode.ButikkNr NO-ERROR NO-WAIT.
          IF NOT AVAILABLE Lager THEN
            DO:
              CREATE Lager.
              ASSIGN
                  Lager.ArtikkelNr = DEC(KOrdreLinje.VareNr)
                  Lager.Butik      = KOrdreHode.ButikkNr
                  Lager.VVareKost  = IF AVAILABLE ArtPris
                                       THEN ArtPris.Varekost[1]
                                       ELSE 0
                  .
            END.
          IF Lager.VVareKost = 0 THEN
              Lager.VVareKost  = IF AVAILABLE ArtPris
                                   THEN ArtPris.Varekost[1]
                                   ELSE 0.
          FIND CURRENT Lager NO-LOCK.

          FIND VarGR NO-LOCK OF ArtBas NO-ERROR.

          ASSIGN
            BongLinje.TTId       = IF (KOrdreLinje.Antall > 0 OR bSalg) 
                                     THEN 1  /* Kontant */
                                     ELSE 10 /* Retur   */
            BongLinje.ArtikkelNr = STRING(ArtBas.ArtikkelNr)
            BongLinje.Strekkode  = KOrdreLinje.Kode
            BongLinje.VareGr     = ArtBas.Vg
            BongLinje.LopeNr     = ArtBas.LopNr
            BongLinje.Storrelse  = KOrdreLinje.Storl
            BongLinje.BongTekst  = KOrdreLinje.Varetekst
            BongLinje.Antall     = KOrdreLinje.Antall
            BongLinje.LinjeSum   = abs(KOrdreLinje.NettoLinjeSum) + abs(KOrdreLinje.LinjeRabattKr)
            BongLinje.BongPris   = abs(BongLinje.LinjeSum)
            BongLinje.VVarekost  = abs(Lager.VVarekost) * abs(BongLinje.Antall)
            BongLinje.LinjeRab   = abs(KOrdreLinje.LinjeRabattKr)
            BongLinje.VareGruppeNavn = IF AVAILABLE VarGr
                                     THEN VarGr.VgBeskr
                                     ELSE ""

            BongLinje.Mva%       = ABS(KOrdreLinje.Mva%)
            BongLinje.MvaKr      = abs(KOrdreLinje.MvaKr)
            
            BongLinje.FeilKode   = 0
            BongLinje.NotatKode  = 0
            BongLinje.RefNr      = KOrdreLinje.RefNr 
            BongLinje.RefTekst   = KOrdreLinje.RefTekst
            .

          ASSIGN
            plLinjeSum = plLinjeSum + BongLinje.LinjeSum - KOrdreLinje.LinjeRabattKr
            .

          FIND FIRST Moms NO-LOCK WHERE
            Moms.MomsProc = BongLinje.Mva% NO-ERROR.
          IF AVAILABLE Moms THEN
            ASSIGN
            BongLinje.MvaGr         = Moms.MomsKod
            BongLinje.MvaGruppeNavn = Moms.Beskrivelse
            .
          RELEASE BongLinje.
      END. /* BONGLINJE */
  END. /* LESKORDRELINJE */

END PROCEDURE.

PROCEDURE behandlePassivlinje:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

  /* Opprettelse av bonglinjene.                  */
  LESKORDRELINJE:
  FOR EACH bufKOrdreLinje OF KOrdreHode NO-LOCK WHERE
      bufKOrdreLinje.Aktiv = FALSE:
      
      /* Skal ikke ha med betalingslinjene her. */
      FIND ArtBas NO-LOCK WHERE 
        ArtBas.ArtikkelNr = DEC(bufKOrdreLinje.VareNr) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN 
        NEXT.
      FIND VarGr OF ArtBas NO-LOCK NO-ERROR.
      
      rStandardFunksjoner:SkrivTilLogg(cLogg,
          '  Behandler passive linjer.'
          ).
      
      /* Flagger overstyring av TTId til varesalg. */
      bSalg = FALSE.
      
      piBongLinje = piBongLinje + 1.
      
      /* Bare for sikkerhets skyld. */  
      IF AVAILABLE KORdreLinje THEN 
        RELEASE KORdreLinje.
          
      /* Men hvis det er byttet vare på linjen og den peker på en linje på den opprinnelige ordren */
      /* hvor vare IKKE er byttet, skal det opprettes en gjenkjøps transaksjon.                    */
      IF bufKOrdreLinje.Aktiv = FALSE AND bufKOrdreLinje.KopiKOrdreLinjeNr > 0 THEN 
      DO:
        /* Sjekker opprinnelig linje om det skal genereres salg eller gjenkjøp. */
        FIND KOrdreLinje EXCLUSIVE-LOCK WHERE 
          KOrdreLinje.KOrdre_ID     = KOrdreHode.RefKOrdre_Id AND 
          KOrdreLinje.KOrdreLinjeNr = bufKOrdreLinje.KOrdreLinjeNr NO-ERROR.
        IF AVAILABLE KOrdreLinje AND 
          KOrdreLinje.KopiKOrdreLinjeNr = 0 THEN
        DO:
          /* Henter vi linjen som det skal opprettes returtrans på. */
          FIND KOrdreLinje EXCLUSIVE-LOCK WHERE 
            KOrdreLinje.KOrdre_ID     = bufKOrdreLinje.KOrdre_Id AND 
            KOrdreLinje.KOrdreLinjeNr = bufKOrdreLinje.KOrdreLinjeNr NO-ERROR.
        END.
        ELSE DO:
          /* Det skal ikke opprettes noen transaksjon. */ 
          IF AVAILABLE KOrdreLinje THEN 
            RELEASE KOrdreLinje.
        END. 
      END.
      
      /* Vi må ha en linje her for å gjøre noe. */
      IF NOT AVAILABLE KOrdreLinje THEN 
        NEXT.

      IF AVAILABLE ArtBas THEN RELEASE ArtBas.
      ASSIGN
          plArtikkelNr = DEC(KOrdreLinje.VareNr) NO-ERROR.
      IF ERROR-STATUS:ERROR = FALSE AND plArtikkelNr > 0 THEN
      DO:
          FIND ArtBas NO-LOCK WHERE
              ArtBas.ArtikkelNr = plArtikkelNr NO-ERROR.
          IF AVAILABLE ArtBas THEN
              FIND FIRST ArtPris OF ArtBas NO-LOCK WHERE
                ArtPris.ProfilNr = Butiker.ProfilNr NO-ERROR.
          IF AVAILABLE ArtBas THEN
              FIND VarGr OF ArtBas NO-LOCK NO-ERROR.
      END.

      ASSIGN
          ocReturn = "Retur"
          .
            
      /* Oppretter bonglinje */
      CREATE_BONGLINJE:
      DO:
          FIND BongLinje EXCLUSIVE-LOCK WHERE
               BongLinje.ButikkNr = BongHode.ButikkNr AND
               BongLinje.GruppeNr = BongHode.GruppeNr AND
               BongLinje.KasseNr  = BongHode.KasseNr  AND
            BongLinje.Dato     = TODAY /*KOrdreLinje.Leveringsdato*/ AND
               BongLinje.BongNr   = BongHode.BongNr   AND
               BongLinje.LinjeNr  = piBongLinje NO-ERROR.
          IF NOT AVAILABLE BongLinje THEN
          DO:
            CREATE BongLinje. /* */
            ASSIGN
                BongLinje.B_Id         = BongHode.B_Id
                BongLinje.ButikkNr     = BongHode.ButikkNr 
                BongLinje.GruppeNr     = BongHode.GruppeNr 
                BongLinje.KasseNr      = BongHode.KasseNr  
                BongLinje.Dato         = TODAY /*KOrdreLinje.Leveringsdato*/    
                BongLinje.TransDato    = TODAY /*KOrdreLinje.Leveringsdato*/
                BongLinje.TransTid     = BongHode.Tid
                BongLinje.BongNr       = BongHode.BongNr   
                BongLinje.LinjeNr      = piBongLinje /*BongLinje*/
                .
          END.
      END. /* CREATE_BONGLINJE */
      
      /* Varelinjer */
      IF AVAILABLE ArtBas THEN 
      BONGLINJE:
      DO:
          /* Hvis sum antall < 0, legges betalingslinjer opp med negativt beløp. */
          lTotAnt = lTotant + KOrdreLinje.Antall.
 
          ASSIGN
            BongLinje.TTId       = 10 /* Retur   */
            BongLinje.ArtikkelNr = STRING(ArtBas.ArtikkelNr)
            BongLinje.Strekkode  = KOrdreLinje.Kode
            BongLinje.VareGr     = ArtBas.Vg
            BongLinje.LopeNr     = ArtBas.LopNr
            BongLinje.Storrelse  = KOrdreLinje.Storl
            BongLinje.BongTekst  = KOrdreLinje.Varetekst
            BongLinje.Antall     = KOrdreLinje.Antall
            BongLinje.LinjeSum   = abs(KOrdreLinje.NettoLinjeSum) + abs(KOrdreLinje.LinjeRabattKr)
            BongLinje.BongPris   = abs(BongLinje.LinjeSum)
            BongLinje.VVarekost  = abs(Lager.VVarekost) * abs(BongLinje.Antall)
            BongLinje.LinjeRab   = abs(KOrdreLinje.LinjeRabattKr)
            BongLinje.VareGruppeNavn = IF AVAILABLE VarGr
                                     THEN VarGr.VgBeskr
                                     ELSE ""

            BongLinje.Mva%       = ABS(KOrdreLinje.Mva%)
            BongLinje.MvaKr      = abs(KOrdreLinje.MvaKr)
            BongLinje.FeilKode   = 0
            BongLinje.NotatKode  = 0
            BongLinje.RefNr      = KOrdreLinje.RefNr 
            BongLinje.RefTekst   = KOrdreLinje.RefTekst
            .

          ASSIGN
            plLinjeSum = plLinjeSum + BongLinje.LinjeSum - KOrdreLinje.LinjeRabattKr
            .
          FIND FIRST Moms NO-LOCK WHERE
            Moms.MomsProc = BongLinje.Mva% NO-ERROR.
          IF AVAILABLE Moms THEN
            ASSIGN
            BongLinje.MvaGr         = Moms.MomsKod
            BongLinje.MvaGruppeNavn = Moms.Beskrivelse
            .
          RELEASE BongLinje.
      END. /* BONGLINJE */
  END. /* LESKORDRELINJE */

END PROCEDURE.

PROCEDURE OpprettFil:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* Oppretter posten i filen. */
  IF NOT CAN-FIND(Filer WHERE
                  Filer.FilNavn   = "Salg fra kundeordre " + STRING(KOrdreHode.KOrdre_Id) AND
    Filer.Dato      = TODAY /* KOrdreHode.Dato*/ AND
                  Filer.Kl        = STRING(TIME,"HH:MM") AND
                  Filer.Storrelse = 0 AND
                  Filer.Katalog   = "Kundeordre"
                 ) THEN
  DO TRANSACTION:
    /* Finner FilId */
    FIND LAST Filer NO-LOCK NO-ERROR.
    IF AVAILABLE Filer THEN
      lFilId = Filer.FilId + 1.
    ELSE
      lFilId = 1.
    CREATE Filer.
    ASSIGN
      Filer.FilId       = lFilId
      Filer.FilNavn     = "Salg fra kunderodre " + STRING(KOrdreHode.KOrdre_Id) 
      Filer.Dato        = TODAY /*KOrdreHode.Dato*/
      Filer.Kl          = STRING(TIME,"HH:MM:SS") 
      Filer.Storrelse   = 0 
      Filer.Katalog     = "Kundeordre"
      Filer.AntLinjer   = 0
      Filer.FilType     = 1 
      Filer.Innlest     = TRUE
      Filer.InnlestDato = TODAY /*KOrdreHode.Dato*/
      Filer.InnlestKl   = TIME
      Filer.Oppdatert   = TRUE
      Filer.OppdatertDato = TODAY /*KOrdreHode.Dato*/
      Filer.OppdatertKl = TIME
      .
/*     RUN NyFilLogg (INPUT lFilId, STRING(TODAY) + " " +                      */
/*                           STRING(TIME,"HH:MM:SS")+ " " + userid("skotex") + */
/*                           " - Funnet på filkatalog ").                      */
    RELEASE Filer.
  END.

END PROCEDURE.

PROCEDURE OpprettDatasett:
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEF VAR piSettNr AS INT  NO-UNDO.
    DEF VAR pdKDato  AS DATE NO-UNDO.

    OPPRETTDATASETT:
    DO TRANSACTION:

      FIND FIRST KOrdreLinje OF KordreHode NO-LOCK NO-ERROR.
      IF AVAILABLE KOrdreLinje THEN
          pdKDato = TODAY. /*KOrdreLinje.Leveringsdato.*/
      ELSE 
          pdKDato = ?.

      /* Finner neste SettNr */
      FIND LAST Datasett NO-LOCK WHERE
          Datasett.ButikkNr = KOrdreHode.ButikkNr AND
        Datasett.GruppeNr = 1 /*KOrdreHode.GruppeNr */ AND
          Datasett.KasseNr  = KOrdreHode.KasseNr  AND
        Datasett.Dato     = TODAY /*pdKDato*/ AND
          DataSett.FilType  = 1 /* EL-Journal */
          USE-INDEX DataSett NO-ERROR.
      IF AVAILABLE DataSett THEN
          piSettNr = DataSett.SettNr + 1.
      ELSE DO:
          piSettNr = 1.
      END.

      /* Finner neste DataSettId */
      FIND LAST DataSett NO-LOCK
          USE-INDEX DataSettId NO-ERROR.
      IF AVAILABLE DataSett THEN
          lDataSettId = DataSett.DataSettId + 1.
      ELSE
          lDataSettId = 1.

      RELEASE DataSett. /* Ny post skal skapes. */

      IF NOT AVAILABLE DataSett THEN
      DO:
        CREATE DataSett.
        ASSIGN
            DataSett.DataSettId = lDataSettId
            DataSett.SettStatus = 2 /* Ankommet */
            DataSett.Behandlet  = 3 /* Behandlet */
            .
      END.

      ASSIGN
        DataSett.ButikkNr   = KOrdreHode.ButikkNr 
        DataSett.GruppeNr   = 1 /*KOrdreHode.GruppeNr */
        DataSett.KasseNr    = KOrdreHode.KasseNr
        DataSett.Dato       = TODAY /* pdKDato*/
        DataSett.SettNr     = piSettNr
        DataSett.Tid        = 0
        DataSett.FilId      = lFilId
        DataSett.FilType    = 1 /* EL-Journal */
        .
      RELEASE Datasett.
    END. /* OPPRETTDATASETT */
END PROCEDURE.

PROCEDURE OpprettBongHode:
    DEF VAR piBongNr    AS INT NO-UNDO.

    FIND DataSett NO-LOCK WHERE
        DataSett.DataSettId = lDataSettId NO-ERROR.

    piBongNr = 1.
    BLOKKEN:
    DO:
  /*       WHILE TRUE: */
        FIND LAST BongHode NO-LOCK WHERE
            BongHode.ButikkNr = KOrdreHode.ButikkNr AND
            BongHode.GruppeNr = 1 AND
            BongHode.KasseNr  = KOrdreHode.KasseNr  AND
            BongHode.Dato     = TODAY /*KOrdreHode.Dato*/ /*  AND
            BongHode.BongNr   = piBongNr */ USE-INDEX Bong NO-ERROR.
        IF AVAILABLE BongHode THEN
            piBongNr = BongHode.BongNr + 1.
  /*       ELSE               */
  /*           LEAVE BLOKKEN. */
    END. /* BLOKKEN */

    BONGHODE:
    DO TRANSACTION:
        /* Henter kasserer for kassen. */
        FIND FIRST ButikkForsalj NO-LOCK WHERE
            ButikkForsalj.Butik = DataSett.butikkNr NO-ERROR.
        IF AVAILABLE ButikkForsalj THEN
            FIND Forsalj OF KOrdreHode NO-ERROR.
        FIND Kunde OF KOrdreHode NO-LOCK NO-ERROR.
        IF AVAILABLE Kunde THEN 
          FIND FIRST KundeKort OF Kunde NO-LOCK NO-ERROR.
          
        FIND FIRST Medlemskort NO-LOCK WHERE 
            Medlemskort.InterntKKortId = kundekort.interntkkortid NO-ERROR.
        IF AVAILABLE MedlemsKort THEN 
          FIND Medlem OF MedlemsKort NO-LOCK NO-ERROR.
          
        CREATE BongHode.
        ASSIGN
          BongHode.ButikkNr      = KOrdreHode.ButikkNr 
          BongHode.GruppeNr      = 1 
          BongHode.KasseNr       = KOrdreHode.KasseNr  
          BongHode.Dato          = TODAY /*KOrdreHode.Dato*/
          BongHode.Tid           = TIME
          BongHode.BongNr        = piBongNr
          BongHode.BongStatus    = 0 /* Under klargjøring */
          BongHode.OpdKvit       = TRUE
          Bonghode.DataSettId    = DataSett.DataSettId
          BongHode.Utskriftskopi = "Utskriftskopi ikke mottat for kvittering " + 
                                   STRING(piBongNr) + "."
          BongHode.KassererNr    = KordreHode.ForsNr
          BongHode.KassererNavn  = IF AVAILABLE Forsalj
                                     THEN Forsalj.FoNamn
                                     ELSE "* Ukjent kasserer *"
          BongHode.KOrdre_Id     = KOrdreHode.KOrdre_Id
          BongHode.Konvertert    = TRUE
          BongHode.SelgerNr      = KOrdreHode.SelgerNr
          BongHode.KundeNr       = KOrdreHode.KundeNr
          BongHode.KundeKort     = IF AVAILABLE KundeKort THEN KundeKort.KortNr ELSE ''
          BongHode.KundeNavn     = IF AVAILABLE Kunde THEN Kunde.Navn ELSE ''
          .
        IF AVAILABLE Medlem THEN 
        ASSIGN
            BongHode.MedlemsNr   = Medlem.MedlemsNr 
            BongHode.MedlemsKort = MedlemsKort.KortNr
            BongHode.MedlemNavn  = TRIM(Medlem.ForNavn + ' ' + Medlem.Etternavn)
            .
        FIND CURRENT BongHode NO-LOCK.
        ASSIGN
            plB_Id = BongHode.B_Id
            . 
        FIND bufKOrdreHode EXCLUSIVE-LOCK WHERE 
            RECID(bufKOrdreHode) = RECID(KOrdreHode) NO-ERROR.
        IF AVAILABLE bufKOrdreHode THEN 
        DO:
            ASSIGN 
                bufKOrdreHode.Embalage = STRING(BongHode.B_Id)
                .
            RELEASE bufKOrdreHode. 
        END. 
            
    END. /* BONGHODE */
END PROCEDURE.

PROCEDURE ferdigBong:
    DEF VAR pBongDato AS DATE NO-UNDO.

    DO TRANSACTION:
        FIND CURRENT BongHode EXCLUSIVE-LOCK.
        BETALING:
        DO:
            FIND CURRENT BongHode EXCLUSIVE-LOCK.
            ASSIGN
                BongHode.Belop      = plLinjeSum  * (IF lTotant < 0 THEN -1 ELSE 1)
                BongHode.BongStatus = 5 /* Oppdatert */
                BongHode.Dato       = TODAY /*pBongDato*/
                .
        END. /* BETALING */

        FIND CURRENT BongHode NO-LOCK.
    END.

END PROCEDURE.
