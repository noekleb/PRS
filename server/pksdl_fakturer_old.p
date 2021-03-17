/* Fakturering av pakkseddel
   Parametere: <PkSdlId>
   
   Opprettet: 4/8-17    
    Eks1: DYNAMIC-FUNCTION("runproc","pksdl_fakturer.p",STRING(ihBuffer:BUFFER-FIELD("PkSdlId"):BUFFER-VALUE),?)  
    Eks2: run pksdlfakturer.p (STRING(PkSdlId),?,'',output ocReturn, output obOk).
                
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR lPkSdlId   AS DEC    NO-UNDO.

DEF VAR ix           AS INT    NO-UNDO.
DEF VAR iLnr         AS INT    NO-UNDO.
DEF VAR cLevVareList AS CHAR   NO-UNDO.
DEF VAR bDelLev      AS LOG    NO-UNDO.
DEF VAR fLevAnt      AS DEC    NO-UNDO.
DEF VAR plFaktura_Id AS DEC    NO-UNDO.
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
DEFINE VARIABLE bVarespes AS LOG NO-UNDO.
DEFINE VARIABLE cTekst    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lTotAnt   AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99"NO-UNDO.

DEF BUFFER bPkSdlLinje FOR PkSdlLinje.
DEFINE BUFFER bufPkSdlHode FOR PkSdlHode.

/* Utvidet varespes på fakturalinje. */
{syspara.i 19 100 4 cTekst}
IF CAN-DO('1,Ja,J,Yes,Y,True',cTekst) 
  THEN bVarespes = TRUE.
  ELSE bVarespes = FALSE.

lPkSdlId = DEC(ENTRY(1,icParam,";")).

FIND PkSdlHode NO-LOCK
     WHERE PkSdlHode.PkSdlId = lPkSdlId
     NO-ERROR.

/* Kundeordre fra nettbutikk skal ikke overføres faktura. de er allerede betalt. */
/*
IF AVAILABLE PkSdlHode AND PkSdlHode.Opphav = 10 THEN
  DO:
    ASSIGN
      obOk     = TRUE
      ocReturn = ''.
    RETURN.
  END.
*/  

FIND Kunde WHERE Kunde.KundeNr = PkSdlHode.KundeNr NO-LOCK NO-ERROR.
IF NOT AVAIL Kunde THEN DO:
  ocReturn = "Finner ikke kunde for kundeordre: " + STRING(PkSdlHode.kundenr).
  RETURN.
END.

IF AVAIL PkSdlHode THEN 
DO ON ERROR UNDO, LEAVE TRANSACTION:
  /* Oppretter faktura, setter kundenr m.m. og returnerer faktura_id. */
  RUN getfaktura_id.p (Kunde.KundeNr,PkSdlHode.ButikkNr,1,YES,TODAY,OUTPUT plFaktura_Id).
  IF RETURN-VALUE <> "AVBRYT" AND CAN-FIND(FakturaHode WHERE FakturaHode.Faktura_Id = plFaktura_Id) THEN
  FAKTURAINFO:
  DO:
    FIND FakturaHode NO-LOCK WHERE
        FakturaHode.Faktura_Id = plFaktura_Id.
    /* Initierer faktura med kundeinfo m.m. */
    IF FakturaHode.Navn = "" AND FakturaHode.Adresse1 = "" THEN DO:
      RUN update_fakturahode.p (plfaktura_Id,"INIT","",1).
      RUN update_fakturahode.p (plfaktura_Id,"Butikksalg,TotalRabatt%,Leveringsdato,LevFNr,Leveringsdato,Utsendelsesdato,Referanse",
                                "yes" + chr(1) + 
                                 STRING(Kunde.TotalRabatt%) + CHR(1) + 
                                 STRING(TODAY) + CHR(1) + 
                                 "1" + CHR(1) + 
                                 STRING(TODAY) + CHR(1) + 
                                 STRING(TODAY) + CHR(1) + 
                                 PkSdlHode.Referanse,1) .
      FIND CURRENT FakturaHode NO-LOCK.
    END.
    ELSE 
        RUN update_fakturahode.p (plfaktura_Id,"Referanse",
                              PkSdlHode.Referanse,1) .
  END. /* FAKTURAINFO */
  ELSE DO:
    ocReturn = "Kunne ikke opprette faktura".
    UNDO, LEAVE.
  END.

  /* Oppdaterer fakturahode med informasjon */
  FAKTURAHODE:
  DO:
      FIND CURRENT FakturaHode EXCLUSIVE-LOCK.
      ASSIGN
          FakturaHode.VaarRef   = IF PkSdlHode.VaarRef <> "" THEN PkSdlHode.VaarRef ELSE FakturaHode.VaarRef
          FakturaHode.DeresRef  = IF PkSdlHode.DeresRef <> "" THEN PkSdlHode.DeresRef ELSE FakturaHode.DeresRef
          FakturaHode.Referanse = IF PkSdlHode.Referanse <> "" THEN PkSdlHode.Referanse ELSE FakturaHode.Referanse
          .
      FIND CURRENT FakturaHode NO-LOCK.
  END. /* FAKTURAHODE */

  /* Henter butikken */
  FIND Butiker NO-LOCK WHERE
      Butiker.Butik = PkSdlHode.ButikkNr NO-ERROR.

  /* Klargjør bonghode. */
  RUN OpprettFil.
  RUN OpprettDatasett.
  RUN OpprettBongHode.
  FIND BongHode NO-LOCK WHERE
      BongHode.B_Id = plB_Id NO-ERROR.

  /* Finner neste linjenr på faktura. */
  piLinjeNr = 0.
  FOR EACH FakturaLinje NO-LOCK OF FakturaHode
      BY FakturaLinje.Faktura_Id
      BY FakturaLinje.FakturaLinjeNr:
      piLinjeNr = FakturaLinje.FakturaLinjeNr.
  END.
  piLinjeNr = piLinjeNr + 1. /* Neste ledige linjenr. */

  /* Fakturering av ordrelinjene */
  ocReturn = "".
  lTotAnt = 0.
  LESPkSdlLinje:
  FOR EACH PkSdlLinje OF PkSdlHode EXCLUSIVE-LOCK WHERE
      PkSdlLinje.Leveringsdato <> ? AND
      PkSdlLinje.Faktura_Id = 0:

      IF AVAILABLE ArtBas THEN RELEASE ArtBas.
      ASSIGN
          plArtikkelNr = DEC(PkSdlLinje.VareNr) NO-ERROR.
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
          ocReturn = "Fakturer"
          .
      CREATE FakturaLinje.
      ASSIGN
          piBongLinje                 = piBongLinje + 1
          FakturaLinje.Faktura_Id     = plfaktura_Id
          FakturaLinje.FakturaLinjeNr = piLinjeNr
          piLinjeNr                   = piLinjeNr + 1
          FakturaLinje.Opphav         = 1 /* Artikkel */
          FakturaLinje.Leveringsdato  = TODAY /*PkSdlLinje.Leveringsdato*/
          FakturaLinje.TTId           = 65
          FakturaLinje.TBId           = 1
          FakturaLinje.ArtikkelNr    = IF AVAILABLE ArtBas
                                         THEN ArtBas.ArtikkelNr
                                         ELSE 0
          Fakturalinje.LevFargKod    = IF AVAILABLE ArtBas
                                         THEN ArtBas.LevFargKod
                                         ELSE ""
          FakturaLinje.Notat          = "Ordre nr/linje: "  + STRING(PkSdlLinje.PkSdlId) +
                                        "/"         + STRING(PkSdlLinje.PkSdlLinjeNr)
          FakturaLinje.EkstRefId      = IF PkSdlLinje.RefNr <> 0 
                                             THEN STRING(PkSdlLinje.RefNr)
                                             ELSE ""
          FakturaLinje.EkstRefTekst   = PkSdlLinje.RefTekst

          FakturaLinje.Antall         = PkSdlLinje.Antall
          FakturaLinje.VareNr         = PkSdlLinje.VareNr
          FakturaLinje.Varetekst      = PkSdlLinje.Varetekst
          FakturaLinje.ArbeidsBeskr   = PkSdlLinje.ArbeidsBeskr 
          FakturaLinje.Varespesifikasjon = PkSdlLinje.Varespesifikasjon
                                        /* Linjerabatt og Kunderabatt slå sammen til linjerabatt på faktura */
          FakturaLinje.LinjeRabattKr  = IF (PkSdlLinje.LinjeRabattKr > PkSdlLinje.KundeRabattKr)
                                                 THEN (PkSdlLinje.LinjeRabattKr + PkSdlLinje.OrdreRabattKr)
                                                 ELSE (PkSdlLinje.KundeRabattKr + PkSdlLinje.OrdreRabattKr)
                                        /* Rabatt% beregnes før uttrekk av mva */
          FakturaLinje.LinjeRab%      = IF (PkSdlLinje.LinjeRabattKr > PkSdlLinje.KundeRabattKr)
                                                 THEN PkSdlLinje.LinjeRab% /*(((PkSdlLinje.LinjeRabattKr + PkSdlLinje.OrdreRabattKr) / PkSdlLinje.Antall) / PkSdlLinje.NettoPris) * 100*/
                                                 ELSE PkSdlLinje.KundeRab% /*(((PkSdlLinje.KundeRabattKr + PkSdlLinje.OrdreRabattKr) / PkSdlLinje.Antall) / PkSdlLinje.NettoPris) * 100*/
          /*
          FakturaLinje.LinjeRab%      = IF FakturaLinje.LinjeRab% = ? 
                                          THEN 0
                                          ELSE FakturaLinje.LinjeRab%                                          
          */
          FakturaLinje.TotalrabattKr  = FakturaLinje.LinjeRabattK
          FakturaLinje.TotRab%        = FakturaLinje.LinjeRab%
          FakturaLinje.NettoPris      = PkSdlLinje.NettoPris - (PkSdlLinje.MvaKr / PkSdlLinje.Antall)

          FakturaLinje.MvaKr          = PkSdlLinje.MvaKr
          FakturaLinje.NettoLinjeSum  = PkSdlLinje.NettoLinjeSum - PkSdlLinje.MvaKr
          FakturaLinje.LinjeSum       = PkSdlLinje.NettoLinjeSum

          FakturaLinje.Mva%           = PkSdlLinje.Mva%
          FakturaLinje.MomsKod        = PkSdlLinje.MomsKod
          FakturaLinje.Leveringsdato  = TODAY /*PkSdlLinje.Leveringsdato*/
          FakturaLinje.Storl          = PkSdlLinje.Storl
          FakturaLinje.DbKr           = PkSdlLinje.DbKr
          FakturaLinje.Db%            = PkSdlLinje.Db%
          FakturaLinje.Pris           = PkSdlLinje.Pris * (1 / (1  + (PkSdlLinje.Mva% / 100)))
          /* Logger fakturert kundeordrelinjer */
          PkSdlLinje.Faktura_Id      = plFaktura_Id
          .
            
      IF bVarespes THEN 
        DO:
          IF AVAILABLE ArtBas THEN 
            FIND Farg OF ArtBas NO-LOCK NO-ERROR.
          IF AVAILABLE Farg THEN 
            Fakturalinje.Varespesifikasjon = STRING(Farg.Farg) + ' ' + Farg.FarBeskr + 
                                            (IF Fakturalinje.Varespesifikasjon <> ''
                                                THEN CHR(10) + Fakturalinje.Varespesifikasjon
                                                ELSE '').
        END.

      /* Oppretter bonglinje */
      CREATE_BONGLINJE:
      DO:
          FIND BongLinje EXCLUSIVE-LOCK WHERE
               BongLinje.ButikkNr = BongHode.ButikkNr AND
               BongLinje.GruppeNr = BongHode.GruppeNr AND
               BongLinje.KasseNr  = BongHode.KasseNr  AND
            BongLinje.Dato     = TODAY /*PkSdlLinje.Leveringsdato*/ AND
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
                BongLinje.Dato         = TODAY /*PkSdlLinje.Leveringsdato*/    
                BongLinje.TransDato    = TODAY /*PkSdlLinje.Leveringsdato*/
                BongLinje.TransTid     = BongHode.Tid
                BongLinje.BongNr       = BongHode.BongNr   
                BongLinje.LinjeNr      = piBongLinje /*BongLinje*/
                .
            IF AVAILABLE FakturaLinje THEN
              DO:
                ASSIGN
                  FakturaLinje.B_Id        = BongLinje.B_Id 
                  FakturaLinje.BongLinjeNr = BongLinje.LinjeNr
                  .
              END.
          END.
      END. /* CREATE_BONGLINJE */
      
      /* Overstyring for Betalingslinjer */
      IF CAN-DO('KREDIT,BETALT,BETALING',PkSdlLinje.VareNr) THEN 
      DO:
        FIND TransType NO-LOCK WHERE
            TransType.TTId = 50 NO-ERROR.
        ASSIGN
            FakturaLinje.TTId          = 50
            FakturaLinje.TBId          = 1
            FakturaLinje.Antall        = PkSdlLinje.Antall
            FakturaLinje.Varetekst     = PkSdlLinje.Varetekst
            FakturaLinje.NettoPris     = PkSdlLinje.Linjesum
            FakturaLinje.NettoLinjeSum = PkSdlLinje.Linjesum 
            FakturaLinje.Linjesum      = PkSdlLinje.Linjesum 
            /* Felt som skal nulles ut */
            FakturaLinje.LinjeRabattKr  = 0
            FakturaLinje.LinjeRab%      = 0
            FakturaLinje.TotalrabattKr  = 0
            FakturaLinje.TotRab%        = 0
            FakturaLinje.MvaKr          = 0
            FakturaLinje.Mva%           = 0
            FakturaLinje.MomsKod        = 0
            FakturaLinje.DbKr           = 0
            FakturaLinje.Db%            = 0
            FakturaLinje.Pris           = 0
            .

        /* Betalingslinjer med 0 kr, tas ikke med inn på faktura. */
        /* Betalingslinjen for kreditpost inneholder 0 kr.        */
        /* Denne posteres ved sluttføring av bongen.              */
        IF FakturaLinje.LinjeSum = 0 THEN 
        DO:
            DELETE FakturaLinje.
            /* Posterer bonglinjerecord for betaling */
            ASSIGN
                BongLinje.TTId       = IF CAN-DO('KREDIT',PkSdlLinje.VareNr) THEN 65 ELSE 50 
                BongLinje.TBId       = 1
                BongLinje.BongTekst  = PkSdlLinje.Varetekst
                BongLinje.Antall     = 0
                BongLinje.LinjeSum   = plLinjeSum 
                BongLinje.BongPris   = plLinjeSum 
                .
            RELEASE BongLinje.           
            NEXT LESPkSdlLinje.
        END.  
        ELSE DO:
            /* Posterer bonglinjerecord for betaling */
            ASSIGN
                BongLinje.TTId       = IF CAN-DO('KREDIT',PkSdlLinje.VareNr) THEN 65 ELSE 50 
                BongLinje.TBId       = 1
                BongLinje.BongTekst  = PkSdlLinje.Varetekst
                BongLinje.Antall     = 0
                BongLinje.LinjeSum   = plLinjeSum 
                BongLinje.BongPris   = plLinjeSum 
                .
            RELEASE BongLinje.           
            /* Betaling ferdig */
        END.                 
      END.
      /* Varelinjer */
      ELSE IF AVAILABLE ArtBas THEN 
      BONGLINJE:
      DO:
          /* Hvis sum antall < 0, legges betalingslinjer opp med negativt beløp. */
          lTotAnt = lTotant + PkSdlLinje.Antall.
 
          /* Henter lager og varekost for butikken */
          FIND Lager EXCLUSIVE-LOCK WHERE
            Lager.ArtikkelNr = FakturaLinje.ArtikkelNr AND
            Lager.Butik      = PkSdlHode.ButikkNr NO-ERROR NO-WAIT.
          IF NOT AVAILABLE Lager THEN
            DO:
              CREATE Lager.
              ASSIGN
                  Lager.ArtikkelNr = FakturaLinje.ArtikkelNr
                  Lager.Butik      = PkSdlHode.ButikkNr
                  Lager.VVareKost  = IF AVAILABLE ArtPris
                                       THEN ArtPris.Varekost[1]
                                       ELSE 0
                  .
            END.
          IF Lager.VVareKost = 0 THEN
              Lager.VVareKost  = IF AVAILABLE ArtPris
                                   THEN ArtPris.Varekost[1]
                                   ELSE 0.

          FIND VarGR NO-LOCK OF ArtBas NO-ERROR.

          ASSIGN
            BongLinje.TTId       = IF PkSdlLinje.Antall > 0 
                                     THEN 1  /* Kontant */
                                     ELSE 10 /* Retur   */
            BongLinje.ArtikkelNr = STRING(ArtBas.ArtikkelNr)
            BongLinje.Strekkode  = ""
            BongLinje.VareGr     = ArtBas.Vg
            BongLinje.LopeNr     = ArtBas.LopNr
            BongLinje.Storrelse  = PkSdlLinje.Storl
            BongLinje.BongTekst  = PkSdlLinje.Varetekst
            BongLinje.Antall     = PkSdlLinje.Antall
            BongLinje.LinjeSum   = abs(PkSdlLinje.NettoLinjeSum) + abs(FakturaLinje.LinjeRabattKr)
            BongLinje.BongPris   = abs(BongLinje.LinjeSum)
            BongLinje.VVarekost  = abs(Lager.VVarekost) * abs(BongLinje.Antall)
            BongLinje.LinjeRab   = abs(FakturaLinje.LinjeRabattKr)
            BongLinje.VareGruppeNavn = IF AVAILABLE VarGr
                                     THEN VarGr.VgBeskr
                                     ELSE ""

            BongLinje.Mva%       = PkSdlLinje.Mva%
            BongLinje.MvaKr      = abs(PkSdlLinje.MvaKr)
            BongLinje.FeilKode   = 0
            BongLinje.NotatKode  = 0
            BongLinje.RefNr      = int(FakturaLinje.EkstRefId)
            BongLinje.RefTekst   = FakturaLinje.EkstRefTekst
            .

          ASSIGN
            plLinjeSum = plLinjeSum + BongLinje.LinjeSum - FakturaLinje.LinjeRabattKr
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
  END. /* LESPkSdlLinje */

  /* Vellyket fakturering */
  IF ocReturn = "Fakturer" THEN 
      DO: 
        FIND CURRENT PkSdlHode EXCLUSIVE-LOCK.
        RUN update_fakturahode.p (plfaktura_Id,"KalkulerTotaler","",5).
        RUN beregn_kunde_saldo.p ("idlist|" + STRING(FakturaHode.KundeNr),
                                ?,
                                "",
                                OUTPUT ocReturn,
                                OUTPUT obOk).

        ASSIGN 
            ocReturn = ""
            PkSdlHode.FakturertDato = TODAY /*(IF PkSdlHode.FakturertDato = ? 
                                             THEN TODAY
                                             ELSE PkSdlHode.FakturertDato) */
            PkSdlHode.FakturertTid  = TIME 
            PkSdlHode.LevStatus = (IF CAN-FIND(FIRST PkSdlLinje OF PkSdlHode WHERE
                                                PkSdlLinje.Leveringsdato = ?)
                                      THEN "40"
                                      ELSE  "50") /* Fakturert */
            .
        /* Legger opp betalingstransaksjon på bong */
        RUN ferdigBong.
        FIND CURRENT PkSdlHode NO-LOCK.
      END.
  ELSE ocReturn = "Ingen varelinje for kundeordre tilgjengelig for fakturering".
END.
ELSE ocReturn = "Kundeordre ikke tilgjengelig for fakturering".

obOk = ocReturn = "".

PROCEDURE OpprettFil:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* Oppretter posten i filen. */
  IF NOT CAN-FIND(Filer WHERE
                  Filer.FilNavn   = "Salg fra kundeordre " + STRING(PkSdlHode.PkSdlId) AND
    Filer.Dato      = TODAY /* PkSdlHode.Dato*/ AND
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
      Filer.FilNavn     = "Salg fra kunderodre " + STRING(PkSdlHode.PkSdlId) 
      Filer.Dato        = TODAY /*PkSdlHode.Dato*/
      Filer.Kl          = STRING(TIME,"HH:MM:SS") 
      Filer.Storrelse   = 0 
      Filer.Katalog     = "Kundeordre"
      Filer.AntLinjer   = 0
      Filer.FilType     = 1 
      Filer.Innlest     = TRUE
      Filer.InnlestDato = TODAY /*PkSdlHode.Dato*/
      Filer.InnlestKl   = TIME
      Filer.Oppdatert   = TRUE
      Filer.OppdatertDato = TODAY /*PkSdlHode.Dato*/
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

      FIND FIRST PkSdlLinje OF PkSdlHode NO-LOCK NO-ERROR.
      IF AVAILABLE PkSdlLinje THEN
          pdKDato = TODAY. /*PkSdlLinje.Leveringsdato.*/
      ELSE 
          pdKDato = ?.

      /* Finner neste SettNr */
      FIND LAST Datasett NO-LOCK WHERE
          Datasett.ButikkNr = PkSdlHode.ButikkNr AND
        Datasett.GruppeNr = 1 /*PkSdlHode.GruppeNr */ AND
          Datasett.KasseNr  = PkSdlHode.KasseNr  AND
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
        DataSett.ButikkNr   = PkSdlHode.ButikkNr 
        DataSett.GruppeNr   = 1 /*PkSdlHode.GruppeNr */
        DataSett.KasseNr    = PkSdlHode.KasseNr
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
            BongHode.ButikkNr = PkSdlHode.ButikkNr AND
            BongHode.GruppeNr = 1 AND
            BongHode.KasseNr  = PkSdlHode.KasseNr  AND
            BongHode.Dato     = TODAY /*PkSdlHode.Dato*/ /*  AND
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
            FIND Forsalj OF PkSdlHode NO-ERROR.
        FIND Kunde OF PkSdlHode NO-LOCK NO-ERROR.
        IF AVAILABLE Kunde THEN 
          FIND FIRST KundeKort OF Kunde NO-LOCK NO-ERROR.
          
        FIND FIRST Medlemskort NO-LOCK WHERE 
            Medlemskort.InterntKKortId = kundekort.interntkkortid NO-ERROR.
        IF AVAILABLE MedlemsKort THEN 
          FIND Medlem OF MedlemsKort NO-LOCK NO-ERROR.
          
        CREATE BongHode.
        ASSIGN
          piBongLinje            = 0
          BongHode.ButikkNr      = PkSdlHode.ButikkNr 
          BongHode.GruppeNr      = 1 
          BongHode.KasseNr       = PkSdlHode.KasseNr  
          BongHode.Dato          = TODAY /*PkSdlHode.Dato*/
          BongHode.Tid           = TIME
          BongHode.BongNr        = piBongNr
          BongHode.BongStatus    = 0 /* Under klargjøring */
          BongHode.OpdKvit       = TRUE
          Bonghode.DataSettId    = DataSett.DataSettId
          BongHode.Utskriftskopi = "Utskriftskopi ikke mottat for kvittering " + 
                                   STRING(piBongNr) + "."
          BongHode.KassererNr    = PkSdlHode.ForsNr
          BongHode.KassererNavn  = IF AVAILABLE Forsalj
                                     THEN Forsalj.FoNamn
                                     ELSE "* Ukjent kasserer *"
          BongHode.PkSdlId     = PkSdlHode.PkSdlId
          BongHode.Konvertert    = TRUE
          BongHode.SelgerNr      = PkSdlHode.SelgerNr
          BongHode.KundeNr       = PkSdlHode.KundeNr
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
        FIND bufPkSdlHode EXCLUSIVE-LOCK WHERE 
            RECID(bufPkSdlHode) = RECID(PkSdlHode) NO-ERROR.
        IF AVAILABLE bufPkSdlHode THEN 
        DO:
            ASSIGN 
                bufPkSdlHode.Embalage = STRING(BongHode.B_Id)
                .
            RELEASE bufPkSdlHode. 
        END. 
            
    END. /* BONGHODE */
END PROCEDURE.

PROCEDURE ferdigBong:
    DEF VAR pBongDato AS DATE NO-UNDO.

    DO TRANSACTION:
        FIND CURRENT BongHode EXCLUSIVE-LOCK.
        BETALING:
        DO:
            FIND FIRST BongLinje NO-LOCK WHERE
                BongLinje.B_Id = BongHode.B_Id NO-ERROR.
            IF AVAILABLE BongLinje THEN
                pBongDato = BongLinje.Dato.
            ELSE
                pBongDato = ?.
                
            CREATE BongLinje. /* */
            ASSIGN
                BongLinje.B_Id         = BongHode.B_Id
                BongLinje.ButikkNr     = BongHode.ButikkNr 
                BongLinje.GruppeNr     = BongHode.GruppeNr 
                BongLinje.KasseNr      = BongHode.KasseNr  
                BongLinje.Dato         = TODAY /*pBongDato*/     
                BongLinje.BongNr       = BongHode.BongNr   
                BongLinje.TTId         = 65 /* Kredit */
                BongLinje.TBId         = 1
                BongLinje.LinjeNr      = piBongLinje + 1 /*BongLinje*/
                BongLinje.TransDato    = TODAY /*BongHode.Dato*/
                BongLinje.TransTid     = BongHode.Tid
                .


            ASSIGN
                BongLinje.BongTekst  = "KREDIT"
                BongLinje.Antall     = 0
                BongLinje.LinjeSum   = plLinjeSum * (IF lTotant < 0 THEN -1 ELSE 1)
                BongLinje.BongPris   = plLinjeSum * (IF lTotant < 0 THEN -1 ELSE 1)
                .
            RELEASE BongLinje.
            
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
