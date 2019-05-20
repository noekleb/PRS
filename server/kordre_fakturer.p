/* Levering av kundeordre
   Parametere: <KOrdre_id>;<evt liste over linjenr|antall>
   
   Opprettet: 30.06.06 av BHa                  
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR ix           AS INT    NO-UNDO.
DEF VAR iLnr         AS INT    NO-UNDO.
DEF VAR cLevVareList AS CHAR   NO-UNDO.
DEF VAR fKOrdre_id   AS DEC    NO-UNDO.
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
DEFINE VARIABLE bRetur AS LOG NO-UNDO.

DEFINE VARIABLE rKundeordreBehandling AS cls.Kundeordre.KundeordreBehandling NO-UNDO.
rKundeordreBehandling  = NEW cls.Kundeordre.KundeordreBehandling( ) NO-ERROR.

DEF BUFFER bKOrdreLinje FOR KOrdreLinje.
DEFINE BUFFER bufKOrdreHode FOR KOrdreHode.

/* Utvidet varespes på fakturalinje. */
{syspara.i 19 100 4 cTekst}
IF CAN-DO('1,Ja,J,Yes,Y,True',cTekst) 
  THEN bVarespes = TRUE.
  ELSE bVarespes = FALSE.

fKOrdre_id = DEC(ENTRY(1,icParam,";")).

ASSIGN 
    bRetur = FALSE
    .

FIND KOrdreHode NO-LOCK
     WHERE KOrdreHode.KOrdre_id = fKOrdre_id
     NO-ERROR.

/* Kundeordre fra nettbutikk skal ikke overføres faktura. de er allerede betalt. */
/*
IF AVAILABLE KORdreHode AND KORdreHode.Opphav = 10 THEN
  DO:
    ASSIGN
      obOk     = TRUE
      ocReturn = ''.
    RETURN.
  END.
*/  

FIND Kunde WHERE Kunde.KundeNr = KOrdreHode.KundeNr NO-LOCK NO-ERROR.
IF NOT AVAIL Kunde THEN DO:
  ocReturn = "Finner ikke kunde for kundeordre: " + STRING(KOrdreHode.kundenr).
  RETURN.
END.

IF AVAIL KOrdreHode THEN 
DO ON ERROR UNDO, LEAVE TRANSACTION:
  /* Oppretter faktura, setter kundenr m.m. og returnerer faktura_id. */
  RUN getfaktura_id.p (Kunde.KundeNr,KOrdreHode.ButikkNr,1,YES,TODAY,OUTPUT plFaktura_Id).
  IF RETURN-VALUE <> "AVBRYT" AND CAN-FIND(FakturaHode WHERE FakturaHode.Faktura_Id = plFaktura_Id) THEN
  FAKTURAINFO:
  DO:
    FIND FakturaHode NO-LOCK WHERE
        FakturaHode.Faktura_Id = plFaktura_Id.
    /* Initierer faktura med kundeinfo m.m. */
    IF FakturaHode.Navn = "" AND FakturaHode.Adresse1 = "" THEN DO:
      RUN update_fakturahode.p (plfaktura_Id,"INIT","",1).
      RUN update_fakturahode.p (plfaktura_Id,"Butikksalg,TotalRabatt%,Leveringsdato,LevFNr,Leveringsdato,Utsendelsesdato,Referanse,KOrdre_ID,Bilagstype",
                                "yes" + chr(1) + 
                                 STRING(Kunde.TotalRabatt%) + CHR(1) + 
                                 STRING(TODAY) + CHR(1) + 
                                 "1" + CHR(1) + 
                                 STRING(TODAY) + CHR(1) + 
                                 STRING(TODAY) + CHR(1) + 
                                 KOrdreHode.Referanse  + CHR(1) +
                                 STRING(KOrdreHode.KOrdre_Id) + CHR(1) +
                                 (IF KOrdreHode.SendingsNr = 'RETUR' THEN '2' ELSE '1'),
                                 1) .
      FIND CURRENT FakturaHode NO-LOCK.
    END.
    ELSE 
        RUN update_fakturahode.p (plfaktura_Id,"Referanse",
                              KOrdreHode.Referanse,1) .
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
          FakturaHode.VaarRef   = IF KOrdreHode.VaarRef <> "" THEN KOrdreHode.VaarRef ELSE FakturaHode.VaarRef
          FakturaHode.DeresRef  = IF KOrdreHode.DeresRef <> "" THEN KOrdreHode.DeresRef ELSE FakturaHode.DeresRef
          FakturaHode.Referanse = IF KOrdreHode.Referanse <> "" THEN KOrdreHode.Referanse ELSE FakturaHode.Referanse
          .
      FIND CURRENT FakturaHode NO-LOCK.
  END. /* FAKTURAHODE */

  /* Henter butikken */
  FIND Butiker NO-LOCK WHERE
      Butiker.Butik = KOrdreHode.ButikkNr NO-ERROR.

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
  LESKORDRELINJE:
  FOR EACH KOrdreLinje OF KOrdreHode EXCLUSIVE-LOCK WHERE
      KOrdreLinje.Leveringsdato <> ? AND
      KOrdreLinje.Faktura_Id = 0 AND 
      KOrdreLinje.Aktiv = TRUE:

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
          ocReturn = "Fakturer"
          .
      CREATE FakturaLinje.
      ASSIGN
          piBongLinje                 = piBongLinje + 1
          FakturaLinje.Faktura_Id     = plfaktura_Id
          FakturaLinje.FakturaLinjeNr = piLinjeNr
          piLinjeNr                   = piLinjeNr + 1
          FakturaLinje.Opphav         = 1 /* Artikkel */
          FakturaLinje.Leveringsdato  = TODAY /*KOrdreLinje.Leveringsdato*/
          FakturaLinje.TTId           = 65
          FakturaLinje.TBId           = 1
          FakturaLinje.ArtikkelNr    = IF AVAILABLE ArtBas
                                         THEN ArtBas.ArtikkelNr
                                         ELSE 0
          Fakturalinje.LevFargKod    = IF AVAILABLE ArtBas
                                         THEN ArtBas.LevFargKod
                                         ELSE ""
          FakturaLinje.Notat          = "Ordre nr/linje: "  + STRING(KOrdreLinje.KOrdre_Id) +
                                        "/"         + STRING(KOrdreLinje.KordreLinjeNr)
          FakturaLinje.EkstRefId      = IF KOrdreLinje.RefNr <> 0 
                                             THEN STRING(KOrdreLinje.RefNr)
                                             ELSE ""
          FakturaLinje.EkstRefTekst   = KOrdreLinje.RefTekst

          FakturaLinje.Antall         = KOrdreLinje.Antall
          FakturaLinje.VareNr         = KOrdreLinje.VareNr
          FakturaLinje.Varetekst      = KOrdreLinje.Varetekst
          FakturaLinje.ArbeidsBeskr   = KOrdreLinje.ArbeidsBeskr 
          FakturaLinje.Varespesifikasjon = KOrdreLinje.Varespesifikasjon
                                        /* Linjerabatt og Kunderabatt slå sammen til linjerabatt på faktura */
          FakturaLinje.LinjeRabattKr  = IF (KOrdreLinje.LinjeRabattKr > KOrdreLinje.KundeRabattKr)
                                                 THEN (KOrdreLinje.LinjeRabattKr + KOrdreLinje.OrdreRabattKr)
                                                 ELSE (KOrdreLinje.KundeRabattKr + KOrdreLinje.OrdreRabattKr)
                                        /* Rabatt% beregnes før uttrekk av mva */
          FakturaLinje.LinjeRab%      = IF (KOrdreLinje.LinjeRabattKr > KOrdreLinje.KundeRabattKr)
                                                 THEN KOrdreLinje.LinjeRab% /*(((KOrdreLinje.LinjeRabattKr + KOrdreLinje.OrdreRabattKr) / KOrdreLinje.Antall) / KOrdreLinje.NettoPris) * 100*/
                                                 ELSE KOrdreLinje.KundeRab% /*(((KOrdreLinje.KundeRabattKr + KOrdreLinje.OrdreRabattKr) / KOrdreLinje.Antall) / KOrdreLinje.NettoPris) * 100*/
          FakturaLinje.LinjeRab%      = IF FakturaLinje.LinjeRab% = ? 
                                          THEN 0
                                          ELSE FakturaLinje.LinjeRab%                                          
          FakturaLinje.TotalrabattKr  = FakturaLinje.LinjeRabattK
          FakturaLinje.TotRab%        = FakturaLinje.LinjeRab%
          FakturaLinje.NettoPris      = KOrdreLinje.NettoPris - (KOrdreLinje.MvaKr / KOrdreLinje.Antall)
          FakturaLinje.NettoPris      = (IF FakturaLinje.NettoPris = ? THEN 0 ELSE FakturaLinje.NettoPris)
          
          FakturaLinje.MvaKr          = KOrdreLinje.MvaKr
          FakturaLinje.MvaKr          = (IF FakturaLinje.MvaKr = ? THEN 0 ELSE FakturaLinje.MvaKr)

          FakturaLinje.NettoLinjeSum  = KOrdreLinje.NettoLinjeSum - KOrdreLinje.MvaKr
          FakturaLinje.NettoLinjeSum  = (IF FakturaLinje.NettoLinjeSum = ? THEN 0 ELSE FakturaLinje.NettoLinjeSum)

          FakturaLinje.LinjeSum       = KOrdreLinje.NettoLinjeSum
          FakturaLinje.LinjeSum       = (IF FakturaLinje.LinjeSum = ? THEN 0 ELSE FakturaLinje.LinjeSum)

          FakturaLinje.Mva%           = KOrdreLinje.Mva%
          FakturaLinje.MomsKod        = KOrdreLinje.MomsKod
          FakturaLinje.Leveringsdato  = TODAY /*KOrdreLinje.Leveringsdato*/
          FakturaLinje.Storl          = KOrdreLinje.Storl
          FakturaLinje.DbKr           = KOrdreLinje.DbKr
          FakturaLinje.DbKr           = (IF FakturaLinje.DbKr = ? THEN 0 ELSE FakturaLinje.DbKr)
          FakturaLinje.Db%            = KOrdreLinje.Db%
          FakturaLinje.Pris           = KOrdreLinje.Pris * (1 / (1  + (KordreLinje.Mva% / 100)))
          FakturaLinje.Pris           = (IF FakturaLinje.Pris = ? THEN 0 ELSE FakturaLinje.Pris)
          /* Logger fakturert kundeordrelinjer */
          KOrdreLinje.Faktura_Id      = plFaktura_Id
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
            IF AVAILABLE FakturaLinje THEN
              DO:
                ASSIGN
                  FakturaLinje.B_Id        = BongLinje.B_Id 
                  FakturaLinje.BongLinjeNr = BongLinje.LinjeNr
                  .
              END.
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
            Lager.ArtikkelNr = FakturaLinje.ArtikkelNr AND
            Lager.Butik      = KOrdreHode.ButikkNr NO-ERROR NO-WAIT.
          IF NOT AVAILABLE Lager THEN
            DO:
              CREATE Lager.
              ASSIGN
                  Lager.ArtikkelNr = FakturaLinje.ArtikkelNr
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
            BongLinje.TTId       = IF KOrdreLinje.Antall > 0 
                                     THEN 1  /* Kontant */
                                     ELSE 10 /* Retur   */
            BongLinje.ArtikkelNr = STRING(ArtBas.ArtikkelNr)
            BongLinje.Strekkode  = ""
            BongLinje.VareGr     = ArtBas.Vg
            BongLinje.LopeNr     = ArtBas.LopNr
            BongLinje.Storrelse  = KOrdreLinje.Storl
            BongLinje.BongTekst  = KOrdreLinje.Varetekst
            BongLinje.Antall     = KOrdreLinje.Antall
            BongLinje.LinjeSum   = abs(KOrdreLinje.NettoLinjeSum) + abs(FakturaLinje.LinjeRabattKr)
            BongLinje.BongPris   = abs(BongLinje.LinjeSum)
            BongLinje.VVarekost  = abs(Lager.VVarekost) * abs(BongLinje.Antall)
            BongLinje.LinjeRab   = abs(FakturaLinje.LinjeRabattKr)
            BongLinje.VareGruppeNavn = IF AVAILABLE VarGr
                                     THEN VarGr.VgBeskr
                                     ELSE ""

            BongLinje.Mva%       = KOrdreLinje.Mva%
            BongLinje.MvaKr      = abs(KOrdreLinje.MvaKr)
            BongLinje.FeilKode   = 0
            BongLinje.NotatKode  = 0
            BongLinje.RefNr      = int(FakturaLinje.EkstRefId)
            BongLinje.RefTekst   = FakturaLinje.EkstRefTekst
            .

          ASSIGN
            bRetur     = IF bRetur = FALSE THEN (IF BongLinje.TTId = 10 THEN TRUE ELSE FALSE) ELSE bRetur
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
      
      /* Betalingslinjer  */
      ELSE IF CAN-DO('KREDIT,BETALT,BETALING',KORdreLinje.VareNr) THEN 
      DO:
        IF CAN-DO('unknown,klarna,mastercard,visa,amex,diners',KORdreLinje.Varetekst) THEN 
        DO:
            CASE KOrdreLinje.Varetekst:
                WHEN 'klarna' THEN 
                DO:
                    FIND SIETransType NO-LOCK WHERE 
                        SIETransType.ButikkNr = BongHode.ButikkNr AND SIETransType.TTId = 52 AND SIETransType.TBId = 901 NO-ERROR.
                    IF NOT AVAILABLE SIETransType THEN 
                    FIND SIETransType NO-LOCK WHERE 
                        SIETransType.ButikkNr = 0 AND SIETransType.TTId = 52 AND SIETransType.TBId = 901 NO-ERROR.
                END.
                WHEN 'visa' THEN 
                DO:
                    FIND SIETransType NO-LOCK WHERE 
                        SIETransType.ButikkNr = BongHode.ButikkNr AND SIETransType.TTId = 52 AND SIETransType.TBId = 3 NO-ERROR.
                    IF NOT AVAILABLE SIETransType THEN 
                    FIND SIETransType NO-LOCK WHERE 
                        SIETransType.ButikkNr = 0 AND SIETransType.TTId = 52 AND SIETransType.TBId = 3 NO-ERROR.
                END.
                WHEN 'mastercard' THEN 
                DO:
                    FIND SIETransType NO-LOCK WHERE 
                        SIETransType.ButikkNr = BongHode.ButikkNr AND SIETransType.TTId = 52 AND SIETransType.TBId = 4 NO-ERROR.
                    IF NOT AVAILABLE SIETransType THEN 
                    FIND SIETransType NO-LOCK WHERE 
                        SIETransType.ButikkNr = 0 AND SIETransType.TTId = 52 AND SIETransType.TBId = 4 NO-ERROR.
                END.
                WHEN 'amex' THEN 
                DO:
                    FIND SIETransType NO-LOCK WHERE 
                        SIETransType.ButikkNr = BongHode.ButikkNr AND SIETransType.TTId = 52 AND SIETransType.TBId = 5 NO-ERROR.
                    IF NOT AVAILABLE SIETransType THEN 
                    FIND SIETransType NO-LOCK WHERE 
                        SIETransType.ButikkNr = 0 AND SIETransType.TTId = 52 AND SIETransType.TBId = 5 NO-ERROR.
                END.
                WHEN 'diners' THEN 
                DO:
                    FIND SIETransType NO-LOCK WHERE 
                        SIETransType.ButikkNr = BongHode.ButikkNr AND SIETransType.TTId = 52 AND SIETransType.TBId = 6 NO-ERROR.
                    IF NOT AVAILABLE SIETransType THEN 
                    FIND SIETransType NO-LOCK WHERE 
                        SIETransType.ButikkNr = 0 AND SIETransType.TTId = 52 AND SIETransType.TBId = 6 NO-ERROR.
                END.
                WHEN 'unknown' THEN 
                DO:
                    FIND SIETransType NO-LOCK WHERE 
                        SIETransType.ButikkNr = BongHode.ButikkNr AND SIETransType.TTId = 52 AND SIETransType.TBId = 1 NO-ERROR.
                    IF NOT AVAILABLE SIETransType THEN 
                    FIND SIETransType NO-LOCK WHERE 
                        SIETransType.ButikkNr = 0 AND SIETransType.TTId = 52 AND SIETransType.TBId = 1 NO-ERROR.
                END.
            END CASE.
        END.    
        IF NOT AVAILABLE SIETransType THEN 
            FIND SIETransType NO-LOCK WHERE 
                SIETransType.ButikkNr = 0 AND SIETransType.TTId = 65 AND SIETransType.TBId = 1 NO-ERROR.
        ASSIGN
            FakturaLinje.TTId          = SIETransType.TTId
            FakturaLinje.TBId          = SIETransType.TBId
            FakturaLinje.Antall        = SIETransType.TBId /*KOrdreLinje.Antall*/
            FakturaLinje.Varetekst     = KORdreLinje.Varetekst
            FakturaLinje.NettoPris     = KOrdreLinje.Linjesum
            FakturaLinje.NettoLinjeSum = KOrdreLinje.Linjesum 
            FakturaLinje.Linjesum      = KOrdreLinje.Linjesum 
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
                BongLinje.TTId       = SIETransType.TTId 
                BongLinje.TBId       = SIETransType.TBId
                BongLinje.BongTekst  = KORdreLinje.Varetekst
                BongLinje.Antall     = 0
                BongLinje.LinjeSum   = plLinjeSum 
                BongLinje.BongPris   = plLinjeSum 
                .
            RELEASE BongLinje.           
            NEXT LESKORDRELINJE.
        END.  
        ELSE DO:
            /* Posterer bonglinjerecord for betaling */
            ASSIGN
                BongLinje.TTId       = SIETransType.TTId 
                BongLinje.TBId       = SIETransType.TBId
                BongLinje.BongTekst  = KORdreLinje.Varetekst
                BongLinje.Antall     = SIETransType.TBId
                BongLinje.LinjeSum   = IF bRetur THEN plLinjeSum * -1 ELSE plLinjesum 
                BongLinje.BongPris   = IF bRetur THEN plLinjeSum * -1 ELSE plLinjesum
                .
            RELEASE BongLinje.           
            /* Betaling ferdig */
        END.                 
      END.
            
  END. /* LESKORDRELINJE */

  /* Vellyket fakturering */
  IF ocReturn = "Fakturer" THEN 
      DO: 
        FIND CURRENT KOrdreHode EXCLUSIVE-LOCK.
        RUN update_fakturahode.p (plfaktura_Id,"KalkulerTotaler","",5).
        RUN beregn_kunde_saldo.p ("idlist|" + STRING(FakturaHode.KundeNr),
                                ?,
                                "",
                                OUTPUT ocReturn,
                                OUTPUT obOk).

        ASSIGN 
            ocReturn = ""
            KOrdreHode.FakturertDato = TODAY /*(IF KOrdreHode.FakturertDato = ? 
                                             THEN TODAY
                                             ELSE KOrdreHode.FakturertDato) */
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
  ELSE ocReturn = "Ingen varelinje for kundeordre tilgjengelig for fakturering".
END.
ELSE ocReturn = "Kundeordre ikke tilgjengelig for fakturering".

obOk = TRIM(ocReturn) = "".
IF obOk THEN /* Sender med fakturaid tilbake. */
    ocReturn = STRING(plFaktura_Id).

RETURN.


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
          piBongLinje            = 0
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
/*            FIND FIRST BongLinje NO-LOCK WHERE                                     */
/*                BongLinje.B_Id = BongHode.B_Id NO-ERROR.                           */
/*            IF AVAILABLE BongLinje THEN                                            */
/*                pBongDato = BongLinje.Dato.                                        */
/*            ELSE                                                                   */
/*                pBongDato = ?.                                                     */
/*                                                                                   */
/*            CREATE BongLinje. /* */                                                */
/*            ASSIGN                                                                 */
/*                BongLinje.B_Id         = BongHode.B_Id                             */
/*                BongLinje.ButikkNr     = BongHode.ButikkNr                         */
/*                BongLinje.GruppeNr     = BongHode.GruppeNr                         */
/*                BongLinje.KasseNr      = BongHode.KasseNr                          */
/*                BongLinje.Dato         = TODAY /*pBongDato*/                       */
/*                BongLinje.BongNr       = BongHode.BongNr                           */
/*                BongLinje.TTId         = 65 /* Kredit */                           */
/*                BongLinje.TBId         = 1                                         */
/*                BongLinje.LinjeNr      = piBongLinje + 1 /*BongLinje*/             */
/*                BongLinje.TransDato    = TODAY /*BongHode.Dato*/                   */
/*                BongLinje.TransTid     = BongHode.Tid                              */
/*                .                                                                  */
/*                                                                                   */
/*                                                                                   */
/*            ASSIGN                                                                 */
/*                BongLinje.BongTekst  = "KREDIT"                                    */
/*                BongLinje.Antall     = 0                                           */
/*                BongLinje.LinjeSum   = plLinjeSum * (IF lTotant < 0 THEN -1 ELSE 1)*/
/*                BongLinje.BongPris   = plLinjeSum * (IF lTotant < 0 THEN -1 ELSE 1)*/
/*                .                                                                  */
/*            RELEASE BongLinje.                                                     */
            
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
