/* Kreditering av kundeordre ved retur

  kordre_krediter.p
  
  Parametere: <KOrdre_id>;<evt liste over linjenr|antall>
   
  Opprettet: 15.12.2019 av TN                  
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

DEF BUFFER bufKOrdreLinje FOR KOrdreLinje.
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
      RUN update_fakturahode.p (plfaktura_Id,"Butikksalg,TotalRabatt%,Leveringsdato,LevFNr,Leveringsdato,Utsendelsesdato,Referanse,KOrdre_ID,Bilagstype,Opphav",
                                "yes" + chr(1) + 
                                 STRING(Kunde.TotalRabatt%) + CHR(1) + 
                                 STRING(TODAY) + CHR(1) + 
                                 "1" + CHR(1) + 
                                 STRING(TODAY) + CHR(1) + 
                                 STRING(TODAY) + CHR(1) + 
                                 KOrdreHode.Referanse  + CHR(1) +
                                 STRING(KOrdreHode.KOrdre_Id) + CHR(1) +
                                 (IF KOrdreHode.SendingsNr = 'RETUR' THEN '2' ELSE '1') + CHR(1) + 
                                 '10',
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
      KOrdreLinje.Aktiv = TRUE AND 
      KOrdreLinje.ByttetKOrdreLinjeNR = 0: /* Linjer som er byttet skal ikke faktureres. */

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

      /* Betalingslinjer  */
      IF CAN-DO('KREDIT,BETALT,BETALING',KORdreLinje.VareNr) THEN
      BETALINGSLINJE: 
      DO:
        IF CAN-DO('unknown,klarna,mastercard,visa,amex,diners',KORdreLinje.Varetekst) THEN 
        DO:
            CASE KOrdreLinje.Varetekst:
                WHEN 'klarna' THEN 
                DO:
                    FIND SIETransType NO-LOCK WHERE 
                        SIETransType.ButikkNr = KOrdrEHode.ButikkNr AND SIETransType.TTId = 52 AND SIETransType.TBId = 901 NO-ERROR.
                    IF NOT AVAILABLE SIETransType THEN 
                    FIND SIETransType NO-LOCK WHERE 
                        SIETransType.ButikkNr = 0 AND SIETransType.TTId = 52 AND SIETransType.TBId = 901 NO-ERROR.
                END.
                WHEN 'visa' THEN 
                DO:
                    FIND SIETransType NO-LOCK WHERE 
                        SIETransType.ButikkNr = KOrdrEHode.ButikkNr AND SIETransType.TTId = 52 AND SIETransType.TBId = 3 NO-ERROR.
                    IF NOT AVAILABLE SIETransType THEN 
                    FIND SIETransType NO-LOCK WHERE 
                        SIETransType.ButikkNr = 0 AND SIETransType.TTId = 52 AND SIETransType.TBId = 3 NO-ERROR.
                END.
                WHEN 'mastercard' THEN 
                DO:
                    FIND SIETransType NO-LOCK WHERE 
                        SIETransType.ButikkNr = KOrdrEHode.ButikkNr AND SIETransType.TTId = 52 AND SIETransType.TBId = 4 NO-ERROR.
                    IF NOT AVAILABLE SIETransType THEN 
                    FIND SIETransType NO-LOCK WHERE 
                        SIETransType.ButikkNr = 0 AND SIETransType.TTId = 52 AND SIETransType.TBId = 4 NO-ERROR.
                END.
                WHEN 'amex' THEN 
                DO:
                    FIND SIETransType NO-LOCK WHERE 
                        SIETransType.ButikkNr = KOrdrEHode.ButikkNr AND SIETransType.TTId = 52 AND SIETransType.TBId = 5 NO-ERROR.
                    IF NOT AVAILABLE SIETransType THEN 
                    FIND SIETransType NO-LOCK WHERE 
                        SIETransType.ButikkNr = 0 AND SIETransType.TTId = 52 AND SIETransType.TBId = 5 NO-ERROR.
                END.
                WHEN 'diners' THEN 
                DO:
                    FIND SIETransType NO-LOCK WHERE 
                        SIETransType.ButikkNr = KOrdrEHode.ButikkNr AND SIETransType.TTId = 52 AND SIETransType.TBId = 6 NO-ERROR.
                    IF NOT AVAILABLE SIETransType THEN 
                    FIND SIETransType NO-LOCK WHERE 
                        SIETransType.ButikkNr = 0 AND SIETransType.TTId = 52 AND SIETransType.TBId = 6 NO-ERROR.
                END.
                WHEN 'unknown' THEN 
                DO:
                    FIND SIETransType NO-LOCK WHERE 
                        SIETransType.ButikkNr = KOrdrEHode.ButikkNr AND SIETransType.TTId = 52 AND SIETransType.TBId = 1 NO-ERROR.
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
            NEXT LESKORDRELINJE.
        END.  
        ELSE DO:
            /* Betaling ferdig */
        END.                 
      END. /* BETALINGSLINJE */
            
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
        FIND CURRENT KOrdreHode NO-LOCK.
      END.
  ELSE ocReturn = "Ingen varelinje for kundeordre tilgjengelig for fakturering".
END.
ELSE ocReturn = "Kundeordre ikke tilgjengelig for fakturering".

obOk = TRIM(ocReturn) = "".
IF obOk THEN /* Sender med fakturaid tilbake. */
    ocReturn = STRING(plFaktura_Id).

RETURN.

