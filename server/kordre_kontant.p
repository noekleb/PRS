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
DEFINE VARIABLE lDec AS DECIMAL NO-UNDO.
DEFINE VARIABLE iNettButNr   AS INTEGER NO-UNDO.
DEFINE VARIABLE iNettLagerNr AS INTEGER NO-UNDO.
DEFINE VARIABLE iBatchNr     AS INTEGER NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEFINE VARIABLE bTest AS LOG NO-UNDO.

DEF BUFFER bKOrdreLinje FOR KOrdreLinje.
DEFINE BUFFER bufKOrdreHode FOR KOrdreHode.

DEFINE VARIABLE rKundeordreBehandling AS cls.Kundeordre.KundeordreBehandling NO-UNDO.
rKundeordreBehandling  = NEW cls.Kundeordre.KundeordreBehandling( ) NO-ERROR.

ASSIGN
    bTest = TRUE 
    cLogg = 'KOrdreUtlever' + REPLACE(STRING(TODAY),'/','')
    .

{syspara.i 150 1 2 iNettButNr INT}
{syspara.i 150 1 3 iNettLagerNr INT}

fKOrdre_id = DEC(ENTRY(1,icParam,";")).

IF bTest THEN 
DO:
    RUN Bibl_LoggDbFri.p(cLogg,'Start kordre_kontant.p').
    RUN Bibl_LoggDbFri.p(cLogg,'    iNettButNr: ' + STRING(iNettButNr) + '.').
    RUN Bibl_LoggDbFri.p(cLogg,'    iNettLagerNr: ' + STRING(iNettLagerNr) + '.').
    RUN Bibl_LoggDbFri.p(cLogg,'    icParam: ' + icParam + '.').
END.

FIND KOrdreHode NO-LOCK
     WHERE KOrdreHode.KOrdre_id = fKOrdre_id
     NO-ERROR.

/* Her behandles bare ordre fra nettbutikk. */
IF AVAILABLE KORdreHode AND KORdreHode.Opphav <> 10 THEN
  DO:
    ASSIGN
      obOk     = TRUE
      ocReturn = ''.

    RETURN.
  END.
  
FIND Kunde WHERE Kunde.KundeNr = KOrdreHode.KundeNr NO-LOCK NO-ERROR.
IF NOT AVAIL Kunde THEN DO:
    IF bTest THEN 
        RUN Bibl_LoggDbFri.p(cLogg,'    Ukjent kunde: ' + STRING(KOrdreHode.kundenr) + '.' ).    
    ocReturn = "Finner ikke kunde for kundeordre: " + STRING(KOrdreHode.kundenr).
    RETURN.
END.

IF AVAIL KOrdreHode THEN 
DO ON ERROR UNDO, LEAVE TRANSACTION:
  /* Henter butikken */
  FIND Butiker NO-LOCK WHERE
      Butiker.Butik = KOrdreHode.ButikkNr NO-ERROR.

  IF bTest THEN 
      RUN Bibl_LoggDbFri.p(cLogg,'    OpprettFil').    
  RUN OpprettFil.

  IF bTest THEN 
      RUN Bibl_LoggDbFri.p(cLogg,'    OpprettDatasett').    
  RUN OpprettDatasett.

  IF bTest THEN 
      RUN Bibl_LoggDbFri.p(cLogg,'    OpprettBongHode').    
  RUN OpprettBongHode.
  FIND BongHode NO-LOCK WHERE
      BongHode.B_Id = plB_Id NO-ERROR.

  IF bTest THEN 
      RUN Bibl_LoggDbFri.p(cLogg,'    Available BongHode: ' + STRING(AVAILABLE bonghode) ).    

  /* Fakturering av ordrelinjene */
  ocReturn = "".
  FOR EACH KOrdreLinje OF KOrdreHode EXCLUSIVE-LOCK WHERE
      KOrdreLinje.Leveringsdato <> ? AND
      KOrdreLinje.Faktura_Id = 0 AND 
      KOrdreLinje.Aktiv = TRUE:

      IF AVAILABLE ArtBas THEN RELEASE ArtBAs.
      
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
          ocReturn    = "Nettbutikk"
          piBongLinje = piBongLinje + 1.

      /* Her legges bongens varelinjer opp. */
      IF AVAILABLE ArtBas THEN 
      VARE_BONGLINJE:
      DO:
          IF bTest THEN 
              RUN Bibl_LoggDbFri.p(cLogg,'    start VARE_BONGLINJE').
          /* Henter lager og varekost for butikken */
          FIND Lager NO-LOCK WHERE
            Lager.ArtikkelNr = DECIMAL(KOrdreLinje.VareNr) AND
            Lager.Butik      = KOrdreHode.ButikkNr NO-ERROR.
          IF NOT AVAILABLE Lager THEN
            DO:
              CREATE Lager.
              ASSIGN
                  Lager.ArtikkelNr = DECIMAL(KOrdreLinje.VareNr)
                  Lager.Butik      = KOrdreHode.ButikkNr
                  Lager.VVareKost  = IF AVAILABLE ArtPris
                                       THEN ArtPris.Varekost[1]
                                       ELSE 0
                  Lager.VVareKost  = IF AVAILABLE ArtPris
                                       THEN ArtPris.Varekost[1]
                                       ELSE 0
                  NO-ERROR.
              /* Tar bort den delhvis opprettede posten hvis det feiler. */
              IF ERROR-STATUS:ERROR AND AVAILABLE Lager THEN
                  DELETE Lager.
            END.

          FIND VarGR NO-LOCK OF ArtBas NO-ERROR.

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
                BongLinje.TTId         = IF KOrdreLinje.Antall > 0 
                                           THEN 1  /* Kontant salg */
                                           ELSE 10 /* Retur */
                BongLinje.TBId         = 1
                BongLinje.LinjeNr      = piBongLinje /*BongLinje*/
                .
          END.

          ASSIGN
            BongLinje.ArtikkelNr = STRING(ArtBas.ArtikkelNr)
            BongLinje.Strekkode  = KOrdreLinje.Kode
            BongLinje.VareGr     = ArtBas.Vg
            BongLinje.LopeNr     = ArtBas.LopNr
            BongLinje.Storrelse  = KOrdreLinje.Storl
            BongLinje.BongTekst  = KOrdreLinje.Varetekst
            BongLinje.Antall     = KOrdreLinje.Antall
            BongLinje.LinjeSum   = abs(KOrdreLinje.NettoLinjeSum) + abs(KOrdreLinje.LinjeRabattKr)
            BongLinje.BongPris   = abs(BongLinje.LinjeSum)
            BongLinje.VVarekost  = (IF AVAILABLE Lager THEN abs(Lager.VVarekost) ELSE 0) * abs(BongLinje.Antall)
            BongLinje.LinjeRab   = abs(KOrdreLinje.LinjeRabattKr)
            BongLinje.VareGruppeNavn = IF AVAILABLE VarGr
                                     THEN VarGr.VgBeskr
                                     ELSE ""

            BongLinje.Mva%       = KOrdreLinje.Mva%
            BongLinje.MvaKr      = ABSOLUTE(KOrdreLinje.MvaKr)
            BongLinje.FeilKode   = 0
            BongLinje.NotatKode  = 0
            BongLinje.RefNr      = KOrdreHode.Opphav
            BongLinje.RefTekst   = (IF KOrdreHode.EkstOrdreNr <> '' THEN 'OrdreNr. Nettbutikk: ' ELSE 'Kundeordre: ') + 
                                   (IF KOrdreHode.EkstOrdreNr <> '' THEN KOrdreHode.EkstOrdreNr ELSE STRING(KOrdreLinje.KOrdre_Id))
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
            
          /* Legger opp overføring til lager hvis det er en retur fra nettbutikk.                                       */
          /* Retur fra nettbutikk, øker lager i nettbutikk i PRS. Dette lageret skal overføres til nettbutikkens lager. */
          IF BongLinje.Antall < 0 AND
              BongLinje.ButikkNr = iNettButNr AND  
              CAN-FIND(Butiker WHERE Butiker.butik = iNettLagerNr) THEN
              DO:
                  IF bTest THEN 
                    RUN Bibl_LoggDbFri.p(cLogg,'    start poster overføring.').    
                  RUN posterOverforing.

                  IF bTest THEN 
                    RUN Bibl_LoggDbFri.p(cLogg,'    slutt poster overføring.').    
              END.
      
          IF bTest THEN 
            RUN Bibl_LoggDbFri.p(cLogg,'    VARELINJE BongLinje.BongNr: ' + STRING(BongLinje.BongNr) + 
                                          ' BongLinje.LinjeNr: ' + STRING(BongLinje.LinjeNr) +
                                          ' BongLinje.ArtikkelNr: ' + STRING(BongLinje.ArtikkelNr) + 
                                          '.').    
            
          IF AVAILABLE BongLinje THEN RELEASE BongLinje.

        IF bTest THEN 
            RUN Bibl_LoggDbFri.p(cLogg,'    ferdig VARE_BONGLINJE').
      END. /* VARE_BONGLINJE */
      /* Her legges betalingslinjene opp */
      ELSE 
      BET_BONGLINJE:
      DO:
        IF bTest THEN 
            RUN Bibl_LoggDbFri.p(cLogg,'    start BET_BONGLINJE').

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
                BongLinje.TTId         = IF KOrdreLinje.VareNr = 'KUPONG' THEN 71 ELSE 52
                BongLinje.TBId         = 1
                BongLinje.LinjeNr      = piBongLinje /*BongLinje*/
                BongLinje.BongTekst    = IF KOrdreLinje.VareNr = 'KUPONG' THEN "KUPONG1" ELSE 'KLARNA'
                BongLinje.Strekkode    = KOrdreLinje.Kode
                .
          END.

          ASSIGN
            BongLinje.BongPris = abs(KOrdreLinje.NettoLinjeSum)
            BongLinje.LinjeSum = abs(KOrdreLinje.NettoLinjeSum)
            BongLinje.Antall   = IF BongLinje.TTId = 71 THEN 0 ELSE 901
            .


          IF bTest THEN 
            RUN Bibl_LoggDbFri.p(cLogg,'    BETALINGSLINJE BongLinje.BongNr: ' + STRING(BongLinje.BongNr) + 
                                          ' BongLinje.LinjeNr: ' + STRING(BongLinje.LinjeNr) +
                                          ' BongLinje.BongTekst: ' + BongLinje.BongTekst + 
                                          '.').    
          IF AVAILABLE BongLinje THEN RELEASE BongLinje.

        IF bTest THEN 
            RUN Bibl_LoggDbFri.p(cLogg,'    ferdig BET_BONGLINJE').
      END. /* BET_BONGLINJE */      
  END.

  /* Vellyket fakturering */
  IF ocReturn = "Nettbutikk" THEN 
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
        IF bTest THEN 
            RUN Bibl_LoggDbFri.p(cLogg,'    start FerdigBong').
        /* Legger opp betalingstransaksjon på bong */
        RUN ferdigBong.

        IF bTest THEN 
            RUN Bibl_LoggDbFri.p(cLogg,'    slutt FerdigBong').
        FIND CURRENT KOrdreHode NO-LOCK.
      END.
  ELSE ocReturn = "Ingen varelinje for kundeordre tilgjengelig for fakturering".
END.
ELSE ocReturn = "Kundeordre ikke tilgjengelig for fakturering".

/* Flagger batchen klar for oppdatering. */
IF iBatchNr > 0 THEN
DO: 
    IF bTest THEN 
        RUN Bibl_LoggDbFri.p(cLogg,'    start batchstatus iBatchNr: ' + STRING(iBatchNr) + '.' ).    
    RUN batchstatus.p (iBatchNr, 2).

    IF bTest THEN 
        RUN Bibl_LoggDbFri.p(cLogg,'    ferdig batchstatus.' ).    
END.

IF bTest THEN 
    RUN Bibl_LoggDbFri.p(cLogg,'Ferdig kordre_kontant.p').

obOk = ocReturn = "".

/* ************************  Function Prototypes ********************** */
FUNCTION FixStorl RETURNS CHARACTER 
	( pcStorl AS CHAR ) FORWARD.

/* **********************  Internal Procedures  *********************** */


PROCEDURE OpprettFil:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* Oppretter posten i filen. */
  IF NOT CAN-FIND(Filer WHERE
                  Filer.FilNavn   = "Salg fra nettbutikk " + STRING(KOrdreHode.KOrdre_Id) AND
    Filer.Dato      = TODAY /* KOrdreHode.Dato*/ AND
                  Filer.Kl        = STRING(TIME,"HH:MM") AND
                  Filer.Storrelse = 0 AND
                  Filer.Katalog   = "Nettbutikk"
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
      Filer.FilNavn     = "Salg fra nettbutikk " + STRING(KOrdreHode.KOrdre_Id) 
      Filer.Dato        = TODAY /*KOrdreHode.Dato*/
      Filer.Kl          = STRING(TIME,"HH:MM:SS") 
      Filer.Storrelse   = 0 
      Filer.Katalog     = "Nettbutikk"
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

    IF bTest THEN 
        RUN Bibl_LoggDbFri.p(cLogg,'    start OpprettBongHode').

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
        FIND FIRST Medlem WHERE medlem.kundenr = kordrehode.kundenr NO-LOCK NO-ERROR.
        IF AVAIL medlem THEN DO:
            FIND FIRST Medlemskort NO-LOCK WHERE 
                Medlemskort.medlemsnr = medlem.medlemsnr NO-ERROR.
        END.        
/*         FIND FIRST Medlemskort NO-LOCK WHERE                          */
/*             Medlemskort.InterntKKortId = KOrdreHode.KundeNr NO-ERROR. */
/*         IF AVAILABLE MedlemsKort THEN                                 */
/*           FIND Medlem OF MedlemsKort NO-LOCK NO-ERROR.                */
          
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
          BongHode.KundeNavn     = IF AVAILABLE Kunde THEN Kunde.Navn ELSE ''
          BongHode.KundeKort     = IF AVAILABLE KundeKort THEN KundeKort.KortNr ELSE ''
          .
        IF AVAILABLE Medlem THEN 
        ASSIGN
            BongHode.MedlemsNr   = Medlem.MedlemsNr 
            BongHode.MedlemsKort = IF AVAIL medlemskort THEN MedlemsKort.KortNr ELSE ""
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

    IF bTest THEN 
        RUN Bibl_LoggDbFri.p(cLogg,'    ferdig OpprettBongHode').
    
END PROCEDURE.

PROCEDURE ferdigBong:
    DEF VAR pBongDato AS DATE NO-UNDO.

    IF bTest THEN 
        RUN Bibl_LoggDbFri.p(cLogg,'    start ferdigBong').

    DO TRANSACTION:
        FIND CURRENT BongHode EXCLUSIVE-LOCK.
        BETALING:
        DO:
            ASSIGN
                BongHode.Belop      = plLinjeSum
                BongHode.BongStatus = 5 /* Oppdatert */
                BongHode.Dato       = TODAY /*pBongDato*/
                .
        END. /* BETALING */

        FIND CURRENT BongHode NO-LOCK.
    END.

    IF bTest THEN 
        RUN Bibl_LoggDbFri.p(cLogg,'    ferdig ferdigBong').

END PROCEDURE.

PROCEDURE posterOverforing:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE piTransNr AS INTEGER NO-UNDO.    

    IF bTest THEN 
        RUN Bibl_LoggDbFri.p(cLogg,'    start posterOverforing').
    
    /* Batch for TransLogg */
    IF iBatchNr = 0 THEN 
        RUN batchlogg.p (PROGRAM-NAME(1),
            "RETUR fra kundeordre " +
            string(TODAY) +
            " " +
            string(TIME,"HH:MM") +
            " " +
            USERID("dictdb"),
            OUTPUT iBatchNr).
 
    IF AVAILABLE ArtBas THEN RELEASE ArtBas.
    IF AVAILABLE ArtPris THEN RELEASE ArtPris.
    
    FIND ArtBas NO-LOCK WHERE 
        ArtBas.ArtikkelNr = DEC(BongLinje.ArtikkelNr) NO-ERROR.
    IF AVAILABLE ArtBas THEN 
    DO:
        FIND ArtPris NO-LOCK WHERE 
            ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND 
            ArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.
        IF NOT AVAILABLE ArtPris THEN 
            FIND FIRST ArtPris WHERE 
                ArtPris.ArtikkelNR = ArtBas.ArtikkelNr NO-ERROR.
    END. 
    
    /* Setter transaksjonsnummer  */
    IF piTransNr = 0 THEN
    DO:
        FIND LAST TransLogg WHERE 
            TransLogg.Butik = BongLinje.ButikkNr
            USE-INDEX TransLogg NO-ERROR.
        IF AVAILABLE TransLogg THEN
            piTransNr = TransLogg.TransNr + 1.
        ELSE
            piTransNr = 1.
    END.
    ELSE
        piTransNr = piTransNr + 1.

    /* Oppretter TransLogg */    
    CREATE TransLogg.
    NYTRANSLOGG:
    DO WHILE TRUE ON ERROR UNDO, RETRY:
        ASSIGN 
            TransLogg.Butik     = BongLinje.ButikkNr
            TransLogg.TransNr   = piTransNr
            TransLogg.SeqNr     = 1
            /* Setter inn pekere på transaksjonene */
            BongLinje.TransNr   = piTransNr
            BongLinje.SeqNr     = 1
                   NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            piTransNr = piTransNr + 1.
        ELSE LEAVE NYTRANSLOGG.
    END. /* NYTRANSLOGG */

    ASSIGN
        TransLogg.BatchNr       = iBatchNr
        TransLogg.TTId          = 6 /* Overføring */
        TransLogg.TBId          = 1 /* Når TBId = 1, trekker transaksjonen ned fra butikk og opp i mottagende butikk automstisk. */
        TransLogg.OvButik       = iNettLagerNr
        TransLogg.ArtikkelNr    = IF AVAILABLE ArtBas
                                          THEN ArtBas.ArtikkelNr
                                          ELSE 0
        Translogg.Kode          = BongLinje.Strekkode
        TransLogg.Vg            = BongLinje.VareGr
        TransLogg.LopNr         = BongLinje.LopeNr
        TransLogg.Antall        = ABS(BongLinje.Antall)
        TransLogg.KundNr        = BongHode.KundeNr

        TransLogg.LevNr         = IF AVAILABLE ArtBas
                                          THEN ArtBas.LevNr
                                          ELSE 0
        TransLogg.OvTransNr     = TransLogg.TransNr
        TransLogg.BongId        = BongLinje.BongNr
        TransLogg.BongLinjeNr   = BongLinje.LinjeNr
        TransLogg.KassaNr       = BongLinje.KasseNr
        TransLogg.ForsNr        = BongHode.KassererNr
        TransLogg.Plukket       = IF BongLinje.TTId = 6 THEN TRUE ELSE FALSE
        TransLogg.Dato          = BongLinje.TransDato
        TransLogg.Tid           = BongLinje.TransTid
        TransLogg.SelgerNr      = BongHode.SelgerNr
        TransLogg.BestNr        = 0
        TransLogg.Postert       = FALSE
        TransLogg.KortNr        = (IF BongHode.KortType = 2
                                           THEN BongHode.KundeKort
                                           ELSE BongHode.MedlemsKort)
        TransLogg.KundNr        = BongHode.KundeNr
        TransLogg.MedlemsNr     = BongHode.MedlemsNr
        TransLogg.KortType      = BongHode.KortType
        TransLogg.RefNr         = BongLinje.RefNr
        TransLogg.RefTekst      = BongLinje.RefTekst
        Translogg.Kode          = Bonglinje.Strekkode
        Translogg.BongTekst     = BongLinje.BongTekst
        TransLogg.VVareKost     = BongLinje.VVareKost / ABS(BongLinje.Antall)
        TransLogg.VVareKost     = IF TransLogg.VVareKost = ? THEN 0 ELSE Translogg.VVareKost
        TransLogg.SattVVarekost = (IF AVAILABLE ArtBas AND ArtBas.Lager = TRUE 
                                            THEN FALSE 
                                          ELSE IF CAN-DO("1,3,10",STRING(Translogg.TTId))
                                            THEN TRUE /* Skal ikke regnes om ved opp. av statistikker. */
                                          ELSE FALSE)
        TransLogg.KalkylePris   = IF AVAILABLE ArtPris
                                          THEN ArtPris.Pris[IF ArtPris.Tilbud THEN 2 ELSE 1]
                                          ELSE Translogg.KalkylePris
        TransLogg.Varekost      = IF AVAILABLE ArtPris
                                          THEN ArtPris.Varekost[IF ArtPris.Tilbud THEN 2 ELSE 1]
                                          ELSE TransLogg.Varekost
        TransLogg.Pris          = TransLogg.vVarekost
        TransLogg.RabKr         = 0
        TransLogg.SubtotalRab   = 0
        TransLogg.Mva           = 0
        TransLogg.Mva%          = 0
        .
    ASSIGN 
        TransLogg.Storl    = FixStorl(BongLinje.Storrelse)
        TransLogg.TilStorl = TransLogg.Storl
        .
        
    IF bTest THEN 
        RUN Bibl_LoggDbFri.p(cLogg,'    ferdig posterOverforing').
END PROCEDURE.

/* ************************  Function Implementations ***************** */

FUNCTION FixStorl RETURNS CHARACTER 
	( pcStorl AS CHAR ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/	
    ASSIGN
        pcStorl = TRIM(pcStorl)
        pcStorl = CAPS(pcStorl)
        pcStorl = IF (LENGTH(pcStorl) = 1 OR 
                 LENGTH(pcStorl) = 3
                 ) 
                then " " + pcStorl
                else pcStorl.          

    /* Bytter ut eventuelle comma med punkt. */
    IF INDEX(pcStorl,",") <> 0 THEN
        OVERLAY(pcStorl, INDEX(pcStorl,","), 1, "CHARACTER") = ".".

    RETURN pcStorl.   /* Function return value. */
END FUNCTION.
