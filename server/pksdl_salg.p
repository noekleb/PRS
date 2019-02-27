/* Registrer salg fra pakkseddel
   Parameter:  <PkSdlId>;<brukerid>
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR fPkSdlId        AS DEC    NO-UNDO.
DEF VAR cUserId         AS CHAR   NO-UNDO.
DEF VAR cBestNrListe    AS CHAR   NO-UNDO.
DEF VAR h_PrisKo        AS HANDLE NO-UNDO.
DEF VAR fDbKr           AS DEC    NO-UNDO.
DEF VAR fDb%            AS DEC    NO-UNDO.
DEF VAR fMvaKr          AS DEC    NO-UNDO.
DEF VAR fRab1Kr         AS DEC    NO-UNDO.
DEF VAR fRab1%          AS DEC    NO-UNDO.
DEF VAR fEuroKurs       AS DEC    NO-UNDO.
DEF VAR fFrakt%         AS DEC    NO-UNDO.
DEF VAR fFrakt          AS DEC    NO-UNDO.
DEF VAR fVarekost       AS DEC    NO-UNDO.
DEF VAR fInnkjopsPris   AS DEC    NO-UNDO.
DEF VAR fPris           AS DEC    NO-UNDO.
DEF VAR hQuery          AS HANDLE NO-UNDO.
DEF VAR iCurrBestNr     AS INT    NO-UNDO.
DEF VAR iMottaksId      AS INT    NO-UNDO INIT 1.
DEF VAR cButliste       AS CHAR   NO-UNDO.
DEF VAR piBongLinje     AS INT    NO-UNDO.
DEF VAR plLinjeSum      AS DEC    NO-UNDO.
DEF VAR lDataSettId     AS DEC    NO-UNDO.
DEF VAR plB_Id          LIKE BongHode.B_Id NO-UNDO.
DEF VAR lFilId          AS DEC    NO-UNDO.
DEF VAR iOverskuddslager AS INT NO-UNDO.
DEF VAR iMButikkNr       AS INT NO-UNDO.
DEF VAR iOutlet          AS INT NO-UNDO.

DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.

cUserId = icParam.

{syspara.i 2 1 1 fEuroKurs DECIMAL}
IF fEuroKurs = ? OR fEuroKurs = 0 THEN
  fEuroKurs = 0.5.

{syspara.i 22 20 1 iOverskuddslager INT}
{syspar2.i 22 20 1 iOutlet INT}

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " WHERE AntLevert > 0 BY LinjeNr").
hQuery:QUERY-OPEN().

KJOP:
DO ON ERROR UNDO, LEAVE:
  hQuery:GET-FIRST().
  IF NOT ihBuffer:AVAIL THEN DO:
    ocReturn = "Ingen linjer med levert antall er valgt".
    UNDO, LEAVE.  
  END. 
  
  /* Henter linje og hode til pakkseddel. */
  FIND PkSdlLinje NO-LOCK WHERE 
      PkSdlLinje.PkSdlId      = DEC(ihBuffer:BUFFER-FIELD("PkSdlId"):BUFFER-VALUE) AND 
      PkSdlLinje.PkSdlLinjeId = INT(ihBuffer:BUFFER-FIELD("PkSdlLinjeId"):BUFFER-VALUE)
      NO-ERROR.
  IF AVAILABLE PkSdlLinje THEN
      FIND PkSdlHode OF PkSdlLinje NO-LOCK NO-ERROR.
  IF NOT AVAILABLE PkSdlHode THEN
      LEAVE KJOP.
  FIND Butiker NO-LOCK WHERE
      Butiker.Butik = PkSdlLinje.ButikkNr NO-ERROR.
  IF AVAILABLE Butiker THEN
      FIND LAST Kasse NO-LOCK WHERE Kasse.ButikkNr = PkSdlLinje.ButikkNr NO-ERROR.
  FIND ArtBas NO-LOCK WHERE
      ArtBas.ArtikkelNr = PkSdlLinje.ArtikkelNr NO-ERROR.
  FIND ArtPris NO-LOCK WHERE
      ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND 
      ArtPris.ProfilNr = Butiker.ProfilNr NO-ERROR.
  IF NOT AVAILABLE ArtPris THEN
      FIND FIRST ArtPris NO-LOCK WHERE
      ArtPris.ArtikkelNr = ArtBas.ArtikkelNr NO-ERROR.
  FIND StrKonv NO-LOCK WHERE
      StrKonv.StrKode = PkSdlLinje.StrKode NO-ERROR.
  FIND FIRST FakturaHode WHERE
      FakturaHode.Faktura_Id = DEC(PkSdlHode.EkstId) AND 
      FakturaHode.FakturaNr  = DEC(PkSdlHode.PkSdlNr) NO-ERROR.
  IF NOT AVAILABLE FakturaHode THEN
      LEAVE KJOP.
  FIND Kunde NO-LOCK WHERE
      Kunde.KundeNr = FakturaHode.KundeNr NO-ERROR.
  IF AVAILABLE Kunde THEN
  FIND FIRST Butiker NO-LOCK WHERE
      Butiker.KundeNr = FakturaHode.KundeNr NO-ERROR.
  IF NOT AVAILABLE Butiker THEN
      LEAVE KJOP.
  FIND LAST Kasse NO-LOCK WHERE 
      Kasse.ButikkNr = Butiker.Butik NO-ERROR.


  /* Oppsett for GANT */
  IF NOT (iOverskuddslager = Butiker.Butik AND iOutlet > 0) THEN
      LEAVE KJOP.
      
  /* Finnes bongen fra før, skal den ikke opprettes på nytt :) */
  

  RUN OpprettFil.
  RUN opprettDatasett.
  RUN OpprettBongHode.

  FIND BongHode NO-LOCK WHERE
      BongHode.B_Id = plB_Id NO-ERROR.

  REPEAT WHILE NOT hQuery:QUERY-OFF-END:
    FIND PkSdlLinje NO-LOCK WHERE 
        PkSdlLinje.PkSdlId      = DEC(ihBuffer:BUFFER-FIELD("PkSdlId"):BUFFER-VALUE) AND 
        PkSdlLinje.PkSdlLinjeId = INT(ihBuffer:BUFFER-FIELD("PkSdlLinjeId"):BUFFER-VALUE)
        NO-ERROR.
         
    IF NOT AVAIL PkSdlLinje THEN DO:
      ocReturn = "Pakkseddel-linje ikke tilgjengelig for oppdatering".
      UNDO, LEAVE.
    END.

    FIND PkSdlPris NO-LOCK WHERE
        PkSdlPris.PkSdlId    = PkSdlLinje.PkSdlId AND
        PkSdlPris.ArtikkelNr = PkSdlLinje.ArtikkelNr NO-ERROR.
  
    piBongLinje = piBongLinje + 1.

    /* Oppretter bonglinje */
    CREATE_BONGLINJE:
    DO:
      FIND BongLinje EXCLUSIVE-LOCK WHERE
           BongLinje.ButikkNr = BongHode.ButikkNr AND
           BongLinje.GruppeNr = BongHode.GruppeNr AND
           BongLinje.KasseNr  = BongHode.KasseNr  AND
           BongLinje.Dato     = BongHode.Dato     AND
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
            BongLinje.Dato         = BongHode.Dato    
            BongLinje.TransDato    = BongHode.Dato
            BongLinje.TransTid     = BongHode.Tid
            BongLinje.BongNr       = BongHode.BongNr   
            BongLinje.LinjeNr      = piBongLinje /*BongLinje*/
            .
      END.
    END. /* CREATE_BONGLINJE */

    BONGLINJE:
    DO:
      /* Henter lager og varekost for butikken */
      FIND Lager EXCLUSIVE-LOCK WHERE
        Lager.ArtikkelNr = PkSdlLinje.ArtikkelNr AND
        Lager.Butik      = PkSdlLinje.ButikkNr NO-ERROR NO-WAIT.
      IF NOT AVAILABLE Lager THEN
        DO:
          CREATE Lager.
          ASSIGN
              Lager.ArtikkelNr = PkSdlLinje.ArtikkelNr
              Lager.Butik      = PkSdlLinje.ButikkNr
              Lager.VVareKost  = IF AVAILABLE ArtPris
                                   THEN ArtPris.Varekost[1]
                                   ELSE 0
              .
        END.
      IF Lager.VVareKost = 0 THEN
          Lager.VVareKost  = IF AVAILABLE ArtPris
                               THEN ArtPris.Varekost[1]
                               ELSE 0.

      FIND VarGr NO-LOCK OF ArtBas NO-ERROR.
      FIND Moms OF VarGr NO-LOCK NO-ERROR.

      ASSIGN
        BongLinje.TTId       = 1 /* Salg */ 
        BongLinje.ArtikkelNr = STRING(PkSdlLinje.ArtikkelNr)
        BongLinje.Strekkode  = ""
        BongLinje.VareGr     = ArtBas.Vg
        BongLinje.LopeNr     = ArtBas.LopNr
        BongLinje.Storrelse  = StrKonv.Storl
        BongLinje.BongTekst  = ArtBas.BongTekst
        BongLinje.Antall     = PkSdlLinje.AntLevert

        BongLinje.LinjeSum   = PkSdlLinje.AntLevert * PkSdlPris.NyVareKost + 
                               ((PkSdlLinje.AntLevert * PkSdlPris.NyVareKost * Moms.MomsProc) / 100)
        BongLinje.BongPris   = BongLinje.LinjeSum
        BongLinje.VVarekost  = PkSdlLinje.AntLevert * PkSdlPris.NyVareKost
        BongLinje.LinjeRab   = 0
        BongLinje.VareGruppeNavn = IF AVAILABLE VarGr
                                 THEN VarGr.VgBeskr
                                 ELSE ""
        BongLinje.Mva%       = Moms.MomsProc
        BongLinje.MvaKr      = ((PkSdlLinje.AntLevert * PkSdlPris.NyVareKost * Moms.MomsProc) / 100)
        BongLinje.FeilKode   = 0
        BongLinje.NotatKode  = 0
        BongLinje.RefNr      = 0
        BongLinje.RefTekst   = PkSdlHode.EkstId + "/" + PkSdlHode.PkSdlNr 
        .

      ASSIGN
        plLinjeSum = plLinjeSum + BongLinje.LinjeSum
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

    hQuery:GET-NEXT().
  END.

END. /* KJOP */

DELETE OBJECT hQuery NO-ERROR.

RUN ferdigBong.


/* **********************  Internal Procedures  *********************** */

PROCEDURE ferdigBong:
/*------------------------------------------------------------------------------
		Purpose:  																	  
		Notes:  																	  
------------------------------------------------------------------------------*/
    DEF VAR pBongDato AS DATE NO-UNDO.

    IF NOT AVAILABLE BongHode THEN
        RETURN.

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
                
            /* Betalingsrecord. */
            CREATE BongLinje. /* */
            ASSIGN
                BongLinje.B_Id         = BongHode.B_Id
                BongLinje.ButikkNr     = BongHode.ButikkNr 
                BongLinje.GruppeNr     = BongHode.GruppeNr 
                BongLinje.KasseNr      = BongHode.KasseNr  
                BongLinje.Dato         = TODAY /*pBongDato*/     
                BongLinje.BongNr       = BongHode.BongNr   
                BongLinje.TTId         = 50 /* Kontant */
                BongLinje.TBId         = 1
                BongLinje.LinjeNr      = piBongLinje + 1 /*BongLinje*/
                BongLinje.TransDato    = TODAY /*BongHode.Dato*/
                BongLinje.TransTid     = BongHode.Tid
                .


            ASSIGN
                BongLinje.BongTekst  = "KONTANT"
                BongLinje.Antall     = 0
                BongLinje.LinjeSum   = plLinjeSum
                BongLinje.BongPris   = plLinjeSum
                .
            RELEASE BongLinje.
            
            FIND CURRENT BongHode EXCLUSIVE-LOCK.
            ASSIGN
                BongHode.Belop      = plLinjeSum
                BongHode.BongStatus = 5 /* Oppdatert */
                BongHode.Dato       = TODAY /*pBongDato*/
                .
        END. /* BETALING */

        FIND CURRENT BongHode NO-LOCK.
    END.

END PROCEDURE.

PROCEDURE OpprettBongHode:
/*------------------------------------------------------------------------------
		Purpose:  																	  
		Notes:  																	  
------------------------------------------------------------------------------*/
    DEF VAR piBongNr    AS INT NO-UNDO.

    FIND DataSett NO-LOCK WHERE
        DataSett.DataSettId = lDataSettId NO-ERROR.

    piBongNr = 1.
    BLOKKEN:
    DO:
        FIND LAST BongHode NO-LOCK WHERE
            BongHode.ButikkNr = Butiker.Butik AND
            BongHode.GruppeNr = 1 AND
            BongHode.KasseNr  = Kasse.KasseNr  AND
            BongHode.Dato     = TODAY /* AND
            BongHode.BongNr   = piBongNr*/ USE-INDEX Bong NO-ERROR.
        IF AVAILABLE BongHode THEN
            piBongNr = BongHode.BongNr + 1.
    END. /* BLOKKEN */

    BONGHODE:
    DO TRANSACTION:
        /* Henter kasserer for kassen. */
        FIND FIRST ButikkForsalj NO-LOCK WHERE
            ButikkForsalj.Butik = DataSett.ButikkNr NO-ERROR.
        IF AVAILABLE ButikkForsalj THEN
            FIND Forsalj OF KOrdreHode NO-ERROR.
        FIND Kunde NO-LOCK WHERE Kunde.KundeNr = Butiker.KundeNr NO-ERROR.
        IF AVAILABLE Kunde THEN 
          FIND FIRST KundeKort OF Kunde NO-LOCK NO-ERROR.
        CREATE BongHode.
        ASSIGN
          piBongLinje            = 0
          BongHode.ButikkNr      = DataSett.ButikkNr 
          BongHode.GruppeNr      = 1 
          BongHode.KasseNr       = Kasse.KasseNr  
          BongHode.Dato          = TODAY 
          BongHode.Tid           = TIME
          BongHode.BongNr        = piBongNr
          BongHode.BongStatus    = 0 /* Under klargjøring */
          BongHode.OpdKvit       = TRUE
          Bonghode.DataSettId    = DataSett.DataSettId
          BongHode.Utskriftskopi = "Utskriftskopi ikke mottat for kvittering " + 
                                   STRING(piBongNr) + "."
          BongHode.KassererNr    = IF AVAILABLE Forsalj THEN Forsalj.ForsNr ELSE 0
          BongHode.KassererNavn  = IF AVAILABLE Forsalj
                                     THEN Forsalj.FoNamn
                                     ELSE "* Ukjent kasserer *"
          BongHode.KOrdre_Id     = 0
          BongHode.Konvertert    = TRUE
          BongHode.SelgerNr      = 0
          BongHode.KundeNr       = IF AVAILABLE Kunde THEN Kunde.KundeNr ELSE 0
          BongHode.KundeKort     = IF AVAILABLE KundeKort THEN KundeKort.KortNr ELSE ''
          BongHode.KundeNavn     = IF AVAILABLE Kunde THEN Kunde.Navn ELSE ''
          .
        FIND CURRENT BongHode NO-LOCK.
        ASSIGN
            plB_Id = BongHode.B_Id
            . 
    END. /* BONGHODE */

END PROCEDURE.

PROCEDURE OpprettDatasett:
/*------------------------------------------------------------------------------
		Purpose:  																	  
		Notes:  																	  
------------------------------------------------------------------------------*/
    DEF VAR piSettNr AS INT  NO-UNDO.
    DEF VAR pdKDato  AS DATE NO-UNDO.

    OPPRETTDATASETT:
    DO TRANSACTION:

      ASSIGN pdKDato = TODAY. /*KOrdreLinje.Leveringsdato.*/

      /* Finner neste SettNr */
      FIND LAST Datasett NO-LOCK WHERE
          Datasett.ButikkNr = Butiker.Butik AND
          Datasett.GruppeNr = 1 AND
          Datasett.KasseNr  = Kasse.KasseNr  AND
          Datasett.Dato     = pdKDato AND
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
        DataSett.ButikkNr   = Butiker.Butik 
        DataSett.GruppeNr   = 1 
        DataSett.KasseNr    = Kasse.KasseNr
        DataSett.Dato       = pdKDato
        DataSett.SettNr     = piSettNr
        DataSett.Tid        = 0
        DataSett.FilId      = lFilId
        DataSett.FilType    = 1 
        .
      RELEASE Datasett.
    END. /* OPPRETTDATASETT */

END PROCEDURE.

PROCEDURE OpprettFil:
/*------------------------------------------------------------------------------
		Purpose:  																	  
		Notes:  																	  
------------------------------------------------------------------------------*/

  /* Oppretter posten i filen. */
  IF NOT CAN-FIND(Filer WHERE
                  Filer.FilNavn   = "Salg fra pakkseddel " + STRING(PkSdlHode.PkSdlNr) AND
                  Filer.Dato      = TODAY AND
                  Filer.Kl        = STRING(TIME,"HH:MM") AND
                  Filer.Storrelse = 0 AND
                  Filer.Katalog   = "Pakkseddel"
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
      Filer.FilNavn     = "Salg fra Pakkseddel " + STRING(PkSdlHode.PkSdlNr) 
      Filer.Dato        = TODAY
      Filer.Kl          = STRING(TIME,"HH:MM:SS") 
      Filer.Storrelse   = 0 
      Filer.Katalog     = "Pakkseddel"
      Filer.AntLinjer   = 0
      Filer.FilType     = 1 
      Filer.Innlest     = TRUE
      Filer.InnlestDato = TODAY 
      Filer.InnlestKl   = TIME
      Filer.Oppdatert   = TRUE
      Filer.OppdatertDato = TODAY 
      Filer.OppdatertKl = TIME
      .
    RELEASE Filer.
  END.


END PROCEDURE.
