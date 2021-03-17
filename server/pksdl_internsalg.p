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
DEFINE VARIABLE bHarGjortDet AS LOG NO-UNDO.
DEFINE VARIABLE cOutletLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPkSdlNr AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.
DEFINE VARIABLE iPksdlOpphav AS INT NO-UNDO.
DEFINE VARIABLE cButLstKommisjon AS CHARACTER NO-UNDO.
DEFINE VARIABLE iGantAktiv AS INTEGER NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.

DEFINE BUFFER bufButiker FOR Butiker.
DEFINE BUFFER bufPkSdlHode FOR PkSdlHode.

rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( ).

cUserId = icParam.

{syspara.i 22 5 2 cOutletLst}
{syspara.i 210 100 8 iGantAktiv INT}

{syspara.i 2 1 1 fEuroKurs DECIMAL}
IF fEuroKurs = ? OR fEuroKurs = 0 THEN
  fEuroKurs = 0.5.
{syspar2.i 22 20 1 iOutlet INT}

/* Henter liste med kommisjonsbutikker. */    
IF iGantAktiv = 1 THEN 
DO:
  rStandardFunksjoner:getKommisjonsButLst(OUTPUT cButLstKommisjon).
  cButLstKommisjon = ',' + cButLstKommisjon.    
END.

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
  ASSIGN 
    cPkSdlNr     = PkSdlHode.PkSdlNr
    iPksdlOpphav = INT(PkSdlHode.PkSdlOpphav)
    .
  /* Styrer hvilken butikk det skal faktureres fra.                           */
  /* For Outlet - but 20. For overføring fra Nettbutikk overskuddslager - 50. */
  IF bHarGjortDet = FALSE THEN 
    DO:
        bHarGjortDet = TRUE.
        IF CAN-DO('6',STRING(PkSdlHode.PkSdlOpphav)) THEN 
            {syspara.i 22 20 2 iOverskuddslager INT}
        ELSE 
            {syspara.i 22 20 1 iOverskuddslager INT}
    END.

  IF CAN-DO(cOutletLst + cButLstKommisjon,STRING(PkSdlLinje.ButikkNr)) THEN. /* Gjør ingenting. */ 
  ELSE IF NOT CAN-DO('4,5,6',STRING(PkSdlHode.PkSdlOpphav)) THEN 
      LEAVE KJOP.
  
  FIND bufButiker NO-LOCK WHERE 
      bufButiker.Butik = INT(ihBuffer:BUFFER-FIELD("ButikkNr"):BUFFER-VALUE) NO-ERROR.
  FIND Butiker NO-LOCK WHERE
      Butiker.Butik = iOverskuddslager NO-ERROR.
  IF AVAILABLE Butiker THEN
      FIND LAST Kasse NO-LOCK WHERE Kasse.ButikkNr = PkSdlLinje.ButikkNr NO-ERROR.
  
  RUN OpprettFil.
  RUN opprettDatasett.
  RUN OpprettBongHode.

  FIND BongHode NO-LOCK WHERE
      BongHode.B_Id = plB_Id NO-ERROR.
  IF BongHode.KundeNr > 0 THEN 
      FIND Kunde NO-LOCK WHERE 
          Kunde.KundeNr = BongHode.KundeNr NO-ERROR.
          
  REPEAT WHILE NOT hQuery:QUERY-OFF-END:
    FIND PkSdlLinje NO-LOCK WHERE 
        PkSdlLinje.PkSdlId      = DEC(ihBuffer:BUFFER-FIELD("PkSdlId"):BUFFER-VALUE) AND 
        PkSdlLinje.PkSdlLinjeId = INT(ihBuffer:BUFFER-FIELD("PkSdlLinjeId"):BUFFER-VALUE)
        NO-ERROR.
         
    IF NOT AVAIL PkSdlLinje THEN DO:
      ocReturn = "Pakkseddel-linje ikke tilgjengelig for oppdatering".
      UNDO, LEAVE.
    END.

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
      FIND StrKonv NO-LOCK WHERE
          StrKonv.StrKode = PkSdlLinje.StrKode NO-ERROR.

      ASSIGN
        BongLinje.TTId       = 1 /* Salg */ 
        BongLinje.ArtikkelNr = STRING(PkSdlLinje.ArtikkelNr)
        BongLinje.Strekkode  = PkSdlLinje.Kode
        BongLinje.VareGr     = ArtBas.Vg
        BongLinje.LopeNr     = ArtBas.LopNr
        BongLinje.Storrelse  = StrKonv.Storl
        BongLinje.BongTekst  = ArtBas.BongTekst
        BongLinje.Antall     = PkSdlLinje.AntLevert

        BongLinje.Mva%       = IF (AVAILABLE Kunde AND Kunde.MvaFri = TRUE) THEN 0 ELSE Moms.MomsProc
        BongLinje.LinjeSum   = PkSdlLinje.AntLevert * PkSdlPris.NyVareKost + 
                               ((PkSdlLinje.AntLevert * PkSdlPris.NyVareKost * BongLinje.Mva%) / 100)
        BongLinje.BongPris   = BongLinje.LinjeSum
        BongLinje.VVarekost  = PkSdlLinje.AntLevert * PkSdlPris.NyVareKost
        BongLinje.LinjeRab   = 0
        BongLinje.VareGruppeNavn = IF AVAILABLE VarGr
                                 THEN VarGr.VgBeskr
                                 ELSE ""
        BongLinje.MvaKr      = ((PkSdlLinje.AntLevert * PkSdlPris.NyVareKost * BongLinje.Mva%) / 100)
        BongLinje.FeilKode   = 0
        BongLinje.NotatKode  = 0
        BongLinje.RefNr      = 1
        BongLinje.Reftekst   = 'PksdlNr: ' + PkSdlHode.PkSdlNr + '.'
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
                BongLinje.TTId         = IF CAN-DO('6',STRING(iPksdlOpphav)) THEN 114 ELSE 65 /* Nettbutikk ventelager skal ikke faktureres. Overskuddsvarer skal faktureres. */
                BongLinje.TBId         = 1
                BongLinje.LinjeNr      = piBongLinje + 1 /*BongLinje*/
                BongLinje.TransDato    = TODAY /*BongHode.Dato*/
                BongLinje.TransTid     = BongHode.Tid
                BongLinje.RefNr        = 1
                BongLinje.Reftekst     = 'PksdlNr: ' + cPkSdlNr + '.'
                .

            ASSIGN
                BongLinje.BongTekst  = "KREDIT"
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
            BongHode.Dato     = TODAY USE-INDEX Bong NO-ERROR.
        IF AVAILABLE BongHode THEN
            piBongNr = BongHode.BongNr + 1.
    END. /* BLOKKEN */

    BONGHODE:
    DO TRANSACTION:
        /* Henter kasserer for kassen. */
        FIND FIRST ButikkForsalj NO-LOCK WHERE
            ButikkForsalj.Butik = DataSett.ButikkNr NO-ERROR.
        IF AVAILABLE ButikkForsalj THEN
            FIND Forsalj NO-LOCK WHERE 
                Forsalj.ForSnr = ButikkForsalj.ForsNr NO-ERROR.
            
        FIND Kunde NO-LOCK WHERE Kunde.KundeNr = bufButiker.KundeNr NO-ERROR.
        
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
          BongHode.KassererNr    = IF AVAILABLE ButikkForsalj THEN ButikkForsalj.ForsNr ELSE 0
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
    
    DO TRANSACTION:
        FIND bufPkSdlHode EXCLUSIVE-LOCK WHERE 
            RECID(bufPkSdlHode) = RECID(PkSdlHode) NO-ERROR.
        IF AVAILABLE bufPkSdlHode THEN 
        DO:
            ASSIGN
            bufPkSdlHode.Merknad = bufPkSdlHode.Merknad + 
                                   (IF bufPkSdlHode.Merknad <> '' THEN CHR(10) ELSE '') + 
                                   'Fakturert fra but: ' + STRING(BongHode.butikkNr) + 
                                   '/Kasse: ' + STRING(BongHode.KasseNr) + 
                                   '/Dato: ' + STRING(BongHode.Dato) + ' ' +
                                   '/BongNr: ' + STRING(BongHode.BongNr) + ' ' +
                                   STRING(BongHode.Tid,"HH:MM:SS") + '.'  
            .
            RELEASE bufPkSdlHode.            
        END.
    END.

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
                  Filer.Kl        = STRING(TIME,"HH:MM:SS") AND
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
