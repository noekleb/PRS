CURRENT-WINDOW:WIDTH = 350.

DEF VAR cKode AS CHAR NO-UNDO.
DEF VAR cStorl AS CHAR NO-UNDO.
DEF VAR h_Prisko AS HANDLE NO-UNDO.
DEF VAR iBatchNr AS INT NO-UNDO.

DEF TEMP-TABLE tmpBongLinje LIKE BongLinje.
DEF BUFFER pantArtBas FOR ArtBas.
DEF BUFFER bButiker FOR Butiker.

DEF VAR iCl AS INT NO-UNDO.
{syspara.i 5 1 1 iCL INT}

/* Batch for TransLogg */
RUN batchlogg.p (PROGRAM-NAME(1),
                 "Korr av NonSale " +
                 string(TODAY) +
                 " " +
                 string(TIME,"HH:MM") +
                 " " +
                 USERID("dictdb"),
                 OUTPUT iBatchNr).

/* Probrambiliotek */
IF NOT VALID-HANDLE(h_PrisKo) THEN
    RUN PrisKo.p PERSISTENT SET h_PrisKo.

/* Korreksjon
 - Skape poster i Non_Sale_spes.
 - Skape TransLogg poster.  
*/

OUTPUT TO VALUE('konv_pant.txt') NO-ECHO.

FOR EACH BongLinje WHERE
  BongLinje.Dato < 05/21/2010 AND 
  BongLinje.TTId = 57
  BREAK BY BongLinje.Dato DESCENDING
        BY BongLinje.ButikkNr
        BY BongLinje.KasseNr:
  
  FIND BongHode NO-LOCK WHERE
     BongHode.B_Id = BongLinje.B_Id.
  /*  
  DISPLAY
      BongHode.ButikkNr
      BongHode.KasseNr
      BongHode.Dato
      BongHode.BongNr
      BongHode.KassererNr
      bongLinje.TTID
      BongLinje.ArtikkelNr
      BongLinje.Strekkode
      BongLinje.BongTekst
      BongLinje.Antall
      BongLinje.Linjesum
      BongLinje.BongPris
      substring(BongLinje.Originaldata,1,40) FORMAT "x(40)"
      WITH WIDTH 350.
  */    .
  /* Oppretter bonglinje (buffer) og poster ut fra den. */
  CREATE tmpBongLinje.

  BUFFER-COPY BongLinje TO tmpBongLinje.  
  ASSIGN
      cKode                   = '99999'
      tmpBongLinje.Antall     = DEC(ENTRY( 8,Bonglinje.Originaldata,";")) / 1000 /* Antall        */ 
      tmpBongLinje.LinjeSum   = (DEC(ENTRY( 9,Bonglinje.Originaldata,";")) / 100) * -1  /* Beløp         */
      tmpBongLinje.BongPris   = BongLinje.LinjeSum
      .
 
  FIND Strekkode NO-LOCK WHERE 
    Strekkode.Kode = cKode NO-ERROR.
  IF AVAILABLE Strekkode THEN 
    FIND pantArtBas OF Strekkode NO-ERROR.
  
  IF AVAILABLE pantArtBas THEN 
  DO:
    ASSIGN
      tmpBongLinje.TTID = 1
      tmpBongLinje.Strekkode  = cKode
      tmpBongLinje.VareGr     = pantArtBas.Vg
      tmpBongLinje.ArtikkelNr = STRING(pantArtBas.ArtikkelNr)
      tmpBongLinje.OrgVareGr  = BongLinje.VareGr
      tmpBongLinje.BongTekst  = pantArtBas.Beskr
      tmpBongLinje.Antall     = 1
      .
  END.

  /* Identifiserer artikkelen og henter størrelsen. */
  RUN ValiderArtikkel.

  /* Hent varelinjeinfo */
  RUN HentVareLinjeInfo.
  ASSIGN
      tmpBongLinje.Storrelse = ' 1'
      tmpBongLinje.VVAreKost = 0
      .

  /* Henter kassarapporten. */
  FIND FIRST kas_rap WHERE 
      kas_rap.dato        = BongHode.Dato     AND
      kas_rap.butikk      = BongHode.ButikkNr AND
      kas_rap.kasse       = BongHode.KasseNr  AND
      kas_rap.KassererNr >= 0 /*int(BongHode.KassererNr)*/
     EXCLUSIVE-LOCK NO-ERROR.

  IF NOT AVAILABLE Kas_Rap THEN
  DO:
      PUT UNFORMATTED 
          "** Finner ikke Kas_Rap for: "
          BongHode.Dato ' '     
          BongHode.ButikkNr ' ' 
          BongHode.KasseNr ' '  
          0 SKIP.
      NEXT.
  END.

  /* Oppretter Non_Sale_Spes poster. */
  RUN PosterNonSale.

  /* Døden */
  DELETE tmpbongLinje.
  
END.
OUTPUT CLOSE.

PROCEDURE PosterNonSale:
    /* NonSale artikler behandles her. */
    IF CAN-DO('1,3,10',STRING(tmpBongLinje.TTID)) THEN
    NON_SALE_BEHANDLING:
    DO:
      IF AVAILABLE ArtBas THEN RELEASE ArtBas.
      IF dec(tmpBongLinje.ArtikkelNr) > 0 AND CAN-FIND(ArtBas WHERE ArtBas.ArtikkelNr = dec(tmpBongLinje.ArtikkelNr)) THEN 
        FIND ArtBas NO-LOCK WHERE
             ArtBas.ArtikkelNr = dec(tmpBongLinje.ArtikkelNr) NO-ERROR.
      IF AVAILABLE ArtBas AND ArtBas.NON_Sale THEN 
        DO:
              IF ArtBas.NegVare THEN
              DO: 
                ASSIGN
                  Kas_Rap.Non_SaleNeg = Kas_Rap.Non_SaleNeg + ((tmpBongLinje.LinjeSum 
                                                            - tmpBongLinje.LinjeRab
                                                            - tmpBongLinje.SubtotalRab
                                                            - tmpBongLinje.MvaKr) * (IF tmpBongLinje.Antall > 0
                                                                                    THEN 1
                                                                                    ELSE -1))
                  Kas_Rap.Non_SaleNegAnt = Kas_Rap.Non_SaleNegAnt + tmpBongLinje.Antall.
                /* Spes. av neg nonsale. */
                FIND Non_Sale_Spes EXCLUSIVE-LOCK WHERE
                  Non_Sale_Spes.Butikk     = tmpBongLinje.ButikkNr AND
                  Non_Sale_Spes.Kasse      = tmpBongLinje.KasseNr AND
                  Non_Sale_Spes.Dato       = tmpBongLinje.Dato AND
                  Non_Sale_Spes.KassererNr = int(BongHode.KassererNr) AND 
                  Non_Sale_Spes.Non_Sale_type = 2 AND
                  Non_Sale_Spes.Kode       = tmpBongLinje.Strekkode NO-ERROR.
                IF NOT AVAILABLE Non_Sale_Spes THEN 
                DO:
                  CREATE Non_Sale_Spes.
                  ASSIGN
                    Non_Sale_Spes.Butikk     = tmpBongLinje.ButikkNr
                    Non_Sale_Spes.Kasse      = tmpBongLinje.KasseNr
                    Non_Sale_Spes.KassererNr = int(BongHode.KassererNr) 
                    Non_Sale_Spes.Dato       = tmpBongLinje.Dato
                    Non_Sale_Spes.Non_Sale_type = 2 
                    Non_Sale_Spes.Kode       = tmpBongLinje.Strekkode
                    .
                END.
                ASSIGN
                  Non_Sale_Spes.NON_SaleVerdi = Non_Sale_Spes.NON_SaleVerdi + ((tmpBongLinje.LinjeSum 
                                                                              - tmpBongLinje.LinjeRab
                                                                              - tmpBongLinje.SubtotalRab
                                                                              - tmpBongLinje.MvaKr) * (IF tmpBongLinje.Antall > 0
                                                                                                      THEN 1
                                                                                                      ELSE -1))
                  Non_Sale_Spes.NON_SaleAntall = Non_Sale_Spes.NON_SaleAntall + tmpBongLinje.Antall.                                                                                                      
              END.
              ELSE DO: 
                ASSIGN
                  Kas_Rap.Non_SalePos = Kas_Rap.Non_SalePos + ((tmpBongLinje.LinjeSum 
                                                            - tmpBongLinje.LinjeRab
                                                            - tmpBongLinje.SubtotalRab
                                                            - tmpBongLinje.MvaKr) * (IF tmpBongLinje.Antall > 0
                                                                                    THEN 1
                                                                                    ELSE -1))
                  Kas_Rap.Non_SalePosAnt = Kas_Rap.Non_SalePosAnt + tmpBongLinje.Antall
                  .   
                /* Spes. av pos nonsale. */
                FIND Non_Sale_Spes EXCLUSIVE-LOCK WHERE
                  Non_Sale_Spes.Butikk     = tmpBongLinje.ButikkNr AND
                  Non_Sale_Spes.Kasse      = tmpBongLinje.KasseNr AND
                  Non_Sale_Spes.Dato       = tmpBongLinje.Dato AND
                  Non_Sale_Spes.KassererNr = int(BongHode.KassererNr) AND 
                  Non_Sale_Spes.Non_Sale_type = 1 AND
                  Non_Sale_Spes.Kode       = tmpBongLinje.Strekkode NO-ERROR.
                IF NOT AVAILABLE Non_Sale_Spes THEN 
                DO:
                  CREATE Non_Sale_Spes.
                  ASSIGN
                    Non_Sale_Spes.Butikk = tmpBongLinje.ButikkNr
                    Non_Sale_Spes.Kasse  = tmpBongLinje.KasseNr
                    Non_Sale_Spes.KassererNr = int(BongHode.KassererNr) 
                    Non_Sale_Spes.Dato   = tmpBongLinje.Dato
                    Non_Sale_Spes.Non_Sale_type = 1 
                    Non_Sale_Spes.Kode   = tmpBongLinje.Strekkode
                    .
                END.
                ASSIGN
                  Non_Sale_Spes.NON_SaleVerdi = Non_Sale_Spes.NON_SaleVerdi + ((tmpBongLinje.LinjeSum 
                                                                              - tmpBongLinje.LinjeRab
                                                                              - tmpBongLinje.SubtotalRab
                                                                              - tmpBongLinje.MvaKr) * (IF tmpBongLinje.Antall > 0
                                                                                                      THEN 1
                                                                                                      ELSE -1))
                  Non_Sale_Spes.NON_SaleAntall = Non_Sale_Spes.NON_SaleAntall + tmpBongLinje.Antall.                                                                                                      
              END.         
        END.
    END. /* NON_SALE_BEHANDLING */
END PROCEDURE.

PROCEDURE ValiderArtikkel:

  /* Konverterer PLU koder. */
  IF DEC(tmpBongLinje.Strekkode) <= 99999 THEN
      tmpBongLinje.Strekkode = LEFT-TRIM(tmpBongLinje.Strekkode,"0").

  FIND Strekkode NO-LOCK WHERE
      Strekkode.Kode = tmpBongLinje.Strekkode NO-ERROR.
  IF NOT AVAILABLE Strekkode THEN
      FIND Strekkode NO-LOCK WHERE
      Strekkode.Kode = LEFT-TRIM(tmpBongLinje.Strekkode,"0") NO-ERROR.

  FIND ArtBas NO-LOCK WHERE
      ArtBas.ArtikkelNr = Strekkode.ArtikkelNr NO-ERROR.

  ASSIGN
      tmpBongLinje.ArtikkelNr = (IF AVAILABLE ArtBas
                                THEN STRING(ArtBas.ArtikkelNr,">>>>>>>>>>>>9")
                                ELSE STRING(Strekkode.ArtikkelNr,">>>>>>>>>>>>9"))
      tmpBongLinje.VareGr     = (IF AVAILABLE ArtBas
                               THEN ArtBas.Vg
                               ELSE tmpBongLinje.VareGr)
      tmpBongLinje.LopeNr     = (IF AVAILABLE ArtBas
                               THEN ArtBas.LopNr
                               ELSE ?)
      .

  /* Kontrollerer gyldig varegruppe */
  FIND VarGr NO-LOCK WHERE
      VarGr.Vg = tmpBongLinje.VareGr NO-ERROR.

  IF AVAILABLE VarGr THEN
    ASSIGN 
      tmpBongLinje.VareGruppeNavn = VarGr.VgBeskr
      tmpBongLinje.MvaGr          = VarGr.MomsKod 
      .

  IF AVAILABLE VarGr THEN
  DO:
      FIND HuvGr NO-LOCK WHERE
          HuvGr.Hg = VarGr.Hg NO-ERROR.
      ASSIGN
          tmpBongLinje.HovedGr            = VarGr.Hg
          tmpBongLinje.HovedGrBeskrivelse = HuvGr.HgBeskr
          .
  END.

  ASSIGN
      tmpBongLinje.Storrelse = " 1"
      .

  /* Overstyrer vvarekost på artikler som er lagerstyrt og ikke har åpen pris. */
  IF AVAILABLE ArtBas THEN
  SETT-VVAREKOST:
  DO:
/*       /* Artikler som ikke skal ha vvarekost overstyrt. */ */
/*       IF ArtBas.Opris = TRUE  THEN LEAVE SETT-VVAREKOST.   */
/*       IF Artbas.Lager = FALSE THEN LEAVE SETT-VVAREKOST.   */
      IF AVAILABLE Lager THEN RELEASE Lager.

      /* Endrer prisen på varemottakstransaksjoner.                                  */
      /* Finnes ikke lagerpost, det er åpen pris eller varekost er <= 0, benyttes    */
      /* varekost fra kassen.                                                        */
      IF ArtBas.OPris = FALSE THEN 
      DO:
        IF ArtBas.Lager = TRUE THEN 
          FIND Lager NO-LOCK WHERE        
            Lager.ArtikkelNr = ArtBas.ArtikkelNr  AND
            Lager.Butik      = tmpBongLinje.ButikkNr NO-ERROR.
      END. 

      /* TN 4/5-05 Ny håndtering av vvarekost. */
      /* Er varekost eller lagerantall 0 eller negativ, skal varekost fra kalkyle benyttes */
      /* Ikke lagerstyrte varer skal også ha varekosten satt fra kalkylen hvis denne er 0. */
      IF (AVAILABLE Lager AND (Lager.VVareKost <= 0 OR Lager.Lagant <= 0)) OR
         (ArtBas.Lager = FALSE AND tmpBongLinje.VVarekost = 0) THEN
      DO:
          /* Initierer vektet varekost. */
          FIND bButiker NO-LOCK WHERE
              bButiker.Butik = tmpBongLinje.ButikkNr NO-ERROR.
          IF AVAILABLE bButiker THEN
              FIND ArtPris OF ArtBas NO-LOCK WHERE
                   ArtPris.ProfilNr = bButiker.ProfilNr NO-ERROR.

          IF NOT AVAILABLE bButiker OR NOT AVAILABLE ArtPris THEN
          DO:
              FIND bButiker WHERE bButiker.Butik = iCl NO-ERROR.
              IF AVAILABLE bButiker THEN
                  FIND ArtPris OF ArtBas NO-LOCK WHERE
                       ArtPris.ProfilNr = bButiker.ProfilNr NO-ERROR.
          END.
          IF AVAILABLE ArtPris THEN
              tmpBongLinje.VVarekost = ArtPris.VareKost[IF ArtPris.Tilbud THEN 2 ELSE 1] * abs(tmpBongLinje.Antall).
      END.
      /* Setter varekost på lagerstyrte varer. */
      ELSE IF AVAILABLE Lager AND ArtBas.Lager THEN
          tmpBongLinje.VVarekost = Lager.VVareKost * abs(tmpBongLinje.Antall).

      /* Det kan forekomme at tmpBongLinje.Antall = 0. Da blir det ? i varekost. */
      IF tmpBongLinje.VVareKost = ? THEN
          tmpBongLinje.VVarekost = 0.

      /* Har artikkelen åpen pris, skal varekost settes fra kalkylen. */
      IF ArtBas.OPris AND (tmpBongLinje.VVarekost = 0 OR tmpBongLinje.VVarekost = ?) THEN
          tmpBongLinje.VVarekost = 0.

      /* Sjekker om varekost er satt.                                       */
      /* Er det ikke satt noen varekost, merkes transaksjonen med feilkode. */
      IF tmpBongLinje.VVarekost = 0 THEN /* or wBrutto% *** Skal også utføres for brutto% artikkler */
        DO:
          IF VALID-HANDLE(h_PrisKo) THEN
            /* NB: Varekost skal regnes av pris eksklusive rabatter       */
            /* Mva trekkes fra i rutinen som kalles. Fordi hvis det er    */
            /* gitt rabatt, er det feil mva. MvaKr må da beregnes pånytt. */
            RUN HentVareKost IN h_PrisKo (INPUT ArtBas.ArtikkelNr, 
                                          INPUT tmpBongLinje.ButikkNr, 
                                          INPUT ((tmpBongLinje.LinjeSum / ABSOLUTE(tmpBongLinje.Antall)) - ((tmpBongLinje.LinjeSum / ABSOLUTE(tmpBongLinje.Antall)) - ((tmpBongLinje.LinjeSum / ABSOLUTE(tmpBongLinje.Antall)) / (1 + (tmpBongLinje.Mva% / 100))))), 
                                          OUTPUT tmpBongLinje.VVarekost).
            ASSIGN
                tmpBongLinje.VVarekost = tmpBongLinje.VVarekost * ABSOLUTE(tmpBongLinje.Antall)
                tmpBongLinje.VVarekost = IF tmpBongLinje.VVareKost = ? 
                                        THEN 0
                                        ELSE tmpBongLinje.VVarekost
                .
        END.
  END. /* SETT-VVAREKOST */

END PROCEDURE.

PROCEDURE HentVareLinjeInfo:
  /* Henter mva koden.                                      */
  FIND Moms NO-LOCK WHERE
    Moms.MomsKod = BongLinje.MvaGr NO-ERROR.
  IF NOT AVAILABLE Moms THEN
    FIND LAST Moms NO-LOCK WHERE
      Moms.MomsProc = BongLinje.Mva%  USE-INDEX momsin NO-ERROR.
  IF AVAILABLE Moms THEN
      ASSIGN
      BongLinje.MvaGr         = Moms.MomsKod
      BongLinje.MvaGruppeNavn = Moms.Beskrivelse
      .
  /* Henter feilkodeteksten */
  FIND FIRST FeilKode NO-LOCK WHERE
      FeilKode.FeilKode = BongLinje.FeilKode NO-ERROR.
  IF AVAILABLE FeilKode THEN
      BongLinje.FeilKodeTekst = FeilKode.Beskrivelse.

  /* Henter tiltakskode */
  FIND FIRST KravKode NO-LOCK WHERE
      KravKode.KravKode = BongLinje.NotatKode NO-ERROR.
  IF AVAILABLE KravKode THEN
      BongLinje.NotatKodeTekst = KravKode.Beskrivelse.
END PROCEDURE.

PROCEDURE TransLogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR plWork        AS DEC  NO-UNDO.
DEF VAR pcTekst       AS CHAR NO-UNDO.
DEF VAR piTransNr     AS INT  NO-UNDO.
DEF VAR pcTTId        AS CHAR NO-UNDO.
DEF VAR piTTId        AS INT  NO-UNDO.
DEF VAR piTBId        AS INT  NO-UNDO.
DEF VAR piButikkNr    AS INT  NO-UNDO.
DEF VAR piMButikkNr   AS INT  NO-UNDO.
DEF VAR piEtikettBut  AS INT  NO-UNDO.
DEF VAR bEtikettKasse AS LOG  NO-UNDO.

DEFINE VARIABLE iSeq AS INTEGER    NO-UNDO.
DEFINE        VARIABLE  cInfoRad1 AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE  cInfoRad2 AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE  cInfoRad3 AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE  cInfoRad4 AS CHARACTER  NO-UNDO.

DEF BUFFER ovButiker FOR Butiker.

DEF VAR flVaremottak AS LOG NO-UNDO.

/* KonvReg */
pcTekst = "".
{syspara.i 1 2 2 pcTekst} /* RESTPAR */

ASSIGN
    flVaremottak = FALSE 
    .
/* Legger opp transaksjonene i translogg. Kun salgstransaksjoner. */
/* TANSNR og SEQNR påførs tmpBongLinjene her.                     */
SKAPELSEN:
DO:
    IF AVAILABLE ArtBas  THEN RELEASE ArtBas. 
    IF AVAILABLE ArtPris THEN RELEASE ArtPris.
    FIND Butiker NO-LOCK WHERE
        Butiker.Butik = tmpBongLinje.ButikkNr NO-ERROR.

    ASSIGN
        piTTId      = tmpBongLinje.TTId
        piTBId      = tmpBongLinje.TBId
        pcTTId      = STRING(tmpBongLinje.TTId,"999")
        piButikkNr  = tmpBongLinje.ButikkNr
        piMButikkNr = tmpBongLinje.MButikkNr
        .

    /* Setter transaksjonsnummer  */
    IF piTransNr = 0 THEN
      DO:
        FIND LAST TransLogg WHERE
          TransLogg.Butik = tmpBongLinje.ButikkNr
          USE-INDEX TransLogg NO-ERROR.
        IF AVAILABLE TransLogg THEN
          piTransNr = TransLogg.TransNr + 1.
        ELSE
          piTransNr = 1.
      END.
    ELSE
      piTransNr = piTransNr + 1.

    IF DECIMAL(tmpBongLinje.ArtikkelNr) > 0 THEN 
      FIND ArtBas NO-LOCK WHERE
        ArtBas.ArtikkelNr = DECIMAL(tmpBongLinje.ArtikkelNr) NO-ERROR.
    IF AVAILABLE ArtBas THEN
      ASSIGN
        tmpBongLinje.VareGr = ArtBas.VG
        tmpBongLinje.LopeNr = ArtBas.LopNr.
    
    TRANSLOGGEN:
    DO:
        /* Oppretter TransLogg */    
        CREATE TransLogg.
        NYTRANSLOGG:
        DO WHILE TRUE ON ERROR UNDO, RETRY:
            ASSIGN TransLogg.Butik        = piButikkNr
                   TransLogg.TransNr      = piTransNr
                   TransLogg.SeqNr        = 1
                   /* Setter inn pekere på transaksjonene */
                   tmpBongLinje.TransNr      = piTransNr
                   tmpBongLinje.SeqNr        = 1
                   NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                piTransNr = piTransNr + 1.
            ELSE LEAVE NYTRANSLOGG.
        END. /* NYTRANSLOGG */

        ASSIGN
               TransLogg.BatchNr      = iBatchNr
               TransLogg.TTId         = tmpBongLinje.TTId
               TransLogg.TBId         = tmpBongLinje.TBId
               TransLogg.ArtikkelNr   = IF AVAILABLE ArtBas
                                          THEN ArtBas.ArtikkelNr
                                          ELSE 0
               TransLogg.Vg           = tmpBongLinje.VareGr
               TransLogg.LopNr        = tmpBongLinje.LopeNr
               TransLogg.Antall       = tmpBongLinje.Antall
               TransLogg.Pris         = tmpBongLinje.LinjeSum / ABSOLUTE(tmpBongLinje.Antall)
               TransLogg.Pris         = IF TransLogg.Pris = ?
                                          THEN 0
                                          ELSE TransLogg.Pris
               TransLogg.RabKr        = (tmpBongLinje.LinjeRab + tmpBongLinje.SubtotalRab) / absolute(tmpBongLinje.Antall)
               TransLogg.RabKr        = IF TransLogg.RabKr = ?
                                          THEN 0
                                          ELSE TransLogg.RabKr
               TransLogg.KundNr       = BongHode.KundeNr

               TransLogg.LevNr        = IF AVAILABLE ArtBas
                                          THEN ArtBas.LevNr
                                          ELSE 0
               TransLogg.OvButik      = IF tmpBongLinje.TTId = 6 THEN piMButikkNr ELSE 0
               TransLogg.OvTransNr    = IF tmpBongLinje.TTId = 6 THEN TransLogg.TransNr ELSE 0
               TransLogg.BongId       = tmpBongLinje.BongNr
               TransLogg.BongLinjeNr  = tmpBongLinje.LinjeNr
               TransLogg.KassaNr      = tmpBongLinje.KasseNr
               TransLogg.ForsNr       = BongHode.KassererNr
               TransLogg.Plukket      = IF tmpBongLinje.TTId = 6 THEN TRUE ELSE FALSE
               TransLogg.Dato         = tmpBongLinje.TransDato
               TransLogg.Tid          = tmpBongLinje.TransTid
               TransLogg.SelgerNr     = BongHode.SelgerNr
               TransLogg.BestNr       = 0
               TransLogg.Postert      = FALSE
               TransLogg.KortNr       = (IF BongHode.KortType = 2
                                           THEN BongHode.KundeKort
                                           ELSE BongHode.MedlemsKort)
               TransLogg.KundNr       = BongHode.KundeNr
               TransLogg.MedlemsNr    = BongHode.MedlemsNr
               TransLogg.KortType     = BongHode.KortType
               TransLogg.RefNr        = tmpBongLinje.RefNr
               TransLogg.RefTekst     = tmpBongLinje.RefTekst
               Translogg.Kode         = tmpBongLinje.Strekkode
               Translogg.BongTekst    = tmpBongLinje.BongTekst
               TransLogg.VVareKost    = tmpBongLinje.VVareKost / ABS(tmpBongLinje.Antall)
               TransLogg.VVareKost    = IF TransLogg.VVAreKost = ? THEN 0 ELSE Translogg.VVareKost
               TransLogg.SattVVarekost = (IF AVAILABLE ArtBas AND ArtBas.Lager = TRUE 
                                            THEN FALSE 
                                          ELSE IF CAN-DO("1,3,10",STRING(Translogg.TTId))
                                            THEN TRUE /* Skal ikke regnes om ved opp. av statistikker. */
                                          ELSE FALSE)
               TransLogg.KalkylePris  = IF AVAILABLE ArtPris
                                          THEN ArtPris.Pris[IF ArtPris.Tilbud THEN 2 ELSE 1]
                                          ELSE Translogg.KalkylePris
               TransLogg.Varekost     = IF AVAILABLE ArtPris
                                          THEN ArtPris.Varekost[IF ArtPris.Tilbud THEN 2 ELSE 1]
                                          ELSE TransLogg.Varekost
               TransLogg.Mva          = (dec(tmpBongLinje.MvaKr / ABSOLUTE(tmpBongLinje.Antall)))
               Translogg.Mva          = (IF Translogg.Mva = ? THEN 0 ELSE Translogg.Mva)
               TransLogg.Mva%         = (IF AVAILABLE ArtPris
                                           THEN ArtPris.Mva%[IF ArtPris.Tilbud THEN 2 ELSE 1]
                                           ELSE (TransLogg.Mva / (TransLogg.Pris - TransLogg.Mva - TransLogg.RabKr)) * 100)
               Translogg.Mva%         = (IF Translogg.Mva% = ? THEN 0 ELSE Translogg.Mva%)
               .
        ASSIGN TransLogg.Storl        = ' 1'
               TransLogg.TilStorl     = TransLogg.Storl
               .
    END. /* TRANSLOGGEN */
END. /* SKAPELSEN */
END PROCEDURE.

