/* fix-korr_medlemsbong.p */
/* 
Dette programmet leter rett på alle bonger som har feil. Det endrer ingenting
på selve bongen, men oppretter de manglende postene i MedTrans tabellen.
Kommer det ukjente medlemskort, vil det automatisk bli opprettet et 
medlem hvor dette kortet legges inn.

Til slutt oppdateres alle berørte medlemmers saldo og statistikk seg.    
*/

                                                                  
CURRENT-WINDOW:WIDTH = 250.

DEF VAR dDato      AS DATE NO-UNDO.
DEF VAR dvVarekost AS DEC  NO-UNDO.
DEF VAR iBatchNr   AS DEC  NO-UNDO.
DEF VAR wMedlemsNr AS DEC  NO-UNDO.
DEF VAR cLoggFil   AS CHAR NO-UNDO.

DEF TEMP-TABLE ttMedlemBut 
    FIELD MedlemsNr LIKE Medlem.MedlemsNr
    FIELD ButikkNr  AS   INT.

DEF STREAM Ut.

cLoggFil = 'MedlemsBonger_M_Feil.csv'.

OUTPUT STREAM Ut TO VALUE(cLoggFil) NO-ECHO APPEND.
BUTIKK:
FOR EACH butiker NO-LOCK:
  KASSE:
  FOR EACH kasse NO-LOCK WHERE 
      kasse.butik = butiker.butik:
    DATOLOOP:
      DO dDato = DATE(4,13,2010) TO DATE(9,30,2010):
      BONGHODELOOP:
      FOR EACH bonghode EXCLUSIVE-LOCK WHERE 
          bonghode.butikknr = butiker.butik AND
          bonghode.gruppenr = 1 AND
          bonghode.kassenr  = kasse.kassenr AND
          bonghode.dato     = dDato:
        /* Leser bonger hvor medlemskortnr feilaktig har blitt plassert i medlemsnummer feltet. */
        IF BongHode.MedlemsKort = "" AND BongHode.MedlemsNr > 0 THEN
        FEILBONG:
        DO:
            PUT STREAM Ut UNFORMATTED
                STRING(TODAY) ' ' STRING(TIME,"hh:mm:ss") ' ' 
                BongHode.B_Id     ';'
                bonghode.butikknr ';'
                bonghode.dato      ';'
                bonghode.bongnr    ','
                BongHode.KundeKort   ';'
                bonghode.MedlemsKort ';'
                bonghode.medlemsnr   
                SKIP.
            
            DISP 
                bonghode.butikknr 
                bonghode.dato 
                bonghode.bongnr 
                BongHode.KundeKort
                bonghode.MedlemsKort 
                bonghode.medlemsnr
                WITH WIDTH 250.

            /* Retter opp bongen. Flytter medlemskortet til riktig felt. */
            ASSIGN
                BongHode.MedlemsKort = STRING(BongHode.MedlemsNr)
                BongHode.MedlemsNr   = 0
                .
            
            /* Gjør oppslag og henter medlemmet hvis medlemskortet finnes. */
            /* Stempler inn medlemmet på bongen.                           */
            FIND FIRST Medlemskort NO-LOCK WHERE
                MedlemsKort.KortNr = BongHode.MedlemsKort NO-ERROR.
            IF AVAILABLE MedlemsKort THEN
                ASSIGN
                BongHode.MedlemsNr = MedlemsKort.MedlemsNr
                BongHode.KortType  = 3.

            /* Ukjente medlemmer skal opprettes */
            IF BongHode.MedlemsNr = 0 THEN
            OPPRETTMEDLEM:
            DO:
              wMedlemsNr = 0.
              RUN opprettmedlem.p (?, INPUT BongHode.KundeKort, INPUT BongHode.ButikkNr , OUTPUT wMedlemsNr).
              FIND Medlem EXCLUSIVE-LOCK WHERE
                  Medlem.MedlemsNr = wMedlemsNr NO-ERROR.
              /* Det nyopprettede medlemmet skal markeres med referanse til bongen.*/
              IF AVAILABLE Medlem THEN
                ASSIGN
                  Medlem.EtterNavn    = "But/Kasse: " + 
                                        STRING(BongHode.ButikkNr) + "/" + 
                                        STRING(BongHode.KasseNr) + " " + 
                                        STRING(BongHode.Dato)
                  BongHode.MedlemsNr = Medlem.MedlemsNr
                  BongHode.KortType  = 3
                  .
              RELEASE Medlem.
            END. /* OPPRETTMEDLEM */
            
            /* Posterer medlemssalget. */
            IF BongHode.MedlemsNr <> 0 OR BongHode.Medlemskort <> "" THEN
              RUN Medlemssalg. 

        END. /* FEILBONG */
      END. /* BONGHODELOOP */
    END. /* DATOLOOP */
  END. /* KASSE */
END. /* BUTIKK */

/* Ajourfører MedlemsSaldo og statistikk på berørste medlemmer. */
/* Listen er opprettet i 'Medlemssalg'.                         */
/* Gjør dette til slutt for å slippe å gjøre dette pr. bong :)  */
FOR EACH ttMedlemBut:
    RUN beregnmedlemsaldo.p (ttMedlemBut.MedlemsNr, ttMedlemBut.ButikkNr).
    RUN oppdatmedlemstat.p(ttMedlemBut.MedlemsNr).
END.

PROCEDURE Medlemssalg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
                
/* Det er sjekket på forhånd at det er medlemssalg på denne bongen. */
/* Dvs bongen skal posteres.                                        */ 
/* Det forutsettes her at det er kontrollert at medlemmet finnes og */
/* Her posteres nå salg og betalinger.                              */

DEF BUFFER bMedTrans FOR MedTrans.                
DEF BUFFER bMedlemSaldo FOR MEdlemSaldo.
                
/* Finner ikke medlemmet, posteres ikke medlemstransene.            */
FIND Medlem NO-LOCK WHERE
    Medlem.MedlemsNr = BongHode.MedlemsNr NO-ERROR.
IF NOT AVAILABLE Medlem THEN
  RETURN "".
                 
/* Leser og posteres varesalgstranser. */
VARESALG:
FOR EACH BongLinje NO-LOCK WHERE
    BongLinje.B_Id = BongHode.B_Id AND
    BongLinje.TTId < 50 AND
    BongLinje.Makulert = FALSE:

    /* Overføringer skal ikke med */
    IF BongLinje.TTId = 6 THEN
        NEXT VARESALG.

    /* Henter artikkel. */
    FIND ArtBas NO-LOCK WHERE
        ArtBas.ArtikkelNr = DEC(BongLinje.ArtikkelNr) NO-ERROR.

    /* Transaksjonen kan ha blitt oppdatert tidligere. */
    IF CAN-FIND(bMedTrans WHERE
                bMedTrans.MedlemsNr = BongHode.MedlemsNr AND
                bMedTrans.Butik     = BongLinje.ButikkNr    AND
                bMedTrans.TransNr   = BongLinje.TransNr  AND
                bMedTrans.SeqNr     = BongLinje.SeqNr) THEN
      RETURN "AVBRYT".

    /* Hånterer varekost */
    ASSIGN
        dVVareKost = BongLinje.VVareKost.
    IF dVVareKost = 0 THEN
        RUN SetVVAreKost (INPUT BongLinje.ArtikkelNr,
                          INPUT BongLinje.ButikkNr,
                          INPUT BongLinje.LinjeSum / BongLinje.Antall,
                          OUTPUT dVVareKost).

    POSTERING-TRANS:
    DO FOR bMedTrans TRANSACTION:
      CREATE bMedTrans.
      ASSIGN
          bMedTrans.MedlemsNr      = BongHode.MedlemsNr
          bMedTrans.Butik          = BongLinje.ButikkNr
          bMedTrans.TransNr        = BongLinje.TransNr
          bMedTrans.SeqNr          = BongLinje.SeqNr

          .
      ASSIGN
        bMedTrans.KassaNr        = BongLinje.KasseNr
        bMedTrans.Dato           = BongLinje.TransDato
        bMedTrans.Tid            = BongLinje.TransTid
        bMedTrans.BongId         = BongLinje.BongNr
        bMedTrans.BongLinjeNr    = BongLinje.LinjeNr

        bMedTrans.BatchNr        = iBatchNr
        bMedTrans.TTId           = BongLinje.TTId
        bMedTrans.TBId           = BongLinje.TBId
        bMedTrans.ArtikkelNr     = dec(BongLinje.ArtikkelNr)
        bMedTrans.LevNr          = (IF AVAILABLE ArtBas
                                    THEN ArtBas.LevNr
                                    ELSE 0)
        bMedTrans.Vg             = BongLinje.VareGr
        bMedTrans.LopNr          = BongLinje.LopeNr
        bMedTrans.Storl          = BongLinje.Storrelse
        bMedTrans.Antall         = BongLinje.Antall
        bMedTrans.Pris           = BongLinje.LinjeSum
        bMedTrans.RabKr          = BongLinje.LinjeRab
        bMedTrans.SubTotalRab    = BongLinje.SubTotalRab
        bMedTrans.Mva            = BongLinje.MvaKr
        bMedTrans.VVarekost      = dVVarekost
        bMedTrans.SelgerNr       = BongHode.SelgerNr
        bMedTrans.BongTekst      = BongLinje.BongTekst
        bMedTrans.SattVVareKost  = (IF BongLinje.VVareKost <> 0
                                      THEN TRUE 
                                      ELSE FALSE)
        bMedTrans.KortNr         = IF BongHode.KortType = 3 /* Medlemskort */
                                     THEN BongHode.MedlemsKort
                                   ELSE IF BongHode.KortType = 2 
                                     THEN BongHode.KundeKort
                                   ELSE ""
        bMedTrans.ForsNr         = BongHode.KassererNr
        bMedTrans.RefNr          = BongLinje.RefNr
        bMedTrans.RefTekst       = BongLinje.RefTekst
        .

       /* Logger peker for oppdatering av medlemssaldo. */
       IF NOT CAN-FIND(FIRST ttMedlemBut WHERE
                             ttMedlemBut.MedlemsNr = BongHode.MedlemsNr AND
                             ttMedlemBut.ButikkNr  = BongHode.ButikkNr) THEN
       DO:
           CREATE ttMedlemBut.
           ASSIGN
               ttMedlemBut.MedlemsNr = BongHode.MedlemsNr
               ttMedlemBut.ButikkNr  = BongHode.ButikkNr
               .
       END.
 
    END. /* POSTERING-TRANS */

END. /* VARESALG */

END PROCEDURE.

PROCEDURE SetVVareKost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER pdArtikkelNr AS DEC                   NO-UNDO.
  DEF INPUT  PARAMETER piButikkNr   AS INT                   NO-UNDO.
  DEF INPUT  PARAMETER pdPris       AS DEC                   NO-UNDO.
  DEF OUTPUT PARAMETER pdVVareKost  LIKE BongLinje.VVareKost NO-UNDO. 

  FIND Lager NO-LOCK WHERE
      Lager.ArtikkelNr = pdArtikkelNr AND
      Lager.Butik      = piButikkNr NO-ERROR.
  IF AVAILABLE Lager THEN
      pdVVarekost = Lager.VVareKost.
  ELSE 
      pdVVareKost = 0.

  IF pdVVareKost = ? THEN
      pdVVareKost = 0.

  /* Er varekosten ikke kjent, hentes den fra kalkylen. */
  IF pdVVArekost = 0 THEN
  DO:
      FIND FIRST ArtPris NO-LOCK WHERE
          ArtPris.ArtikkelNr = pdArtikkelNr NO-ERROR.
      IF AVAILABLE ArtPris THEN
          pdVVarekost = ArtPris.Varekost[1].
  END.
END PROCEDURE.


