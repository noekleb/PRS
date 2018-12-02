/* posterMedlemsKjop.p (input BongHode.B_Id) */
DEFINE INPUT        PARAMETER dB_Id       AS DECIMAL FORMAT ">>>>>>>>>>>>9".
DEFINE INPUT        PARAMETER h_Prisko    AS HANDLE NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iAntPostert AS INTEGER NO-UNDO.

DEFINE VARIABLE dVVareKost AS DECIMAL NO-UNDO.

/* Letter transaksjonsblokk rundt postering av bongen. */
BONG_BLOKK:
DO TRANSACTION:

  FIND BongHode EXCLUSIVE-LOCK WHERE
    BongHode.b_id = dB_Id NO-ERROR.
  IF NOT AVAILABLE BongHode THEN 
    RETURN 'Ukjent B_Id'.
    
  /* Sjekker om den er postert fra før. */
  FIND MedKjop NO-LOCK WHERE
    MedKjop.B_Id = BongHode.B_id NO-ERROR.
  IF AVAILABLE MedKjop THEN 
    RETURN 'Postert tidligere'.

  /* Sjekker at det er et gyldig medlemsnr på bongen */
  IF BongHode.MedlemsNr = 0 THEN 
    RETURN.
  IF NOT CAN-FIND(Medlem WHERE
                  Medlem.MedlemsNr = BongHode.MedlemsNr) THEN 
    RETURN 'Ukjent medlem ' + STRING(BongHode.MedlemsNr).

  FIND Medlem NO-LOCK WHERE
      Medlem.MedlemsNr = BongHode.MedlemsNr NO-ERROR.
  IF NOT AVAILABLE Medlem OR Medlem.Bonus_Berettiget = FALSE THEN
      RETURN 'Ukjent eller ikke bonusberettiget.'.

  /* Oppretter Medlemskjøp og posterer totalkjøpet. */
  CREATE MedKjop.
  ASSIGN
    iAntPostert        = iAntPostert + 1
    MedKjop.B_Id       = BongHode.B_Id
    MedKjop.KjopsDato  = BongHode.Dato
    MedKjop.KjopsTid   = BongHode.Tid
    MedKjop.KjopsBelop = BongHode.Belop
    MedKjop.ButikkNr   = BongHode.ButikkNr
    MedKjop.MedlemsNr  = BongHode.MedlemsNr 
    .
    
  /* Leser og posteres varesalgstranser. */
  BONGLINJE:
  FOR EACH BongLinje NO-LOCK WHERE
    BongLinje.B_Id = BongHode.B_Id AND
    BongLinje.TTId < 50 AND
    BongLinje.Makulert = FALSE:

    /* Overføringer skal ikke med */
    IF BongLinje.TTId = 6 THEN
        NEXT BONGLINJE.

    /* Henter artikkel. */
    FIND ArtBas NO-LOCK WHERE
        ArtBas.ArtikkelNr = DEC(BongLinje.ArtikkelNr) NO-ERROR.

    /* Postering kun på artikler som gir bonus rettighet. Det gis aldri bonus */
    /* på NonSale og negative artikler.                                       */
    IF AVAILABLE ArtBas THEN 
      DO:
        IF ArtBas.Non_Sale OR ArtBas.NegVare OR ArtBas.Bonus_Givende = FALSE THEN 
        NEXT BONGLINJE.
      END.

    /* Hånterer varekost */
    ASSIGN
        dVVareKost = BongLinje.VVarekost
        dVVareKost = IF dVVareKost = ? THEN 0 ELSE dVVarekost.
    IF dVVareKost = 0 THEN
        RUN SetVVAreKost (INPUT BongLinje.ArtikkelNr,
                          INPUT BongLinje.ButikkNr,
                          INPUT BongLinje.LinjeSum / BongLinje.Antall,
                          OUTPUT dVVareKost).

    /* Summerer opp bonusgivende beløp */
    ASSIGN
      MedKjop.KjopsGrunnlag = MedKjop.KjopsGrunnlag + 
                              (IF BongLinje.Antall >= 0 THEN BongLinje.LinjeSum
                                 ELSE BongLinje.LinjeSum * -1).

  END. /* BONGLINJE */
  
  /* Er det ikke noe grunnlag, skal posten bare slettes. */
  /* Også salg som ikke gir uttelling posteres.          */
  IF MedKjop.KjopsGrunnlag = 0 THEN
    DO: 
      DELETE MedKjop.
      iAntPostert = iAntPostert - 1.      
    END.
END. /* BONG_BLOKK TRANSACTION */


/* **********************  Internal Procedures  *********************** */

PROCEDURE setVVarekost:
/*------------------------------------------------------------------------------
		Purpose:  																	  
		Notes:  																	  
------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER pdArtikkelNr AS DEC                   NO-UNDO.
  DEF INPUT  PARAMETER piButikkNr   AS INT                   NO-UNDO.
  DEF INPUT  PARAMETER pdPris       AS DEC                   NO-UNDO.
  DEF OUTPUT PARAMETER pdVVareKost  LIKE BongLinje.VVareKost NO-UNDO. 


  /* Henter varekost i butikken det overføres fra. */      
  /* Dette er pris eExMva.                         */
  FIND Lager NO-LOCK WHERE
      Lager.ArtikkelNr = pdArtikkelNr AND
      Lager.Butik      = piButikkNr NO-ERROR.
  IF AVAILABLE Lager THEN
      pdVVarekost = Lager.VVareKost.
  ELSE 
      pdVVareKost = 0.

  /* Sjekker om varekost er satt.                                       */
  /* Er det ikke satt noen varekost, merkes transaksjonen med feilkode. */
  IF pdVVareKost = 0 THEN /* or wBrutto% *** Skal også utføres for brutto% artikkler */
    DO:
      IF VALID-HANDLE(h_PrisKo) THEN
        RUN HentVareKost IN h_PrisKo (INPUT  pdArtikkelNr, 
                                      INPUT  piButikkNr, 
                                      INPUT  pdPris, 
                                      OUTPUT pdVVareKost).
    END.
  IF pdVVareKost = ? THEN
      pdVVareKost = 0.

END PROCEDURE.
