 
 /*------------------------------------------------------------------------
    File        : ArtPrisKalkyle
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : tomn
    Created     : Tue Aug 13 08:39:06 CEST 2019
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.


CLASS cls.Artikkel.ArtPrisKalkyle: 
  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/
  DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iCl AS INTEGER NO-UNDO.
  DEFINE VARIABLE cOutletLst AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cEDB-System AS CHARACTER INITIAL 'Gant Global' NO-UNDO.
  DEFINE VARIABLE lPrisRab%   AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE lforhRab%   AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE fMvaKr      AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE fDbKr       AS DECIMAL   NO-UNDO.
  
  DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.
  
  DEFINE BUFFER bufArtPris FOR ArtPris.

  CONSTRUCTOR PUBLIC ArtPrisKalkyle ( INPUT icLogg AS CHARACTER ):
    SUPER ().

    rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.
    
    cLogg = icLogg.
    IF cLogg = '' THEN 
      cLogg = 'ArtPrisKalkyle' + REPLACE(STRING(TODAY),'/','').

    {syspara.i 5 1 1 iCl INT}
    {syspara.i 22 5 2 cOutletLst}
  
    
  END CONSTRUCTOR.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC VOID KopierArtPris( INPUT plArtikkelNr AS DECIMAL,
                                    INPUT piFraProfilNr AS INTEGER,
                                    INPUT piTilProfilNr AS INTEGER ):
                                      
    FIND ArtPris NO-LOCK WHERE 
      ArtPris.ArtikkelNr = plArtikkelNr AND 
      ArtPris.ProfilNr = piFraProfilNr NO-ERROR.
      
    FIND bufArtPris EXCLUSIVE-LOCK WHERE 
      bufArtPris.ArtikkelNr = plArtikkelNr AND 
      bufArtPris.ProfilNr = piTilProfilNr NO-ERROR.
    IF NOT AVAILABLE bufArtPris AND AVAILABLE ArtPris THEN 
    DO:
      CREATE bufArtPris.
      BUFFER-COPY ArtPris
      EXCEPT ProfilNr
      TO bufArtPris
      ASSIGN 
        bufArtPris.ProfilNr = piTilProfilNr.
    END.
    
    IF AVAILABLE ArtPris AND AVAILABLE bufArtPris THEN
    DO: 
      BUFFER-COPY ArtPris 
        EXCEPT ProfilNr
        TO bufArtPris.
      rStandardFunksjoner:SkrivTilLogg(cLogg,
          '  Kopiert kalkyle for artikkel ' + STRING(ArtPris.ArtikkelNr) + 
          ' fra profil ' + STRING(piFraProfilNr) + 
          ' til profil ' + STRING(piTilProfilNr) 
          ).
    END.    
    
    RETURN.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC VOID NyArtPris( INPUT plArtikkelNr AS DECIMAL,
                                    INPUT piFraProfilNr AS INTEGER,
                                    INPUT piTilProfilNr AS INTEGER ):

    FIND ArtPris NO-LOCK WHERE 
      ArtPris.ArtikkelNr = plArtikkelNr AND 
      ArtPris.ProfilNr = piFraProfilNr NO-ERROR.
    FIND bufArtPris NO-LOCK WHERE 
      bufArtPris.ArtikkelNr = plArtikkelNr AND 
      bufArtPris.ProfilNr = piTilProfilNr NO-ERROR.
    IF AVAILABLE ArtPris AND NOT AVAILABLE bufArtPris THEN
    DO: 
      CREATE bufArtPris.
      BUFFER-COPY ArtPris 
        EXCEPT ProfilNr Tilbud
        TO bufArtPris
        ASSIGN bufArtPris.ProfilNr = pitilProfilNr
               bufArtPris.tilbud   = FALSE.
      rStandardFunksjoner:SkrivTilLogg(cLogg,
          '  Opprettet ny kalkyle for artikkel ' + STRING(ArtPris.ArtikkelNr) + 
          ' for profil ' + STRING(piTilProfilNr) + 
          ' kopiert data fra profil ' + STRING(piTilProfilNr) 
          ).
    END.
    
    RETURN.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC VOID SettRabatt( INPUT plArtikkelNr AS DECIMAL,
                                 INPUT piProfilNr AS INTEGER,
                                 INPUT piButNr AS INTEGER ):
    DEFINE BUFFER ArtPris FOR ArtPris.
        
    FIND FIRST ImpKonv NO-LOCK WHERE 
          ImpKonv.EDB-System = cEDB-System AND 
          ImpKonv.Tabell     = 'Def.Rab%' AND 
          ImpKonv.EksterntId = STRING(piButNr) NO-ERROR.
    IF AVAILABLE ImpKonv 
          THEN ASSIGN 
              lforhRab%      = DEC(ImpKonv.Merknad)
              lPrisRab%      = DEC(ImpKonv.InterntId)
              . 
    DO FOR ArtPris TRANSACTION:
      FIND ArtPris EXCLUSIVE-LOCK WHERE 
        ArtPris.ArtikkelNr = plArtikkelNr AND 
        ArtPris.ProfilNr   = piProfilNr 
        NO-ERROR NO-WAIT.
      IF AVAILABLE ArtPris AND NOT LOCKED ArtPRis THEN
      RABATTKORR: 
      DO:
        /* Regner om kalkylen hvis priser er angitt. */
        IF ArtPris.InnkjopsPris[1] > 0 AND lforhRab% > 0 THEN
        DO: 
          ASSIGN 
            ArtPris.Rab1%[1]        = lforhRab%
            ArtPris.Varekost[1]     = ROUND(ArtPris.InnkjopsPris[1] - (ArtPris.InnkjopsPris[1] * lforhRab% / 100),2)
            fMvaKr                  = ArtPris.Pris[1] - (ArtPris.Pris[1] / (1 + (ArtPris.Mva%[1] / 100)))
            fDbKr                   = ArtPris.Pris[1] - fMvaKr - ArtPris.Varekost[1]                   
            ArtPris.Db%[1]          = ROUND((fDbKr * 100) / (ArtPris.Pris[1] - fMvaKr),2)
            ArtPris.Db%[1]          = IF ArtPris.Db%[1] = ? THEN 0 ELSE ArtPris.Db%[1]
            .
        END.
        
        IF CAN-DO(cOutletLst,STRING(piButNr)) THEN
        UTPRISRABATT: 
        DO:
          /* Regner om kalkylen hvis priser er angitt. */
          IF ArtPris.Pris[1] > 0 AND lPrisRab% > 0 THEN
          DO: 
            ASSIGN 
              ArtPris.Pris[1]         = ROUND(ArtPris.Pris[1] - (ArtPris.Pris[1] * lPrisRab% / 100),2) 
              fMvaKr                  = ArtPris.Pris[1] - (ArtPris.Pris[1] / (1 + (ArtPris.Mva%[1] / 100)))
              fDbKr                   = ArtPris.Pris[1] - fMvaKr - ArtPris.Varekost[1]                   
              ArtPris.Db%[1]          = ROUND((fDbKr * 100) / (ArtPris.Pris[1] - fMvaKr),2)
              ArtPris.Db%[1]          = IF ArtPris.Db%[1] = ? THEN 0 ELSE ArtPris.Db%[1]
              .
          END.        
        END. /* UTPRISRABATT */
      END. /* RABATTKORR */
      IF AVAILABLE ArtPris THEN 
        RELEASE ArtPris.
    END. /* TRANSACTION */
    
    RETURN.

  END METHOD.

  DESTRUCTOR PUBLIC ArtPrisKalkyle ( ):

  END DESTRUCTOR.

END CLASS.