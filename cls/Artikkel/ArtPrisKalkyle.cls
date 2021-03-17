 
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

  CONSTRUCTOR PUBLIC ArtPrisKalkyle ( ):
    SUPER ().

    rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.
    
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
      rStandardFunksjoner:SkrivTilLogg(cLogg,
          '  Ny kalkyle for artikkel ' + STRING(ArtPris.ArtikkelNr) + 
          ' fra profil ' + STRING(piFraProfilNr) + 
          ' til profil ' + STRING(piTilProfilNr) 
          ).
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
    DO TRANSACTION: 
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

      RELEASE bufArtPris.
    END.
    
    RETURN.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC VOID SettRabatt( INPUT plArtikkelNr AS DECIMAL,
                                 INPUT piProfilNr AS INTEGER,
                                 INPUT piButNr AS INTEGER,
                                 INPUT plRab% AS DECIMAL ):
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
    /* Kallende rutine overstyrer forhåndsrabatten. */
    IF plRab% <> 0 THEN 
      lforhRab% = plRab%.
      
    DO FOR ArtPris TRANSACTION:
      FIND ArtPris EXCLUSIVE-LOCK WHERE 
        ArtPris.ArtikkelNr = plArtikkelNr AND 
        ArtPris.ProfilNr   = piProfilNr 
        NO-ERROR NO-WAIT.
      IF AVAILABLE ArtPris AND NOT LOCKED ArtPRis THEN
      RABATTKORR: 
      DO:
        /* Regner om kalkylen hvis priser er angitt. */
        /* Skal regne om også hvis rabatten er 0.    */
        IF ArtPris.InnkjopsPris[1] > 0 /* AND lforhRab% > 0 */ THEN
        DO: 
          ASSIGN 
            ArtPris.Rab1%[1]        = lforhRab%
            ArtPris.Rab1Kr[1]       = ROUND((ArtPris.InnkjopsPris[1] * lforhRab% / 100),2)
            ArtPris.Varekost[1]     = ROUND(ArtPris.InnkjopsPris[1] - (ArtPris.InnkjopsPris[1] * lforhRab% / 100),2)
            fMvaKr                  = ArtPris.Pris[1] - (ArtPris.Pris[1] / (1 + (ArtPris.Mva%[1] / 100)))
            fDbKr                   = ArtPris.Pris[1] - fMvaKr - ArtPris.Varekost[1]                   
            ArtPris.DBKr[1]         = (IF fDbKr <> ? THEN fDbKr ELSE 0)                   
            ArtPris.MvaKr[1]         = (IF fMvaKr <> ? THEN fMvaKr ELSE 0)                   
            ArtPris.Db%[1]          = ROUND((fDbKr * 100) / (ArtPris.Pris[1] - fMvaKr),2)
            ArtPris.Db%[1]          = IF ArtPris.Db%[1] = ? THEN 0 ELSE ArtPris.Db%[1]
            ArtPris.Rab1%[1]        = IF ArtPris.Rab1%[1] = ? THEN 0 ELSE ArtPris.Rab1%[1]
            ArtPris.Varekost[1]     = IF ArtPris.Varekost[1] = ? THEN 0 ELSE ArtPris.Varekost[1]
            .
        END.
        
        IF CAN-DO(cOutletLst,STRING(piButNr)) THEN
        UTPRISRABATT: 
        DO:
          /* Regner om kalkylen hvis priser er angitt. */
          IF ArtPris.Pris[1] > 0 /* AND lPrisRab% > 0 */THEN
          DO: 
            ASSIGN 
              ArtPris.Pris[1]         = ROUND(ArtPris.Pris[1] - (ArtPris.Pris[1] * lPrisRab% / 100),2) 
              fMvaKr                  = ArtPris.Pris[1] - (ArtPris.Pris[1] / (1 + (ArtPris.Mva%[1] / 100)))
              fDbKr                   = ArtPris.Pris[1] - fMvaKr - ArtPris.Varekost[1]
              ArtPris.DBKr[1]         = (IF fDbKr <> ? THEN fDbKr ELSE 0)                   
              ArtPris.MvaKr[1]         = (IF fMvaKr <> ? THEN fMvaKr ELSE 0)                   
              ArtPris.Db%[1]          = ROUND((fDbKr * 100) / (ArtPris.Pris[1] - fMvaKr),2)
              ArtPris.Db%[1]          = IF ArtPris.Db%[1] = ? THEN 0 ELSE ArtPris.Db%[1]
              ArtPris.Rab1%[1]        = IF ArtPris.Rab1%[1] = ? THEN 0 ELSE ArtPris.Rab1%[1]
              ArtPris.Varekost[1]     = IF ArtPris.Varekost[1] = ? THEN 0 ELSE ArtPris.Varekost[1]
              .
          END.        
        END. /* UTPRISRABATT */
      END. /* RABATTKORR */
      IF AVAILABLE ArtPris THEN 
        RELEASE ArtPris.
    END. /* TRANSACTION */
    
    RETURN.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose: Kopierer en endring av ArtPris til prishistorikken.
   Notes: Pristype
            1 - NOR  - Normalprisendring.
            2 - PÅ   - På tilbud
            3 - AV   - Av tilbud
            4 - ETIL - Endring av tilbud
            5 - LPÅ  - Leverandør tilbud på 
            6 - LAV  - Leverandørtilbud av.            
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC VOID settPrishistorikk( INPUT plArtikkelNr AS DECIMAL,
                                        INPUT piProfilNr AS INTEGER,
                                        INPUT piPrisType AS INTEGER ):
    DEFINE VARIABLE piEndringsNr AS INTEGER NO-UNDO.
                                              
    FIND ArtPris NO-LOCK WHERE 
      ArtPris.ArtikkelNr = plArtikkelNr AND 
      ArtPris.ProfilNr   = piProfilNr NO-ERROR.
    IF AVAILABLE ArtPris THEN 
      DO TRANSACTION:
        FIND ArtBas NO-LOCK WHERE 
          ArtBas.ArtikkelNr = ArtPris.ArtikkelNr NO-ERROR.
        FIND FIRST HPrisKo NO-LOCK WHERE
          HPrisKo.ArtikkelNr = ArtPris.ArtikkelNr AND
          HPrisKo.ProfilNr   = ArtPris.ProfilNr NO-ERROR.
        IF AVAILABLE HPrisKo THEN
          piEndringsNr = HPrisKo.EndringsNr + 1.
        ELSE
          piEndringsNr = 1.

        CREATE HPrisKo.
        ASSIGN
          HPrisKo.ArtikkelNr     = ArtPris.ArtikkelNr
          HPrisKo.ProfilNr       = ArtPris.ProfilNr
          HPrisKo.EndringsNr     = piEndringsNr
          .
        ASSIGN
          HPrisKo.LevNr          = IF AVAILABLE ArtBas THEN ArtBas.LevNr ELSE 0
          HPrisKo.ValPris        = ArtPris.ValPris[2]
          HPrisKo.InnkjopsPris   = ArtPris.InnKjopsPris[2]
          HPrisKo.Rab1Kr         = ArtPris.Rab1Kr[2]
          HPrisKo.Rab1%          = ArtPris.Rab1%[2]
          HPrisKo.Rab2Kr         = ArtPris.Rab2Kr[2]
          HPrisKo.Rab2%          = ArtPris.Rab2%[2]
          HPrisKo.Frakt          = ArtPris.Frakt[2]
          HPrisKo.Frakt%         = ArtPris.Frakt%[2]
          HPrisKo.DivKostKr      = ArtPris.DivKostKr[2]
          HPrisKo.DivKost%       = ArtPris.DivKost%[2]
          HPrisKo.Rab3Kr         = ArtPris.Rab3Kr[2]
          HPrisKo.Rab3%          = ArtPris.Rab3%[2]
          HPrisKo.DBKr           = ArtPris.DBKr[2]
          HPrisKo.DB%            = ArtPris.DB%[2]
          HPrisKo.Pris           = ArtPris.Pris[2]
          HPrisKo.EuroPris       = ArtPris.EuroPris[2]
          HPrisKo.EuroManuel     = ArtPris.EuroManuel
          .
        ASSIGN
          HPrisKo.Tilbud         = ArtPris.Tilbud
          HPrisKo.AktiveresDato  = ArtPris.AktivFraDato
          HPrisKo.AktiveresTid   = ArtPris.AktivFraTid
          HPrisKo.GyldigTilDato  = ArtPris.TilbudTilDato
          HPrisKo.GyldigTilTid   = ArtPris.TilbudTilTid
          HPrisKo.Timestyrt      = ArtPris.TilbudTimeStyrt
          HPrisKo.Aktivert       = TRUE
          HPrisKo.Type           = piPrisType
          HPrisKo.VareKost       = ArtPris.VareKost[2]
          HPrisKo.MvaKr          = ArtPris.MvaKr[2]
          HPrisKo.Mva%           = ArtPris.Mva%[2]
        
          HPrisKo.EDato          = ArtPris.EDato
          HPrisKo.ETid           = ArtPris.ETid
          HPrisKo.BrukerID       = ArtPris.BrukerId
        
          HPrisKo.RegistrertDato = ArtPris.RegistrertDato
          HPrisKo.RegistrertTid  = ArtPris.RegistrertTid
          HPrisKo.RegistrertAv   = ArtPris.RegistrertAv
          .
        IF AVAILABLE HPrisKo THEN 
          RELEASE hPrisko.
      END. /* TRANSACTION */
      
    RETURN.

  END METHOD.

  DESTRUCTOR PUBLIC ArtPrisKalkyle ( ):

  END DESTRUCTOR.

END CLASS.