 
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
  
  DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.
  
  DEFINE BUFFER bufArtPris FOR ArtPris.

  CONSTRUCTOR PUBLIC ArtPrisKalkyle ( INPUT icLogg AS CHARACTER ):
    SUPER ().

    rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.
    
    cLogg = icLogg.
    IF cLogg = '' THEN 
      cLogg = 'ArtPrisKalkyle' + REPLACE(STRING(TODAY),'/','').

  
    
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

  DESTRUCTOR PUBLIC ArtPrisKalkyle ( ):

  END DESTRUCTOR.

END CLASS.