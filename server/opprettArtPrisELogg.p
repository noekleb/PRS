
/*------------------------------------------------------------------------
    File        : opprettArtPrisELogg.p
    Purpose     : 

    Syntax      :

    Description : Oppretter eLogg post for ArtPris.

    Author(s)   : Tom Nøkleby
    Created     : Tue Jan 12 10:28:52 CET 2021
    Notes       : ELogg's poster som opprettes med eksternt system 'PRICAT_KOMMISJON', vil 
                  bli behandlet av en rutine som leser disse postene, behandler dem og 
                  legger ut endringene i PRICAT EDI fil som sendes commisjonsbutikk.
                  
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE INPUT PARAMETER lArtikkelNr AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER iProfilNr AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER cESystem AS CHARACTER NO-UNDO. /* Normalt 'PRICAT_KOMMISJON'. */

DEFINE BUFFER lokArtPris FOR ArtPris.
DEFINE BUFFER lokELogg FOR ELogg.
    
FIND lokArtPris NO-LOCK WHERE 
  lokArtPris.ArtikkelNr = lArtikkelNr AND 
  lokArtPris.ProfilNr   = iProfilNr NO-ERROR.
IF AVAILABLE lokArtPris THEN 
  PRICAT_KOMMISJON:
  DO TRANSACTION:
    FIND lokELogg WHERE 
         lokELogg.TabellNavn     = "ArtPris" AND
         lokELogg.EksterntSystem = cESystem    AND
         lokELogg.Verdier        = STRING(lArtikkelNr) + '|' + STRING(iProfilNr) NO-ERROR NO-WAIT.
    IF LOCKED lokELogg THEN LEAVE PRICAT_KOMMISJON.
    ELSE DO:
      IF NOT AVAIL lokELogg THEN DO:
          CREATE lokELogg.
          ASSIGN lokELogg.TabellNavn     = "ArtPris"
                 lokELogg.EksterntSystem = cESystem   
                 lokELogg.Verdier        = STRING(lArtikkelNr) + '|' + STRING(iProfilNr).
      END.
      ASSIGN lokELogg.EndringsType = 1 
             lokELogg.Behandlet    = FALSE.
      RELEASE lokELogg.
    END.
    IF AVAILABLE lokELogg THEN 
      RELEASE lokELogg.
  END. /* TRANSACTION PRICAT_KOMMISJON */


