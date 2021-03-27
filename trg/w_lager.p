TRIGGER PROCEDURE FOR WRITE OF SkoTex.Lager  OLD BUFFER oldLager.

{trg\c_w_trg.i &Fil="SkoTex.Lager" &Type= "W"}

DEF BUFFER trgEkstEDBSystem FOR EkstEDBSystem.
DEF BUFFER trgArtBas        FOR ArtBas.

ASSIGN 
    Lager.EndretDateTime = NOW
    .

FIND trgArtBas OF Lager NO-LOCK NO-ERROR.

IF AVAILABLE trgArtBas AND trgArtBas.iKasse = TRUE THEN 
DO:
  IF (Lager.VVareKost <> oldLager.VVAreKost OR 
     Lager.LagAnt     <> oldLager.LagAnt) THEN DO:
    /* Logger utlegg for de profiler det gjelder. */
    FOR EACH ArtPris NO-LOCK WHERE ArtPris.ArtikkelNr = trgArtBas.ArtikkelNr:
      IF NOT CAN-FIND(FIRST ELogg WHERE 
           ELogg.TabellNavn     = "ArtPris" AND
           ELogg.EksterntSystem = "POS"    AND
           ELogg.Verdier        = STRING(ArtPris.ArtikkelNr) + CHR(1) + string(ArtPris.ProfilNr) AND 
           ELogg.Endringstype   = 1) THEN 
      DO:
        CREATE Elogg.
        ASSIGN ELogg.TabellNavn     = "ArtPris"
               ELogg.EksterntSystem = "POS"   
               ELogg.Verdier        = STRING(ArtPris.ArtikkelNr) + CHR(1) + string(ArtPris.ProfilNr)
               ELogg.EndringsType   = 1
               ELogg.Behandlet      = FALSE NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DELETE ELogg.
      END.
    END.     
    IF AVAILABLE ELogg THEN RELEASE ELogg.
  END.
END.

 


