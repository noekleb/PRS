TRIGGER PROCEDURE FOR WRITE OF ArtLag.

/* {trg\c_w_trg.i &Fil=ArtLag &TYPE=W} */

DEF BUFFER trgEkstEDBSystem FOR EkstEDBSystem.
DEF BUFFER trgArtBas        FOR ArtBas.

ASSIGN 
    ArtLag.EndretDatoTid = NOW
    .

FIND trgArtBas NO-LOCK WHERE
    trgArtBas.ArtikkelNr = ArtLag.ArtikkelNr NO-ERROR. 

IF AVAILABLE trgArtBas AND trgArtBas.WebButikkArtikkel THEN 
DO:
  FIND FIRST trgEkstEDBSystem WHERE 
    trgEkstEDBSystem.DataType = "WEBBUT" AND 
    trgEkstEDBSystem.Aktiv = TRUE NO-LOCK NO-ERROR.
  IF AVAILABLE trgEkstEDBSystem THEN
  WEBBUTIKK:
  DO:
    IF NOT CAN-FIND(FIRST ELogg WHERE 
         ELogg.TabellNavn     = "Lager" AND
         ELogg.EksterntSystem = "WEBBUT"    AND
         ELogg.Verdier        = STRING(ArtLag.ArtikkelNr)
                                + chr(1) + string(ArtLag.butik) AND 
         ELogg.EndringsType   = 1) THEN 
    DO:
        CREATE Elogg.
        ASSIGN ELogg.TabellNavn     = "Lager"
               ELogg.EksterntSystem = "WEBBUT"   
               ELogg.Verdier        = STRING(ArtLag.ArtikkelNr)
                                + chr(1) + string(ArtLag.butik)
               ELogg.EndringsType = 1 
               ELogg.Behandlet    = FALSE NO-ERROR.
        IF ERROR-STATUS:ERROR THEN 
          DELETE ELogg.
        ELSE DO:
          RELEASE ELogg.
        END.
    END.
  END. /* WEBBUTIKK */
END.

RUN cls\GoogleMerchant\logglagerendringGoogleMerchant.p (ArtLag.Butik, Artlag.ArtikkelNr, ArtLag.StrKode). 


