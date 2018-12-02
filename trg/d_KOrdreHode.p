TRIGGER PROCEDURE FOR DELETE OF KOrdreHode.

DEFINE BUFFER trgEkstEDBSystem FOR EkstEDBSystem.


FOR EACH KOrdreLinje OF KOrdreHode:
  DELETE KORdreLinje.
END.

/*
/* Logger for eksport til Nettbutikk. */  
IF KOrdreHode.Opphav = 10 THEN 
  NETTBUTIKK:
  DO:
    FIND FIRST trgEkstEDBSystem WHERE 
        trgEkstEDBSystem.DataType = "WEBBUT" AND 
        trgEkstEDBSystem.Aktiv = TRUE NO-LOCK NO-ERROR.
    IF AVAILABLE trgEkstEDBSystem THEN
    WEBBUTIKK:
    DO:
        FIND ELogg WHERE 
             ELogg.TabellNavn     = "KOrdreHode" AND
             ELogg.EksterntSystem = "WEBBUT"    AND
             ELogg.Verdier        = STRING(KOrdreHode.KOrdre_Id) NO-ERROR.
        IF NOT AVAIL Elogg THEN DO:
            CREATE Elogg.
            ASSIGN ELogg.TabellNavn     = "KOrdreHode"
                   ELogg.EksterntSystem = "WEBBUT"   
                   ELogg.Verdier        = STRING(KOrdreHode.KOrdre_Id).
        END.
        ELSE IF AVAILABLE ELogg THEN
        DO:
            ASSIGN ELogg.EndringsType = 3 
                   ELogg.Behandlet    = FALSE.
            RELEASE ELogg.
        END.
    END. /* WEBBUTIKK */
  END. /* NETTBUTIKK */  
*/
  
