TRIGGER PROCEDURE FOR DELETE OF KOrdreLinje.

DEFINE BUFFER trgEkstEDBSystem FOR EkstEDBSystem.
DEFINE BUFFER trgKOrdreHode    FOR KordreHode.

FIND trgKORdreHode OF KOrdreLinje NO-LOCK NO-ERROR.


/* Logger for eksport til Nettbutikk. */  
/*
IF AVAILABLE trgKordreHode AND trgKOrdreHode.Opphav = 10 AND
  INTEGER(trgKOrdreHode.LevStatus) > 30 THEN 
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
             ELogg.Verdier        = STRING(trgKOrdreHode.KOrdre_Id) NO-ERROR.
        IF NOT AVAIL Elogg THEN DO:
            CREATE Elogg.
            ASSIGN ELogg.TabellNavn     = "KOrdreHode"
                   ELogg.EksterntSystem = "WEBBUT"   
                   ELogg.Verdier        = STRING(trgKOrdreHode.KOrdre_Id).
        END.
        ELSE IF AVAILABLE ELogg THEN
        DO:
            ASSIGN ELogg.EndringsType = 1 /* Endringstype 1, ikke 3. Fordi ordren totalt sett bare er endret når en linje tas bort. */
                   ELogg.Behandlet    = FALSE.
            RELEASE ELogg.
        END.
    END. /* WEBBUTIKK */
  END. /* NETTBUTIKK */
*/



