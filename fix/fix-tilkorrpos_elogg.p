FOR EACH ArtBas NO-LOCK WHERE
    ArtBas.ArtikkelNr < 8500000 AND
    ArtBas.LEvNr = 10:

    FIND ELogg WHERE 
         ELogg.TabellNavn     = "ArtBas" AND
         ELogg.EksterntSystem = "TILKORRPOS"    AND
         ELogg.Verdier        = STRING(ArtBas.ArtikkelNr) NO-ERROR.
    IF NOT AVAIL Elogg THEN DO:
        CREATE Elogg.
        ASSIGN ELogg.TabellNavn     = "ArtBas"
               ELogg.EksterntSystem = "TILKORRPOS"   
               ELogg.Verdier        = STRING(ArtBas.ArtikkelNr).
    END.
    ASSIGN ELogg.EndringsType = 1
           ELogg.Behandlet    = FALSE.
    RELEASE ELogg.
END.
