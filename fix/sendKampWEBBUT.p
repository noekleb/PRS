FIND KampanjeHode WHERE KampanjeHode.KampanjeId = 54 NO-LOCK.
IF NOT AVAIL KampanjeHode THEN
    RETURN.

FOR EACH KampanjeLinje OF KampanjeHode no-lock:
    FIND ELogg WHERE 
         ELogg.TabellNavn     = "ArtBas" AND
         ELogg.EksterntSystem = "WEBBUT"    AND
         ELogg.Verdier        = STRING(KampanjeLinje.ArtikkelNr) NO-ERROR NO-WAIT.
        IF NOT LOCKED ELogg THEN DO:
            IF NOT AVAIL ELogg THEN DO:
                CREATE ELogg.
                ASSIGN ELogg.TabellNavn     = "ArtBas"
                       ELogg.EksterntSystem = "WEBBUT"   
                       ELogg.Verdier        = STRING(KampanjeLinje.ArtikkelNr).
            END.
            ASSIGN ELogg.EndringsType = 1 
                   ELogg.Behandlet    = FALSE.
            RELEASE ELogg.
        END.
END.

