/* pgm för att sända ut suppleringsordre igen */
FOR EACH ordre WHERE varebehnr = 90000026 AND cl =  AND bekreftetdato <> ? NO-LOCK:
    FIND ELogg WHERE 
         ELogg.TabellNavn     = "OrdHK" AND
         ELogg.EksterntSystem = "POS"    AND
         ELogg.Verdier        = STRING(Ordre.OrdreNr) NO-ERROR.
    IF NOT AVAIL Elogg THEN DO:
        CREATE Elogg.
        ASSIGN ELogg.TabellNavn     = "OrdHK"
               ELogg.EksterntSystem = "POS"   
               ELogg.Verdier        = STRING(Ordre.OrdreNr).
    END.
    ASSIGN ELogg.EndringsType = 1
           ELogg.Behandlet    = FALSE.
    RELEASE ELogg.
END.
