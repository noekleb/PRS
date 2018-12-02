TRIGGER PROCEDURE FOR DELETE OF Medlem.

IF Medlem.Kundenr <> 0 THEN DO:
    FIND ELogg WHERE 
         ELogg.TabellNavn     = "Kunde" AND
         ELogg.EksterntSystem = "POS"    AND
         ELogg.Verdier        = STRING(Medlem.KundeNr) NO-ERROR.
    IF NOT AVAIL Elogg THEN DO:
        CREATE Elogg.
        ASSIGN ELogg.TabellNavn     = "Kunde"
               ELogg.EksterntSystem = "POS"   
               ELogg.Verdier        = STRING(Medlem.KundeNr).
    END.
    ASSIGN ELogg.EndringsType = 1
           ELogg.Behandlet    = FALSE.
END.
RELEASE ELogg.
