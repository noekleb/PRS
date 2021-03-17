TRIGGER PROCEDURE FOR DELETE OF Ordre.

/* ERP system */
DO:
    FIND ELogg WHERE 
         ELogg.TabellNavn     = "Ordre" AND
         ELogg.EksterntSystem = "ERP"    AND
         ELogg.Verdier        = STRING(Ordre.OrdreNr) NO-ERROR.
    IF NOT AVAIL Elogg THEN DO:
        CREATE Elogg.
        ASSIGN ELogg.TabellNavn     = "Ordre"
               ELogg.EksterntSystem = "ERP"   
               ELogg.Verdier        = STRING(Ordre.OrdreNr).
    END.
    ASSIGN ELogg.EndringsType = 3
           ELogg.Behandlet    = FALSE.
    RELEASE ELogg.
END.


