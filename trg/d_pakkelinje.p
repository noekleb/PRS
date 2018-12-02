TRIGGER PROCEDURE FOR DELETE OF PakkeLinje.

FIND ELogg WHERE 
     ELogg.TabellNavn     = "Pakkelinje" AND
     ELogg.EksterntSystem = "POS"    AND
     ELogg.Verdier        = STRING(Pakkelinje.PakkeNr) + CHR(1) + string(Pakkelinje.StrKode) NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "Pakkelinje"
           ELogg.EksterntSystem = "POS"   
           ELogg.Verdier        = STRING(Pakkelinje.PakkeNr) + CHR(1) + string(Pakkelinje.StrKode).
END.
ASSIGN ELogg.EndringsType = 3
       ELogg.Behandlet    = FALSE.
RELEASE ELogg.


