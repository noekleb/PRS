TRIGGER PROCEDURE FOR DELETE OF Garanti.

FIND ELogg WHERE 
     ELogg.TabellNavn     = "Garanti" AND
     ELogg.EksterntSystem = "POS"    AND
     ELogg.Verdier        = STRING(Garanti.Garantikl) NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "Garanti"
           ELogg.EksterntSystem = "POS"   
           ELogg.Verdier        = STRING(Garanti.Garantikl).
END.
ASSIGN ELogg.EndringsType = 3
       ELogg.Behandlet    = FALSE.
RELEASE ELogg.



