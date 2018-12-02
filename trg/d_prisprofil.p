TRIGGER PROCEDURE FOR DELETE OF Prisprofil.

FIND ELogg WHERE 
     ELogg.TabellNavn     = "Prisprofil" AND
     ELogg.EksterntSystem = "POS"    AND
     ELogg.Verdier        = STRING(Prisprofil.ProfilNr) NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "Prisprofil"
           ELogg.EksterntSystem = "POS"   
           ELogg.Verdier        = STRING(Prisprofil.ProfilNr).
END.
ASSIGN ELogg.EndringsType = 3
       ELogg.Behandlet    = FALSE.
RELEASE ELogg.


