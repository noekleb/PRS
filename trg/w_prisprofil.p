TRIGGER PROCEDURE FOR WRITE OF SkoTex.Prisprofil.

assign
  SkoTex.Prisprofil.EDato    = today
  SkoTex.Prisprofil.ETid     = time
  SkoTex.Prisprofil.BrukerId = userid("skotex").
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
ASSIGN ELogg.EndringsType = 1
       ELogg.Behandlet    = FALSE.
RELEASE ELogg.


