TRIGGER PROCEDURE FOR WRITE OF SkoTex.Butiker.

assign
  SkoTex.Butiker.EDato = today
  SkoTex.Butiker.ETid  = time
  SkoTex.Butiker.BrukerId = userid("skotex").
FIND ELogg WHERE 
     ELogg.TabellNavn     = "Butiker" AND
     ELogg.EksterntSystem = "POS"    AND
     ELogg.Verdier        = STRING(Butiker.Butik) NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "Butiker"
           ELogg.EksterntSystem = "POS"   
           ELogg.Verdier        = STRING(Butiker.Butik).
END.
ASSIGN ELogg.EndringsType = IF Butiker.harButikksystem = TRUE THEN 1 ELSE 3
       ELogg.Behandlet    = FALSE.
RELEASE ELogg.


