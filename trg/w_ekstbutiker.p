TRIGGER PROCEDURE FOR WRITE OF SkoTex.ekstbutiker.

assign
  SkoTex.ekstbutiker.EDato = today
  SkoTex.ekstbutiker.ETid  = time
  SkoTex.ekstbutiker.BrukerId = userid("skotex").
FIND ELogg WHERE 
     ELogg.TabellNavn     = "ekstbutiker" AND
     ELogg.EksterntSystem = "POS"    AND
     ELogg.Verdier        = STRING(ekstbutiker.Butik) NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "ekstbutiker"
           ELogg.EksterntSystem = "POS"   
           ELogg.Verdier        = STRING(ekstbutiker.Butik).
END.
ASSIGN ELogg.EndringsType = 1
       ELogg.Behandlet    = FALSE.
RELEASE ELogg.


