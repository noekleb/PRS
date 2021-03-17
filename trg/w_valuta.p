TRIGGER PROCEDURE FOR WRITE OF SkoTex.Valuta.

assign
  SkoTex.Valuta.ValDatum = today
  SkoTex.Valuta.EDato    = today
  SkoTex.Valuta.ETid     = time
  SkoTex.Valuta.BrukerId = userid("skotex").
FIND ELogg WHERE 
     ELogg.TabellNavn     = "Valuta" AND
     ELogg.EksterntSystem = "POS"    AND
     ELogg.Verdier        = "ALLE" NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "Valuta"
           ELogg.EksterntSystem = "POS"   
           ELogg.Verdier        = "ALLE".
END.
ASSIGN ELogg.EndringsType = 1
       ELogg.Behandlet    = FALSE.
RELEASE ELogg.


