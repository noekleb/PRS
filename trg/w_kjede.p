TRIGGER PROCEDURE FOR WRITE OF Kjede.

{c_w_trg.i &Type="W" &Fil="Kjede"}

FIND ELogg WHERE 
     ELogg.TabellNavn     = "Kjede" AND
     ELogg.EksterntSystem = "POS"    AND
     ELogg.Verdier        = STRING(Kjede.KjedeNr) NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "Kjede"
           ELogg.EksterntSystem = "POS"   
           ELogg.Verdier        = STRING(Kjede.KjedeNr).
END.
ASSIGN ELogg.EndringsType = 1
       ELogg.Behandlet    = FALSE.
RELEASE ELogg.


