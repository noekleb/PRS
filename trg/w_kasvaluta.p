TRIGGER PROCEDURE FOR WRITE OF KasValuta.

{c_w_trg.i &Type="W" &Fil="KasValuta"}

ASSIGN
    KasValuta.ValDatum = TODAY
    .

FIND ELogg WHERE 
     ELogg.TabellNavn     = "KasValuta" AND
     ELogg.EksterntSystem = "POS"    AND
     ELogg.Verdier        = "ALLE" NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "KasValuta"
           ELogg.EksterntSystem = "POS"   
           ELogg.Verdier        = "ALLE".
END.
ASSIGN ELogg.EndringsType = 1
       ELogg.Behandlet    = FALSE.
RELEASE ELogg.


