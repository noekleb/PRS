TRIGGER PROCEDURE FOR WRITE OF Feilkode.

{trg\c_w_trg.i &Fil=SkoTex.FeilKode &Type="W"}

FIND ELogg WHERE 
     ELogg.TabellNavn     = "FeilKode" AND
     ELogg.EksterntSystem = "POS"    AND
     ELogg.Verdier        = STRING(FeilKode.FeilKode) NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "FeilKode"
           ELogg.EksterntSystem = "POS"   
           ELogg.Verdier        = STRING(FeilKode.FeilKode).
END.
ASSIGN ELogg.EndringsType = 1
       ELogg.Behandlet    = FALSE.
RELEASE ELogg.


