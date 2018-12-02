TRIGGER PROCEDURE FOR WRITE OF Kravkode.

{trg\c_w_trg.i &Fil=SkoTex.Kravkode &Type="w"}

FIND ELogg WHERE 
     ELogg.TabellNavn     = "KravKode" AND
     ELogg.EksterntSystem = "POS"    AND
     ELogg.Verdier        = STRING(KravKode.KravKode) NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "KravKode"
           ELogg.EksterntSystem = "POS"   
           ELogg.Verdier        = STRING(KravKode.KravKode).
END.
ASSIGN ELogg.EndringsType = 1
       ELogg.Behandlet    = FALSE.
RELEASE ELogg.


