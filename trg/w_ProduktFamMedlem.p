TRIGGER PROCEDURE FOR WRITE OF ProduktFamMedlem.

{trg\c_w_trg.i &Fil=SkoTex.ProduktFamMedlem &TYPE=W}

FIND ELogg WHERE 
     ELogg.TabellNavn     = "ProduktFamilie" AND
     ELogg.EksterntSystem = "POS"    AND
     ELogg.Verdier        = STRING(ProduktFamMedlem.ProdFamId) NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "ProduktFamilie"
           ELogg.EksterntSystem = "POS"   
           ELogg.Verdier        = STRING(ProduktFamMedlem.ProdFamId)
           ELogg.Endringstype   = 1.
END.

