TRIGGER PROCEDURE FOR DELETE OF ProduktFamilie.


FIND ELogg WHERE 
     ELogg.TabellNavn     = "ProduktFamilie" AND
     ELogg.EksterntSystem = "POS"    AND
     ELogg.Verdier        = STRING(ProduktFamilie.ProdFamId) NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "ProduktFamilie"
           ELogg.EksterntSystem = "POS"   
           ELogg.Verdier        = STRING(ProduktFamilie.ProdFamId)
           ELogg.Endringstype   = 3.
END.
