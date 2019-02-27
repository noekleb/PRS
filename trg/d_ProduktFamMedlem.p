TRIGGER PROCEDURE FOR DELETE OF ProduktFamMedlem.

/* Ved sletting av medlem, flagger vi familien edret. */
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



