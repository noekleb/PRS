TRIGGER PROCEDURE FOR WRITE OF ProduktFamilie.

{trg\c_w_trg.i &Fil=SkoTex.ProduktFamilie &TYPE=W}

FIND ELogg WHERE 
     ELogg.TabellNavn     = "ProduktFamilie" AND
     ELogg.EksterntSystem = "POS"    AND
     ELogg.Verdier        = STRING(ProduktFamilie.ProdFamId) NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "ProduktFamilie"
           ELogg.EksterntSystem = "POS"   
           ELogg.Verdier        = STRING(ProduktFamilie.ProdFamId)
           ELogg.Endringstype   = 1.
END.

