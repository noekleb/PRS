TRIGGER PROCEDURE FOR WRITE OF KjedeDistrikt.

{trg\c_w_trg.i &Fil=SkoTex.Kjededistrikt &TYPE=W}

FIND ELogg WHERE 
     ELogg.TabellNavn     = "KjedeDistrikt" AND
     ELogg.EksterntSystem = "POS"    AND
     ELogg.Verdier        = STRING(KjedeDistrikt.KjedeNr) + "," + STRING(KjedeDistrikt.RegionNr)
                           + "," + STRING(KjedeDistrikt.DistriktNr) NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "KjedeDistrikt"
           ELogg.EksterntSystem = "POS"   
           ELogg.Verdier        = STRING(KjedeDistrikt.KjedeNr) + "," + STRING(KjedeDistrikt.RegionNr)
                                  + "," + STRING(KjedeDistrikt.DistriktNr).
END.
ASSIGN ELogg.EndringsType = 1
       ELogg.Behandlet    = FALSE.
RELEASE ELogg.


