TRIGGER PROCEDURE FOR WRITE OF KjedensButikker.

{trg\c_w_trg.i &Fil=SkoTex.KjedensButikker &TYPE=W}

FIND ELogg WHERE 
     ELogg.TabellNavn     = "KjedensButikker" AND
     ELogg.EksterntSystem = "POS"    AND
     ELogg.Verdier        = STRING(KjedensButikker.KjedeNr) + "," + STRING(KjedensButikker.RegionNr)
                           + "," + STRING(KjedensButikker.DistriktNr) + "," + STRING(KjedensButikker.ButikkNr) NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "KjedensButikker"
           ELogg.EksterntSystem = "POS"   
           ELogg.Verdier        = STRING(KjedensButikker.KjedeNr) + "," + STRING(KjedensButikker.RegionNr)
                                  + "," + STRING(KjedensButikker.DistriktNr) + "," + STRING(KjedensButikker.ButikkNr).
END.
ASSIGN ELogg.EndringsType = IF KjedensButikker.UtmeldtDato <> ? THEN 3 ELSE 1
       ELogg.Behandlet    = FALSE.
RELEASE ELogg.


