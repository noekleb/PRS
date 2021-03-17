TRIGGER PROCEDURE FOR DELETE OF VgAkt.

FIND ELogg WHERE 
     ELogg.TabellNavn     = "VgAkt" AND
     ELogg.EksterntSystem = "POS"    AND
     ELogg.Verdier        = STRING(VgAkt.Vg) + CHR(1) + STRING(VgAkt.AktNr) NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "VgAkt"
           ELogg.EksterntSystem = "POS"   
           ELogg.Verdier        = STRING(VgAkt.Vg) + CHR(1) + STRING(VgAkt.AktNr).
END.
ASSIGN ELogg.EndringsType = 3
       ELogg.Behandlet    = FALSE.
RELEASE ELogg.


