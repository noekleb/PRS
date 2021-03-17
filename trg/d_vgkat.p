TRIGGER PROCEDURE FOR DELETE OF VgKat.

FIND ELogg WHERE 
     ELogg.TabellNavn     = "VgKat" AND
     ELogg.EksterntSystem = "POS"    AND
     ELogg.Verdier        = STRING(VgKat.Vg) + CHR(1) + STRING(VgKat.VgKat)
                                             + CHR(1) + STRING(VgKat.KatNr) NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "VgKat"
           ELogg.EksterntSystem = "POS"   
           ELogg.Verdier        = STRING(VgKat.Vg) + CHR(1) + STRING(VgKat.VgKat)
                                             + CHR(1) + STRING(VgKat.KatNr).
END.
ASSIGN ELogg.EndringsType = 3
       ELogg.Behandlet    = FALSE.
RELEASE ELogg.


