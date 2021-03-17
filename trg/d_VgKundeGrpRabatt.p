TRIGGER PROCEDURE FOR DELETE OF VgKundeGrpRabatt.

FIND ELogg WHERE 
     ELogg.TabellNavn     = "VarGr" AND
     ELogg.EksterntSystem = "POS"    AND
     ELogg.Verdier        = STRING(VgKundeGrpRabatt.Vg) NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "VarGr"
           ELogg.EksterntSystem = "POS"   
           ELogg.Verdier        = STRING(VgKundeGrpRabatt.Vg).
END.
ASSIGN ELogg.EndringsType = 3
       ELogg.Behandlet    = FALSE.
RELEASE ELogg.


