TRIGGER PROCEDURE FOR DELETE OF butikkSelger.

FIND ELogg WHERE 
     ELogg.TabellNavn     = "butikkSelger" AND
     ELogg.EksterntSystem = "POS"    AND
     ELogg.Verdier        = STRING(butikkSelger.Butik) + CHR(1) + STRING(butikkSelger.SelgerId) NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "butikkSelger"
           ELogg.EksterntSystem = "POS"   
           ELogg.Verdier        = STRING(butikkSelger.Butik) + CHR(1) + STRING(butikkSelger.SelgerId).
END.
ASSIGN ELogg.EndringsType = 3
       ELogg.Behandlet    = FALSE.
RELEASE ELogg.


