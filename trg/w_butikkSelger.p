TRIGGER PROCEDURE FOR WRITE OF butikkSelger OLD BUFFER oldbutikkSelger.

{trg\c_w_trg.i &Fil=SkoTex.butikkSelger &Type=W}
IF oldbutikkSelger.SelgerId <> 0 AND oldbutikkSelger.SelgerId <> ButikkSelger.SelgerId THEN DO:
    FIND ELogg WHERE 
         ELogg.TabellNavn     = "butikkSelger" AND
         ELogg.EksterntSystem = "POS"    AND
         ELogg.Verdier        = STRING(oldbutikkSelger.Butik) + CHR(1) + STRING(oldbutikkSelger.SelgerId) NO-ERROR.
    IF NOT AVAIL ELogg THEN DO:
        CREATE Elogg.
        ASSIGN ELogg.TabellNavn     = "butikkSelger"
               ELogg.EksterntSystem = "POS"   
               ELogg.Verdier        = STRING(oldbutikkSelger.Butik) + CHR(1) + STRING(oldbutikkSelger.SelgerId).
    END.
    ASSIGN ELogg.EndringsType = 3
           ELogg.Behandlet    = FALSE.
END.
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
ASSIGN ELogg.EndringsType = 1
       ELogg.Behandlet    = FALSE.
RELEASE ELogg.


