TRIGGER PROCEDURE FOR DELETE OF butikkforsalj.

FIND ELogg WHERE 
     ELogg.TabellNavn     = "ButikkForsalj" AND
     ELogg.EksterntSystem = "POS"    AND
     ELogg.Verdier        = STRING(ButikkForsalj.Butik) + CHR(1) + STRING(ButikkForsalj.KassererId) NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "ButikkForsalj"
           ELogg.EksterntSystem = "POS"   
           ELogg.Verdier        = STRING(ButikkForsalj.Butik) + CHR(1) + STRING(ButikkForsalj.KassererId).
END.
ASSIGN ELogg.EndringsType = 3
       ELogg.Behandlet    = FALSE.
RELEASE ELogg.


