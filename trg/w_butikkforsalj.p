TRIGGER PROCEDURE FOR WRITE OF butikkforsalj OLD BUFFER oldbutikkforsalj.

{trg\c_w_trg.i &Fil=SkoTex.butikkforsalj &Type=W}
/* om vi har byter kassererid måste vi göra en delete på den gamla */
IF oldButikkForsalj.KassererId <> 0 AND oldButikkForsalj.KassererId <> ButikkForsalj.KassererId THEN DO:
    FIND ELogg WHERE 
         ELogg.TabellNavn     = "ButikkForsalj" AND
         ELogg.EksterntSystem = "POS"    AND
         ELogg.Verdier        = STRING(oldButikkForsalj.Butik) + CHR(1) + STRING(oldButikkForsalj.KassererId) NO-ERROR.
    IF NOT AVAIL Elogg THEN DO:
        CREATE Elogg.
        ASSIGN ELogg.TabellNavn     = "ButikkForsalj"
               ELogg.EksterntSystem = "POS"   
               ELogg.Verdier        = STRING(oldButikkForsalj.Butik) + CHR(1) + STRING(oldButikkForsalj.KassererId).
    END.
    ASSIGN ELogg.EndringsType = 3
           ELogg.Behandlet    = FALSE.
END.
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
ASSIGN ELogg.EndringsType = 1
       ELogg.Behandlet    = FALSE.
RELEASE ELogg.


