TRIGGER PROCEDURE FOR DELETE OF StrKonv.


FIND ELogg WHERE 
     ELogg.TabellNavn     = "StrKonv" AND
     ELogg.EksterntSystem = "POS"    AND
     ELogg.Verdier        = STRING(StrKonv.StrKode) NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "StrKonv"
           ELogg.EksterntSystem = "POS"   
           ELogg.Verdier        = STRING(StrKonv.StrKode).
END.
ASSIGN ELogg.EndringsType = 3
       ELogg.Behandlet    = FALSE.
RELEASE ELogg.


