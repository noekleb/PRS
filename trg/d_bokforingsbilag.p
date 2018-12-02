TRIGGER PROCEDURE FOR DELETE OF Bokforingsbilag.
FIND ELogg WHERE 
     ELogg.TabellNavn     = "Bokforingsbilag" AND
     ELogg.EksterntSystem = "HK"    AND
     ELogg.Verdier        = STRING(BokforingsBilag.ButikkNr) + CHR(1) + STRING(BokforingsBilag.Aar) + CHR(1) + STRING(BokforingsBilag.BokforingsNr) NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "Bokforingsbilag"
           ELogg.EksterntSystem = "HK"   
           ELogg.Verdier        = STRING(BokforingsBilag.ButikkNr) + CHR(1) + STRING(BokforingsBilag.Aar) + CHR(1) + STRING(BokforingsBilag.BokforingsNr).
END.
ASSIGN ELogg.EndringsType = 3
       ELogg.Behandlet    = FALSE.
RELEASE ELogg.


