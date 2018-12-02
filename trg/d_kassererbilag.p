TRIGGER PROCEDURE FOR DELETE OF KassererBilag.
FIND ELogg WHERE 
     ELogg.TabellNavn     = "KassererBilag" AND
     ELogg.EksterntSystem = "HK"    AND
     ELogg.Verdier        = STRING(KassererBilag.ButikkNr) + CHR(1) + STRING(KassererBilag.Dato) + CHR(1) + STRING(KassererBilag.KassererNr) + CHR(1) + STRING(KassererBilag.z_nummer) + CHR(1) + STRING(KassererBilag.BilagsNr) NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "KassererBilag"
           ELogg.EksterntSystem = "HK"   
           ELogg.Verdier        = STRING(KassererBilag.ButikkNr) + CHR(1) + STRING(KassererBilag.Dato) + CHR(1) + STRING(KassererBilag.KassererNr) + CHR(1) + STRING(KassererBilag.z_nummer) + CHR(1) + STRING(KassererBilag.BilagsNr).
END.
ASSIGN ELogg.EndringsType = 3
       ELogg.Behandlet    = FALSE.

RELEASE ELogg.


