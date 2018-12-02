TRIGGER PROCEDURE FOR DELETE OF KassererValuta.
FIND ELogg WHERE 
     ELogg.TabellNavn     = "KassererValuta" AND
     ELogg.EksterntSystem = "HK"    AND
     ELogg.Verdier        = STRING(KassererValuta.ButikkNr) + CHR(1) + STRING(KassererValuta.KasseNr) + CHR(1) + STRING(KassererValuta.Dato) + CHR(1) + STRING(KassererValuta.KassererNr) + CHR(1) + STRING(KassererValuta.z_nummer) + CHR(1) + STRING(KassererValuta.ValKod) NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "KassererValuta"
           ELogg.EksterntSystem = "HK"   
           ELogg.Verdier        = STRING(KassererValuta.ButikkNr) + CHR(1) + STRING(KassererValuta.KasseNr) + CHR(1) + STRING(KassererValuta.Dato) + CHR(1) + STRING(KassererValuta.KassererNr) + CHR(1) + STRING(KassererValuta.z_nummer) + CHR(1) + STRING(KassererValuta.ValKod).
END.
ASSIGN ELogg.EndringsType = 3
       ELogg.Behandlet    = FALSE.
RELEASE ELogg.


