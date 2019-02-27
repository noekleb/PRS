TRIGGER PROCEDURE FOR DELETE OF KassererKontanter.
FIND ELogg WHERE 
     ELogg.TabellNavn     = "KassererKontanter" AND
     ELogg.EksterntSystem = "HK"    AND
     ELogg.Verdier        = STRING(KassererKontanter.ButikkNr) + CHR(1) + STRING(KassererKontanter.Dato) + CHR(1) + STRING(KassererKontanter.KassererNr) + CHR(1) + STRING(KassererKontanter.z_nummer) NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "KassererKontanter"
           ELogg.EksterntSystem = "HK"   
           ELogg.Verdier        = STRING(KassererKontanter.ButikkNr) + CHR(1) + STRING(KassererKontanter.Dato) + CHR(1) + STRING(KassererKontanter.KassererNr) + CHR(1) + STRING(KassererKontanter.z_nummer).
END.
ASSIGN ELogg.EndringsType = 3
       ELogg.Behandlet    = FALSE.

RELEASE ELogg.


