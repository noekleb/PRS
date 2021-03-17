TRIGGER PROCEDURE FOR DELETE OF Kort_Spes.
FIND ELogg WHERE 
     ELogg.TabellNavn     = "Kort_Spes" AND
     ELogg.EksterntSystem = "HK"    AND
     ELogg.Verdier        = STRING(Kort_Spes.dato) + CHR(1) + STRING(Kort_Spes.butikk) + CHR(1) + STRING(Kort_Spes.kasse) + CHR(1) + STRING(Kort_Spes.z_nummer) + CHR(1) + STRING(Kort_Spes.KortType) NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "Kort_Spes"
           ELogg.EksterntSystem = "HK"   
           ELogg.Verdier        = STRING(Kort_Spes.dato) + CHR(1) + STRING(Kort_Spes.butikk) + CHR(1) + STRING(Kort_Spes.kasse) + CHR(1) + STRING(Kort_Spes.z_nummer) + CHR(1) + STRING(Kort_Spes.KortType).
END.
ASSIGN ELogg.EndringsType = 3
       ELogg.Behandlet    = FALSE.
RELEASE ELogg.


