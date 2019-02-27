TRIGGER PROCEDURE FOR DELETE OF kas_rap.
FIND ELogg WHERE 
     ELogg.TabellNavn     = "kas_rap" AND
     ELogg.EksterntSystem = "HK"    AND
     ELogg.Verdier        = STRING(kas_rap.dato) + CHR(1) + STRING(kas_rap.butikk) + CHR(1) + STRING(kas_rap.kasse) + CHR(1) + STRING(kas_rap.z_nummer) NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "kas_rap"
           ELogg.EksterntSystem = "HK"   
           ELogg.Verdier        = STRING(kas_rap.dato) + CHR(1) + STRING(kas_rap.butikk) + CHR(1) + STRING(kas_rap.kasse) + CHR(1) + STRING(kas_rap.z_nummer).
END.
ASSIGN ELogg.EndringsType = 3
       ELogg.Behandlet    = FALSE.
RELEASE ELogg.



