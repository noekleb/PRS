TRIGGER PROCEDURE FOR DELETE OF dags_rap.
FIND ELogg WHERE 
     ELogg.TabellNavn     = "dags_rap" AND
     ELogg.EksterntSystem = "HK"    AND
     ELogg.Verdier        = STRING(dags_rap.butikk) + CHR(1) + STRING(dags_rap.dato) NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "dags_rap"
           ELogg.EksterntSystem = "HK"   
           ELogg.Verdier        = STRING(dags_rap.butikk) + CHR(1) + STRING(dags_rap.dato).
END.
ASSIGN ELogg.EndringsType = 3
       ELogg.Behandlet    = FALSE.
RELEASE ELogg.


