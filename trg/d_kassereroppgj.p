TRIGGER PROCEDURE FOR DELETE OF KassererOppgj.
FIND ELogg WHERE 
     ELogg.TabellNavn     = "KassererOppgj" AND
     ELogg.EksterntSystem = "HK"    AND
     ELogg.Verdier        = STRING(kassererOppgj.ButikkNr) + CHR(1) + STRING(kassererOppgj.Dato) + CHR(1) + STRING(kassererOppgj.KassererNr) + CHR(1) + STRING(kassererOppgj.z_nummer) NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "KassererOppgj"
           ELogg.EksterntSystem = "HK"   
           ELogg.Verdier        = STRING(kassererOppgj.ButikkNr) + CHR(1) + STRING(kassererOppgj.Dato) + CHR(1) + STRING(kassererOppgj.KassererNr) + CHR(1) + STRING(kassererOppgj.z_nummer).
END.
ASSIGN ELogg.EndringsType = 3
       ELogg.Behandlet    = FALSE.
RELEASE ELogg.


