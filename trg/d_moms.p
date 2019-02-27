TRIGGER PROCEDURE FOR DELETE OF Moms.

FIND ELogg WHERE 
     ELogg.TabellNavn     = "Moms" AND
     ELogg.EksterntSystem = "POS"    AND
     ELogg.Verdier        = STRING(Moms.MomsKod) NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "Moms"
           ELogg.EksterntSystem = "POS"   
           ELogg.Verdier        = STRING(Moms.MomsKod).
END.
ASSIGN ELogg.EndringsType = 3
       ELogg.Behandlet    = FALSE.
RELEASE ELogg.


