TRIGGER PROCEDURE FOR WRITE OF VarebehType.

{trg\c_w_trg.i &Fil=SkoTex.VarebehType &TYPE=W}

FIND ELogg WHERE 
     ELogg.TabellNavn     = "VarebehType" AND
     ELogg.EksterntSystem = "POS"    AND
     ELogg.Verdier        = STRING(VarebehType.VarebehType) NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "VarebehType"
           ELogg.EksterntSystem = "POS"   
           ELogg.Verdier        = STRING(VarebehType.VarebehType).
END.
ASSIGN ELogg.EndringsType = 1
       ELogg.Behandlet    = FALSE.
RELEASE ELogg.


