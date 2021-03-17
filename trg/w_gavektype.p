TRIGGER PROCEDURE FOR WRITE OF GaveKType.

{trg\c_w_trg.i &Fil=SkoTex.GaveKType &Type="W"}

FIND ELogg WHERE 
     ELogg.TabellNavn     = "GaveKType" AND
     ELogg.EksterntSystem = "POS"    AND
     ELogg.Verdier        = STRING(GaveKType.IdentType) NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "GaveKType"
           ELogg.EksterntSystem = "POS"   
           ELogg.Verdier        = STRING(GaveKType.IdentType).
END.
ASSIGN ELogg.EndringsType = 1
           ELogg.Behandlet    = FALSE.
RELEASE ELogg.


