TRIGGER PROCEDURE FOR WRITE OF LevKontakt.

{trg\c_w_trg.i &Fil="SkoTex.Levkontakt" &Type="W"}

FIND ELogg WHERE 
     ELogg.TabellNavn     = "Levbas" AND
     ELogg.EksterntSystem = "POS"    AND
     ELogg.Verdier        = STRING(Levkontakt.LevNr) NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "Levbas"
           ELogg.EksterntSystem = "POS"   
           ELogg.Verdier        = STRING(Levkontakt.LevNr).
END.
ASSIGN ELogg.EndringsType = 1
       ELogg.Behandlet    = FALSE.
RELEASE ELogg.


