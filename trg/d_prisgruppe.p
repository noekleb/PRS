TRIGGER PROCEDURE FOR DELETE OF PrisGruppe.

FIND ELogg WHERE 
     ELogg.TabellNavn     = "PrisGruppe" AND
     ELogg.EksterntSystem = "POS"    AND
     ELogg.Verdier        = STRING(PrisGruppe.PrisGrpNr) NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "PrisGruppe"
           ELogg.EksterntSystem = "POS"   
           ELogg.Verdier        = STRING(PrisGruppe.PrisGrpNr).
END.
ASSIGN ELogg.EndringsType = 3
       ELogg.Behandlet    = FALSE.
RELEASE ELogg.


