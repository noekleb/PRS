TRIGGER PROCEDURE FOR DELETE OF Material.

FIND ELogg WHERE 
     ELogg.TabellNavn     = "Material" AND
     ELogg.EksterntSystem = "POS"    AND
     ELogg.Verdier        = STRING(Material.MatKod) NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "Material"
           ELogg.EksterntSystem = "POS"   
           ELogg.Verdier        = STRING(Material.MatKod).
END.
ASSIGN ELogg.EndringsType = 3
       ELogg.Behandlet    = FALSE.
RELEASE ELogg.

DEF BUFFER trgEkstEDBSystem FOR EkstEDBSystem.
    FIND FIRST trgEkstEDBSystem WHERE 
        trgEkstEDBSystem.DataType = "WEBBUT" AND 
        trgEkstEDBSystem.Aktiv = TRUE NO-LOCK NO-ERROR.
IF AVAILABLE trgEkstEDBSystem THEN
WEBBUTIKK:
DO:
    FIND ELogg WHERE 
         ELogg.TabellNavn     = "Material" AND
         ELogg.EksterntSystem = "WEBBUT"    AND
         ELogg.Verdier        = STRING(Material.MatKod) NO-ERROR.
    IF NOT AVAIL Elogg THEN DO:
        CREATE Elogg.
        ASSIGN ELogg.TabellNavn     = "Material"
               ELogg.EksterntSystem = "WEBBUT"   
               ELogg.Verdier        = STRING(Material.MatKod).
    END.
    ASSIGN ELogg.EndringsType = 3 
           ELogg.Behandlet    = FALSE.
    RELEASE ELogg.
END. /* WEBBUTIKK */


