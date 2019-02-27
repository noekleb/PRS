TRIGGER PROCEDURE FOR WRITE OF StrKonv.

assign
  StrKonv.EDato    = today
  StrKonv.ETid     = time
  StrKonv.BrukerId = userid("skotex").

FIND ELogg WHERE 
     ELogg.TabellNavn     = "StrKonv" AND
     ELogg.EksterntSystem = "POS"    AND
     ELogg.Verdier        = STRING(StrKonv.StrKode) NO-ERROR.
IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "StrKonv"
           ELogg.EksterntSystem = "POS"   
           ELogg.Verdier        = STRING(StrKonv.StrKode).
END.
ASSIGN ELogg.EndringsType = 1
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
         ELogg.TabellNavn     = "StrKonv" AND
         ELogg.EksterntSystem = "WEBBUT"    AND
         ELogg.Verdier        = STRING(StrKonv.StrKode) NO-ERROR.
    IF NOT AVAIL Elogg THEN DO:
        CREATE Elogg.
        ASSIGN ELogg.TabellNavn     = "StrKonv"
               ELogg.EksterntSystem = "WEBBUT"   
               ELogg.Verdier        = STRING(StrKonv.StrKode).
    END.
    ASSIGN ELogg.EndringsType = 1 
           ELogg.Behandlet    = FALSE.
    RELEASE ELogg.
END. /* WEBBUTIKK */


