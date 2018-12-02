TRIGGER PROCEDURE FOR WRITE OF SkoTex.LevBas.
    
DEF VAR piLevNr AS INT NO-UNDO.

/* Setter høyeste levnr som skal eksporteres. */
{syspara.i 16 2 3 piLevNr INT}
IF piLevNr = 0 THEN
    piLevNr = 999.

IF LevBas.LevNr <= piLevNr THEN
DO:
    assign
      SkoTex.LevBas.EDato    = today
      SkoTex.LevBas.ETid     = time
      SkoTex.LevBas.BrukerID = userid("skotex").
    FIND ELogg WHERE 
         ELogg.TabellNavn     = "LevBas" AND
         ELogg.EksterntSystem = "POS"    AND
         ELogg.Verdier        = STRING(LevBas.LevNr) NO-ERROR.
    IF NOT AVAIL Elogg THEN DO:
        CREATE Elogg.
        ASSIGN ELogg.TabellNavn     = "LevBas"
               ELogg.EksterntSystem = "POS"   
               ELogg.Verdier        = STRING(LevBas.LevNr).
    END.
    ASSIGN ELogg.EndringsType = 1
           ELogg.Behandlet    = FALSE.
END.
    
RELEASE ELogg.

DEF BUFFER trgEkstEDBSystem FOR EkstEDBSystem.
    FIND FIRST trgEkstEDBSystem WHERE 
        trgEkstEDBSystem.DataType = "WEBBUT" AND 
        trgEkstEDBSystem.Aktiv = TRUE NO-LOCK NO-ERROR.
IF AVAILABLE trgEkstEDBSystem THEN
WEBBUTIKK:
DO:
    FIND ELogg WHERE 
         ELogg.TabellNavn     = "LevBas" AND
         ELogg.EksterntSystem = "WEBBUT"    AND
         ELogg.Verdier        = STRING(LevBas.LevNr) NO-ERROR.
    IF NOT AVAIL Elogg THEN DO:
        CREATE Elogg.
        ASSIGN ELogg.TabellNavn     = "LevBas"
               ELogg.EksterntSystem = "WEBBUT"   
               ELogg.Verdier        = STRING(LevBas.LevNr).
    END.
    ASSIGN ELogg.EndringsType = 1 
           ELogg.Behandlet    = FALSE.
    RELEASE ELogg.
END. /* WEBBUTIKK */


