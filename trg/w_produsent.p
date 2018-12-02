TRIGGER PROCEDURE FOR WRITE OF Produsent.

DEFINE VARIABLE bHK    AS LOG NO-UNDO.
DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.

{trg\c_w_trg.i &Fil=SkoTex.Produsent &TYPE=W}

{syspara.i 1 1 18 cTekst}
IF CAN-DO('1,J,Ja,y,yes,true',cTekst)
THEN bHK = TRUE.
ELSE bHK = FALSE.

IF bHK THEN DO:
  FIND ELogg WHERE 
       ELogg.TabellNavn     = "Produsent" AND
       ELogg.EksterntSystem = "POS"    AND
       ELogg.Verdier        = STRING(Produsent.ProdNr) NO-ERROR.
  IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "Produsent"
           ELogg.EksterntSystem = "POS"   
           ELogg.Verdier        = STRING(Produsent.ProdNr).
  END.
  ASSIGN ELogg.EndringsType = 1
         ELogg.Behandlet    = FALSE.
  RELEASE ELogg.
END.

DEF BUFFER trgEkstEDBSystem FOR EkstEDBSystem.
    FIND FIRST trgEkstEDBSystem WHERE 
        trgEkstEDBSystem.DataType = "WEBBUT" AND 
        trgEkstEDBSystem.Aktiv = TRUE NO-LOCK NO-ERROR.
IF AVAILABLE trgEkstEDBSystem THEN
WEBBUTIKK:
DO:
    FIND ELogg WHERE 
         ELogg.TabellNavn     = "Produsent" AND
         ELogg.EksterntSystem = "WEBBUT"    AND
         ELogg.Verdier        = STRING(Produsent.ProdNr) NO-ERROR.
    IF NOT AVAIL Elogg THEN DO:
        CREATE Elogg.
        ASSIGN ELogg.TabellNavn     = "Produsent"
               ELogg.EksterntSystem = "WEBBUT"   
               ELogg.Verdier        = STRING(Produsent.ProdNr).
    END.
    ASSIGN ELogg.EndringsType = 1 
           ELogg.Behandlet    = FALSE.
    RELEASE ELogg.
END. /* WEBBUTIKK */


