TRIGGER PROCEDURE FOR WRITE OF SkoTex.BestHode OLD BUFFER oldBestHode.

DEF BUFFER bufBestHode FOR BestHode.

assign
  SkoTex.BestHode.EDato    = today
  SkoTex.BestHode.ETid     = time
  SkoTex.BestHode.BrukerID = userid("skotex")
  SkoTex.BestHode.OrdreNr  = (if SkoTex.BestHode.OrdreNr = ?
                                then 0
                                else SkoTex.BestHode.OrdreNr).

/* Setter ordren som delhvis levert. I ordretrigger sjekker det om */
/* om det ligger flere ordre som ikke er levert.                   */                                     
IF oldBestHode.BestStat <> BestHode.BestStat AND
    (BestHode.BestStat > 4 AND BestHode.OrdreNr > 0) THEN
    RUN ordre_sjekk_levert.p (INPUT BestHode.OrdreNr).

/* Trigger sjekk av ordre. Om den skal settes som bekreftet. */
IF oldBestHode.BekreftetOrdre <> BestHode.BekreftetOrdre THEN
  RUN ordre_sjekk_bekreft.p (INPUT BestHode.OrdreNr).

/*
MESSAGE 
program-name(1) 
"Gurre var i BestHodeTrigger" 
VIEW-AS ALERT-BOX INFO BUTTONS OK.
*/


