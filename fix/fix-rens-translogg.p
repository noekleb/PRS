
/* Kontroll av sletting av artikkler mot TransLogg */
/* fix-rens-translogg.p */

DEF VAR wStartTid AS INT NO-UNDO.
DEF VAR wAntLestUt AS INT NO-UNDO.
DEF VAR wAntPoster AS INT NO-UNDO.

DEF STREAM utLogg.
DEF STREAM utData.

OUTPUT STREAM utData TO VALUE("fixTransLogg.sdv").
OUTPUT stream utLogg TO VALUE("fixTrans.log").
  
PUT STREAM utLogg UNFORMATTED
  "------------------------------------------" SKIP
  "** Transaksjoner fra slettede artikkler **" SKIP
  "** Dato: " today "Tid: " STRING(TIME,"HH:MM:SS") SKIP
  "------------------------------------------" SKIP.             
             
ASSIGN
  wStartTid = TIME.                            
  
export STREAM utData DELIMITER ";"
 "BatchNr"
 "Butik"
 "TransNr"
 "ForsNr"
 "KundNr"
 "TTId"
 "TBId"
 "ArtikkelNr"
 "LevNr"
 "RegistrertDato"
 "RegistrertTid"
 "RegistrertAv"
 "BongId"
 "BongLinjeNr"
 "KassaNr"
 "Vg"
 "LopNr"
 "Storl"
 "Antall"
 "Pris"
 "RabKr"
 "Mva"
 "Plukket"
 "Dato"
 "Tid"
 "Postert"
 "PostertDato"
 "PostertTid"
 "BestNr"
 "OvButik"
 "OvTransNr"
 "SeqNr"
 "FeilKode"
 "TilStorl"
 "VVarekost"
 "SattVVareKost" skip.
                   
TRANSLOGG:              
FOR EACH TransLogg EXCLUSIVE-LOCK:
  FIND ArtBas NO-LOCK WHERE
    ArtBas.ArtikkelNr = TransLogg.ArtikkelNr NO-ERROR.
    
  ASSIGN
    wAntPoster = wAntPoster + 1.                                
                                  
  IF wAntPoster MODULO 1000 = 0  THEN
  DO:
    PAUSE 0.
    DISPLAY
      "Antall leste poster:" wAntPoster SKIP
      "Antall slettede poster:" wAntLEstUt SKIP
      WITH FRAME info-Frame.
  END.

  /* Er det salg på en ukjent artikkel, skal den ikke slettes. */
  IF TransLogg.ArtikkelNr = 0 THEN
    NEXT TRANSLOGG.
  
  /* Artikkelen er slettet. */
  IF NOT AVAILABLE ArtBas THEN
  DO:
    ASSIGN
      wAntLestUt = wAntLEstUt + 1.
    EXPORT STREAM utData DELIMITER ";" TransLogg.
    PUT STREAM utLogg UNFORMATTED
      STRING(TransLogg.ArtikkelNr) " "
      STRING(TransLogg.Vg) " " 
      STRING(TransLogg.LopNr) " ** Ukjent artikkelnummer.." SKIP.
    /* DELETE TransLogg. */
    NEXT TRANSLOGG.
  END.
  
  /* Artikkelen er slettet og artikkelnummeret er benyttet omigjen. */  
  IF ArtBas.LopNr = ? THEN
  DO:
    ASSIGN
      wAntLestUt = wAntLestUt + 1.
    EXPORT STREAM utData DELIMITER ";" TransLogg.
    PUT STREAM utLogg UNFORMATTED
      STRING(TransLogg.ArtikkelNr) " "
      STRING(TransLogg.Vg) " " 
      STRING(TransLogg.LopNr) " ** Artikkelen er slettet og artikkelnr benyttet omigjen..." SKIP.
    /* DELETE TransLogg. */
    NEXT TRANSLOGG.
  END.
  
  /* Artikkelen er slettet og artikkelnummeret er benyttet omigjen, og */
  /* artikkelen har fått et nytt løpenummer.                           */  
  IF (ArtBas.Vg    <> TransLogg.Vg) OR
     (ArtBas.LopNr <> TransLogg.LopNr) THEN
  DO:
    ASSIGN
      wAntLestUt = wAntLestUt + 1.
    EXPORT STREAM utData DELIMITER ";" TransLogg.
    PUT STREAM utLogg UNFORMATTED
      STRING(TransLogg.ArtikkelNr) " "
      STRING(TransLogg.Vg) " " 
      STRING(TransLogg.LopNr) " ** Ulikt Vg/LopNr...("
      string(ArtBas.Vg) "/" STRING(ArtBas.LopNr) ")..." SKIP.
    /* DELETE TransLogg. */
  END.
END. /* TRANSLOGG */

PUT STREAM utLogg UNFORMATTED
  "------------------------------------------" SKIP
  "** Ferdig med kontroll av transer.      **" SKIP
  "** Dato: " today "Tid: " STRING(TIME,"HH:MM:SS") SKIP
  "** Brukt tid: " STRING(TIME - wStartTid,"HH:MM:SS") SKIP
  "** Antall poster i translogg: " wAntPoster SKIP
  "** Antall 'slettede' poster: " wAntLestUt SKIP
  "------------------------------------------" SKIP.             
          
OUTPUT stream utLogg CLOSE.             
OUTPUT STREAM utData CLOSE.
