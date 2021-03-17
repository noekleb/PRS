/* Parametere:  
   Opprettet: 14.11.2008 - GOO                  
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icBuffer    AS CHAR NO-UNDO.
DEF INPUT  PARAM icRowid     AS CHAR NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
    
  def var wArtikkelNr  AS INT no-undo.
  def var wButik       as int no-undo.
  DEF VAR wNedskriv    AS LOG NO-UNDO.
  DEF VAR iTellenr     AS INT NO-UNDO.
  
  def buffer bTelleLinje for TelleLinje.
  def buffer bKonvReg    for KonvReg.

  DEF VAR wEDB-System   AS CHAR NO-UNDO.
  DEF VAR wTabell       AS CHAR NO-UNDO.

  {syspara.i 1 2 4 wEDB-System}
  assign wTabell = "ArtBas".

  FIND bTelleLinje WHERE ROWID(bTelleLinje) = TO-ROWID(icRowid) NO-LOCK NO-ERROR.
  IF AVAIL bTelleLinje THEN 
  DO ON ERROR undo, LEAVE TRANSACTION:
  
  /* Leser av verdiene for aktiv tellelinje */
  assign
    iTellenr    = bTelleLinje.TelleNr
    wArtikkelNr = bTelleLinje.ArtikkelNr
    wButik      = bTelleLinje.Butik
    wNedskriv   = FALSE /*Tom's beskjed*/
  .
    
  /* Tar bort tellelinjen */
  find bTelleLinje exclusive-lock where
    recid(bTelleLinje) = recid(TelleLinje) no-error.
  delete bTelleLinje.
  
  /* Sjekker om låsen skal slettes for andre typer av tellinger.      */
  /* Finnes det ikke flere tellelinjer igjen på artikkelen og butikk, */
  /* skal tellelåsen slettes.                                         */
  if wNedskriv = false then
    do:
      if not can-find(first bTelleLinje where
        bTelleLinje.TelleNr    = iTellenr and
        bTelleLinje.ArtikkelNr = wArtikkelNr and
        bTelleLinje.Butik      = wButik) then
        do:
          find first bKonvReg exclusive-lock where
            bKonvReg.EDB-System = wEDB-System and
            bKonvReg.Tabell     = wTabell     and
            bKonvReg.EkstId     = string(wArtikkelNr) + "," + 
                                 string(wButik) and
            bKonvReg.InterntId  = string(wArtikkelNr) + "," + 
                                 string(wButik) no-error.
          if available bKonvReg then
            delete bKonvReg.
        end.     
    end.      
  /* Sletter låsen for nedskrivning. */
  /* Her finnes kun en linje pr. artikkel og butikk. */
  else do:
      find first bKonvReg exclusive-lock where
        bKonvReg.EDB-System = wEDB-System and
        bKonvReg.Tabell     = wTabell     and
        bKonvReg.EkstId     = string(wArtikkelNr) + "," + 
                             string(wButik) and
        bKonvReg.InterntId  = string(wArtikkelNr) + "," + 
                             string(wButik) no-error.
      if available bKonvReg then
        delete bKonvReg.
  end.
end. /* TRANSACTION */
ocReturn = ''.
