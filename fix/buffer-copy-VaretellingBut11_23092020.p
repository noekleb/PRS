/************************************************************
    Program:  buffer-copy-VaretellingBut11_23092020.p
    Created:  TN   23 sep 20
Description:  Oppretter batch logg og fyller den opp med
              motposteringer av svinntransaksjoner.
              Korrigering av feil varetelling fra Traze terminalene.

Last change:  TN   14 Jun 99    9:31 pm
************************************************************/

DEF VAR wBatchNr      AS INT           NO-UNDO.
DEF VAR wSisteBatchNr AS INT NO-UNDO.
DEF VAR wTransNr      LIKE TransLogg.TransNr NO-UNDO.
DEF VAR x             AS INT NO-UNDO.
DEF VAR wOldButik     AS INT NO-UNDO.
DEF VAR wSkipListe    AS CHAR NO-UNDO.
DEFINE VARIABLE iTelleNr AS INTEGER NO-UNDO.

DEF BUFFER bufTransLogg FOR TransLogg.

CURRENT-WINDOW:WIDTH  = 350.
CURRENT-WINDOW:HEIGHT = 40. 

/* Sett inn en kommaseparert liste med de batchnummer som ikke skal kopieres. */
ASSIGN
  wSkipListe = ""
  iTelleNr   = 29215
  .
IF SEARCH('tnc.txt') <> ? THEN 
  iTelleNr = 1.

FIND TelleHode NO-LOCK WHERE 
  TelleHode.TelleNr = iTelleNr NO-ERROR.
IF NOT AVAILABLE TelleHode THEN 
DO:
  MESSAGE 'Ukjent telling ' iTelleNr
  VIEW-AS ALERT-BOX.
  RETURN.
END.


wBatchNr = 0.
TELLELINJER:
FOR EACH TelleLinje OF TelleHode NO-LOCK WHERE 
  TelleLinje.Sasong = 202003 AND 
  TelleLinje.AntallDiff <> 0:

  IF wBatchNr = 0 THEN 
  DO:
    /* Setter batchNr */
    RUN batchlogg.p (PROGRAM-NAME(1),
                     "Fiks TRAZE telling - Motpostere svinn",
                     OUTPUT wBatchNr).
    /* Flagger batchen under oppdatering. */
    RUN batchstatus.p (wBatchNr, 0).
  END.
    
  FIND FIRST TransLogg NO-LOCK WHERE 
    TransLogg.BatchNr    = TelleHode.BatchNr AND 
    TransLogg.Butik      = TelleLinje.Butik AND 
    TransLogg.ArtikkelNr = TelleLinje.ArtikkelNr AND 
    TransLogg.Storl      = TelleLinje.Storl AND 
    TransLogg.TTId       = 9 /* SVINN */ NO-ERROR.
  DISPLAY 
    wBatchNr
    TelleLinje.TelleNr
    TelleHode.BatchNr
    TelleLinje.Butik
    TelleLinje.ArtikkelNr
    TelleLinje.Sasong
    TelleLinje.AntallPar
    TelleLinje.AntallTalt
    TelleLinje.AntallDiff
    '|'
    TransLogg.BatchNr WHEN AVAILABLE Translogg 
    TransLogg.ArtikkelNr WHEN AVAILABLE Translogg
    TransLogg.Storl WHEN AVAILABLE Translogg
    TransLogg.TTId WHEN AVAILABLE Translogg
  WITH WIDTH 350.   
  
  IF AVAILABLE TransLogg THEN
  DAGSLYS: 
  DO:
    FIND LAST bufTransLogg WHERE
      bufTransLogg.Butik = TransLogg.butik
      USE-INDEX TransLogg NO-ERROR.
    IF AVAILABLE bufTransLogg THEN
      wTransNR = bufTransLogg.TransNr + 1.
    ELSE
      wTransNr = 1.
  
    CREATE bufTransLogg.
    
    BUFFER-COPY TransLogg EXCEPT TransNr BatchNr TO  bufTransLogg
      ASSIGN
        bufTransLogg.TransNr     = wTransNr
        bufTransLogg.BatchNr     = wBatchNr
        bufTransLogg.Antall      = TransLogg.Antall * -1 
        bufTransLogg.Postert     = FALSE
        bufTransLogg.PostertDato = ?
        bufTransLogg.PostertTid  = 0
        bufTransLogg.FeilKode    = 0
        bufTransLogg.Plukket     = TRUE
        bufTransLogg.VVAreKost   = 0
        bufTransLogg.SAttvVarekost = FALSE
        .
  END. /* DAGSLYS */  
END. /* TELLELINJER */     

IF iTelleNr <> 0 THEN 
  /* Flagger batchen klar for oppdatering. */
  RUN batchstatus.p (wBatchNr, 0).
    
  
