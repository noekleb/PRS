DEF VAR wLagSum AS INT NO-UNDO.
DEF STREAM err.
DEF VAR iButNr AS INT NO-UNDO.
DEF VAR cOrgFilNavn AS CHAR NO-UNDO.
DEF VAR cFilNavn AS CHAR NO-UNDO.
DEFINE VARIABLE bFlagg AS LOG NO-UNDO.

ASSIGN 
    iButNr = 9
    cOrgFilNavn = "konv\sjekkLagerArtLag_&ButNr_" + REPLACE(STRING(TODAY),'/','') + ".csv"
    .


FOR EACH Butiker NO-LOCK:
    DISPLAY
    Butiker.butik Butiker.ButNamn
    .
    PAUSE 0 BEFORE-HIDE.
    
    cFilNavn = REPLACE(cOrgFilNavn,'&ButNr',STRING(Butiker.Butik)).


    FOR EACH ArtBas NO-LOCK,
        EACH Lager OF ArtBas NO-LOCK WHERE 
        Lager.Butik = Butiker.Butik 
        BREAK BY Lager.butik:

      IF FIRST-OF(Lager.Butik) THEN 
      DO:
        OUTPUT stream err to value(cFilNavn).
    
        PUT STREAM Err UNFORMATTED
            'ArtBas.ArtikkelNr;ArtBas.LevKod;ArtBas.LevFargKod;ArtBas.Vg;ArtBas.LopNr;ArtBas.ARtikkelNr;Lager.Lagant;wLagSum'
            SKIP.
      END.
  
      wLagSum = 0. 
      FOR EACH ArtLAg NO-LOCK WHERE
        ArtLag.Butik = Lager.Butik AND
        ArtLag.ArtikkelNr = Lager.ArtikkelNr:
        wLagSum = wLAgSum + ArtLag.LagAnt.
      END.

      IF Lager.LagAnt <> wLagSum THEN
      DO:
        EXPORT STREAM err DELIMITER ";" 
          ArtBas.ArtikkelNr ArtBas.LevKod ArtBas.LevFargKod ArtBas.Vg ArtBas.LopNr ArtBas.ARtikkelNr
          Lager.Lagant wLagSum.
        RUN korrigerArtlag_fra_translogg.p (ArtBas.ArtikkelNr).
      END.    

      IF LAST-OF(Lager.Butik) THEN 
      DO:
        OUTPUT stream err close.        
      END.
    END.

END.
