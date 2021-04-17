/* pksdl_korrVarekost.p 
  
  Korrigerer varekost til LandedCost for pakkseddler som leveres inn på kommisjonsbutikker.
  Butikken må ligge i mapping tabell for kommisjonsbutikker.
  
*/

DEFINE INPUT PARAMETER lPkSdlId AS DECIMAL NO-UNDO.

DEFINE VARIABLE fMvaKr     AS DECIMAL   NO-UNDO.
DEFINE VARIABLE fDbKr      AS DECIMAL   NO-UNDO.
DEFINE VARIABLE fAntBest   AS DECIMAL   NO-UNDO.
DEFINE VARIABLE fAntLevert AS DECIMAL   NO-UNDO.
DEFINE VARIABLE cTekst     AS CHARACTER NO-UNDO.

DEFINE VARIABLE cEDB-System AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTabell AS CHARACTER NO-UNDO.
DEFINE VARIABLE iGantAktiv AS INTEGER NO-UNDO. 

{syspara.i 210 100 8 iGantAktiv INT}

ASSIGN 
  cEDB-System = 'Gant Global'
  cTabell     = 'KommisjonBut'
  .

IF iGantAktiv = 1 THEN  
DO:
  FOR EACH PkSdlHode NO-LOCK WHERE
    PksdlHode.PkSdlId = lPkSdlId,
    EACH PkSdlPris OF PkSdlHode EXCLUSIVE-LOCK:
      
    /* Skal bare behandle kommisjonsbutikker. */
    FIND FIRST ImpKonv NO-LOCK WHERE 
      ImpKonv.EDB-System = cEDB-System AND 
      ImpKonv.Tabell     = cTabell AND 
      ImpKonv.InterntID = STRING(PkSdlHode.ButikkNr) NO-ERROR.

    IF NOT AVAILABLE ImpKonv THEN 
      NEXT.    
    
    FIND ArtBas NO-LOCK WHERE 
      ArtBas.ArtikkelNr = PkSdlPris.ArtikkelNr NO-ERROR.
    
    IF (ArtBas.KjedeInnkPris > 0 AND 
      ArtBas.KjedeInnkPris <> ?) THEN   
      ASSIGN
        PkSdlPris.NyRab1%        = 0
        PkSdlPris.NyInnkjopsPris = ArtBas.KjedeInnkPris        
        PkSdlPris.NyVarekost     = ArtBas.KjedeInnkPris
        fMvaKr                   = PkSdlPris.NyPris - (PkSdlPris.NyPris / (1 + (25 / 100)))
        fDbKr                    = PkSdlPris.NyPris - fMvaKr - PkSdlPris.NyVarekost
        PkSdlPris.NyDB%          = ROUND((fDbKr * 100) / (PkSdlPris.NyVarekost + fDbKr),2)
        PkSdlPris.NyDB%          = IF PkSdlPris.NyDB% = ? THEN 0 ELSE PkSdlPris.NyDB%
        .
  END.  
END.
