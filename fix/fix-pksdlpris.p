  DEF VAR fMvaKr        AS DEC  NO-UNDO.
  DEF VAR fDbKr         AS DEC  NO-UNDO.
  DEF VAR fAntBest      AS DEC  NO-UNDO.
  DEF VAR fAntLevert    AS DEC  NO-UNDO.
  DEF VAR cTekst        AS CHAR NO-UNDO.

DEF VAR lHK AS LOG NO-UNDO.
{syspara.i 1 1 18 cTekst}
IF CAN-DO("1,yes,true",cTekst) THEN lHK = TRUE.
ELSE lHK = FALSE.

current-window:width = 250.

PUBLISH 'infoDisp' ("Initierer VPI informasjon..").

For each PkSdlHode where
  PkSdlHode.PkSdlStatus = (IF lHK THEN 5 ELSE 10),
  each PkSdlPris of PkSdlHode:
  
  find first ArtPris where
    ArtPris.ArtikkelNr = PkSdlPris.ArtikkelNr no-lock no-error.
  
  Find first PkSdlLinje where
    PkSdlLinje.PkSdlId = PkSdlHode.PkSdlId and
    PkSdlLinje.ArtikkelNr = PkSdlPris.ArtikkelNr.

  /* TN - Retter opp rest antall. Nødvendig pga usikkerhet om hva som har skjedd med ordre lokalt. */
  RESTBEREGNIN:
  DO:
    ASSIGN
        fAntLevert = 0
        fAntBest   = 0.
    /* Henter antall opprinnelig bestillt. */
    FIND FIRST StrKonv NO-LOCK
         WHERE StrKonv.StrKode = PkSdlLinje.StrKode
         NO-ERROR.
    IF AVAIL StrKonv THEN
      FOR EACH BestStr NO-LOCK
          WHERE BestStr.BestNr = PkSdlLinje.BestNr
            AND BestStr.Butik  = PkSdlLinje.ButikkNr
            AND TRIM(BestStr.Storl) = TRIM(StrKonv.Storl)
          BY BestStr.BestStat DESC
          :
        fAntBest = BestStr.Bestilt.
        LEAVE.
      END.

    /* Henter antall tidligere levert. */
    FIND FIRST StrKonv NO-LOCK
         WHERE StrKonv.StrKode = PkSdlLinje.StrKode
         NO-ERROR.
    IF AVAIL StrKonv THEN
      FOR EACH BestLevert NO-LOCK
          WHERE BestLevert.BestNr = PkSdlLinje.BestNr
            AND BestLevert.Butik  = PkSdlLinje.ButikkNr
            AND TRIM(BestLevert.Storl) = TRIM(StrKonv.Storl):
        ASSIGN
            fAntLevert   = fAntLevert + BestLevert.Levert.                        
      END.
    /* Beregner rest. */
    ASSIGN
      PkSdlLinje.AntRest = fAntBest - fAntLevert - PkSdlLinje.AntLevert.
  END. /* RESTBEREGNING */
  
  find first BestHode no-lock where
    BestHode.BestNr = PkSdlLinje.BestNr and
    BestHode.ArtikkelNr = PksdlPris.ArtikkelNr no-error.
  if available BestHode then
    find last BestPris of BestHode no-lock.
    
  if available BestPris then
    assign
    PkSdlPris.Pris = BestPris.Pris.
    
  If available ArtPris then  
    ASSIGN
        PkSdlPris.NyRab1%        = PkSdlPris.Rab1%
        PkSdlPris.Varekost       = PkSdlPris.InnkjopsPris - ((PkSdlPris.Innkjopspris * PkSdlPris.Rab1%) / 100)
        fMvaKr                   = PkSdlPris.Pris - (PkSdlPris.Pris / (1 + (ArtPris.Mva%[1] / 100)))
        fDbKr                    = PkSdlPris.Pris - fMvaKr - PkSdlPris.Varekost
        PkSdlPris.DB%            = ROUND((fDbKr * 100) / (PkSdlPris.Varekost + fDbKr),2)
        PkSdlPris.DB%            = IF PkSdlPris.DB% = ? THEN 0 ELSE PkSdlPris.DB%
                
        PkSdlPris.NyPris         = ArtPris.Pris[1] /* Lokal lpris skal gjelde. */
        PkSdlPris.NyFrakt        = PkSdlPris.Frakt
        PkSdlPris.NyVarekost     = PkSdlPris.NyInnkjopsPris - ((PkSdlPris.NyInnkjopspris * PkSdlPris.NyRab1%) / 100)
        fMvaKr                   = PkSdlPris.NyPris - (PkSdlPris.NyPris / (1 + (ArtPris.Mva%[1] / 100)))
        fDbKr                    = PkSdlPris.NyPris - fMvaKr - PkSdlPris.NyVarekost
        PkSdlPris.NyDB%          = ROUND((fDbKr * 100) / (PkSdlPris.NyVarekost + fDbKr),2)
        PkSdlPris.NyDB%          = IF PkSdlPris.NyDB% = ? THEN 0 ELSE PkSdlPris.NyDB%
        .
  
  /*
  display
  PkSdlHode.PkSdlId
  PkSdlPris.ArtikkelNr
  PkSdlPris.LevKod
  PkSdlPris.Beskr
  BestPris.Pris when available BestPris
  PkSdlPris.Pris
  PkSdlPris.Rab1%
  PkSdlPris.NyRab1%
  PkSdlPris.Varekost
  PkSdlPris.NyVareKost
  PkSdlPris.Db%
  PkSdlPris.NyDb%
  with width 250.
  */
  
end.  

PUBLISH 'infoDisp' ("..").

