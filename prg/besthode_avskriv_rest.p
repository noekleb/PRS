DEF INPUT PARAMETER iBestNr AS INT NO-UNDO.

DEF VAR iLeveringsNr AS INT NO-UNDO.
DEF VAR iAntMak AS INT NO-UNDO.
DEF VAR iBestStat AS INT NO-UNDO.

DEFINE BUFFER bufBestPris FOR BestPris.
DEFINE BUFFER bufBestLevert FOR BestLevert.
DEFINE BUFFER bufBestHLev FOR BestHLev.
DEFINE BUFFER bufBestStr FOR BestStr.

ASSIGN
    iBestStat = 6.

DO TRANSACTION:
  FIND Besthode EXCLUSIVE-LOCK WHERE
      BestHode.BestNr = iBestNr NO-ERROR.
  IF AVAILABLE BestHode THEN  
  BESTHODET:
  DO:
      IF BestHode.BestStat < 3 OR BestHode.BestStat > 5 THEN
          LEAVE BESTHODET.
      
      /* Henter siste innleveranse */
      FIND LAST bufBestHLev OF BestHode NO-LOCK NO-ERROR.
      
      /* Ny innleveranse */
      CREATE BestHLev.
      ASSIGN
          BestHLev.BestNr       = BestHode.BestNr
          BestHLev.LeveringsNr  = IF AVAILABLE bufBestHLev THEN bufBestHLev.LeveringsNr + 1 ELSE 1
          BestHLev.LevertDato   = TODAY
          BestHLev.LevTidspunkt = TIME
          BestHLev.LevertAv     = USERID("SkoTex").
      
      IF AVAILABLE bufBestHLev THEN
      AVSKRIV_DELEVERT:
      DO:
          FOR EACH BestLevert OF bufBestHLev EXCLUSIVE-LOCK:
              BUFFER-COPY BestLevert TO bufBestLEvert
                  ASSIGN 
                  bufBestLevert.Leveringsnr = BestHLev.LeveringsNr
                  bufBestLevert.Avskrevet   = TRUE
                  bufBestLevert.Levert      = 0
                  bufBestLevert.Rest        = BestLevert.Rest
                  .
              iAntMak = iAntMak + bufBestLevert.Rest.
          END.
      END. /* AVSKRIV_DELEVERT */
      ELSE 
      AVVSKRIV_HELE:
      DO:
          FOR EACH BestStr OF BestHode NO-LOCK WHERE 
              BestStr.BestStat = BestHode.BestStat:
              CREATE BestLevert.
              ASSIGN
                  BestLevert.Leveringsnr = BestHLev.LeveringsNr
                  BestLevert.BestNr      = BestHode.BestNr
                  BestLevert.Storl       = BestStr.Storl
                  BestLevert.Butik       = BestStr.Butik
                  BestLevert.Levert      = 0
                  BestLevert.Rest        = BestStr.Bestilt
                  BestLevert.Avskrevet   = TRUE
                  BestLevert.LevertDato  = TODAY
                  BestLevert.LevertAv    = USERID("SkoTex")
                  iAntMak                = iAntMak + BestLevert.Rest.
          END.
      END. /* AVVSKRIV_HELE */
      
      /* Flytter med info på størrelsene */
      FOR EACH BestStr NO-LOCK WHERE 
        BestStr.BestNr = BestHode.BestNr AND 
        BestStr.BestStat = BestHode.BestStat:
        BUFFER-COPY BestStr TO bufBestStr
            ASSIGN bufBestSTr.BestStat = iBestStat.
      END.
      
      FIND FIRST BestPris NO-LOCK WHERE 
        BestPris.BestNr = BestHode.BestNr AND 
        BestPris.BestStat = BestHode.BestStat NO-ERROR.
      BUFFER-COPY BestPris TO bufBestPris
          ASSIGN bufBestPris.BestStat = iBestStat.  
      ASSIGN
          BestHode.BestStat    = iBestStat
          BestHode.TotMakulert = iAntMak.
  END. /* BESTHODET */ 
END.

