/* Innlevering av bestilling. Kan levere inn ordre eller en bestilling 
   Parametere: 
               - Transtype: "fullbest" eller "fullordre";userid;Pipe-separert liste av bestillinger eller liste av ordrenr
                    
   Opprettet: 09.11.05 av BHa    
   Endret:    09.08.07 av BHa:
            - Kan også gjøre del-leveranse dersom funksjonen LevAntall finnes i kallende program              
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.
  
DEFINE VAR wStorlek      AS CHAR NO-UNDO. /* strl i find, går ej med ch_Grid */
DEFINE VAR wButik        LIKE BestStr.Butik NO-UNDO.
DEFINE VAR wLevert       AS INTE NO-UNDO. 
DEF VAR wBatchNr         AS INT  NO-UNDO.
DEF VAR wLoop            AS INT  NO-UNDO.
DEF VAR wTransNr         AS INT  NO-UNDO.
DEF VAR wSkjerm          AS CHAR NO-UNDO.
DEF VAR wDirekte         AS LOG  NO-UNDO.
DEF VAR cUserid          AS CHAR NO-UNDO.
DEF VAR h_PrisKo         AS HANDLE NO-UNDO.
DEFINE VARIABLE lEtikettFinns AS LOGICAL    NO-UNDO.
DEFINE VARIABLE iTotInnLev   LIKE BestHode.TotInnLev   NO-UNDO.
DEFINE VARIABLE lRegistreratButik AS LOGICAL    NO-UNDO.
DEFINE VARIABLE iBuntnr     AS INTEGER          NO-UNDO.
DEFINE VARIABLE iLinjeNr    AS INTEGER  INIT 1  NO-UNDO.
DEF VAR iCount       AS INTEGER NO-UNDO.
DEF VAR dIndividNr   AS DECIMAL    NO-UNDO.
DEF VAR iIndividBatchNr AS INTEGER    NO-UNDO.
DEF VAR dArtikkelNr LIKE ArtBas.ArtikkelNr NO-UNDO.
DEFINE VARIABLE cDummy AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iStrKode AS INTEGER NO-UNDO.
DEFINE VARIABLE iPkSdlOpphav AS INTEGER NO-UNDO.

DEF VAR wBestHodeRecId   AS RECID NO-UNDO.
DEF VAR wArtBasRecid     AS RECID NO-UNDO.
DEF VAR wBestHLevRec     AS RECID NO-UNDO.
DEF VAR wLeveringsNr     AS INT   NO-UNDO.
DEF VAR wEDB-System      AS CHAR  NO-UNDO.
DEF VAR wTabell          AS CHAR  NO-UNDO.
DEF VAR iOpphav          AS INT   INIT 2 NO-UNDO. /* ovBunt.Opphav = 2 */

DEF VAR cTransType       AS CHAR  NO-UNDO.
DEF VAR cBestNrListe     AS CHAR  NO-UNDO.
DEF VAR ix               AS INT   NO-UNDO.
DEF VAR iCl              AS INT   NO-UNDO.
DEF VAR cIndividBest     AS CHAR  NO-UNDO.
DEF VAR cIkkeSendt       AS CHAR  NO-UNDO.

DEF VAR cArtikkelEti    AS CHAR NO-UNDO.
DEF VAR cEtiketter      AS CHAR NO-UNDO.
DEF VAR cAntallEti      AS CHAR NO-UNDO.
DEF VAR cIndividNr      AS CHAR NO-UNDO.
  
DEF VAR cEtiketterTmp   AS CHARACTER  NO-UNDO.
DEF VAR cAntallEtiTmp   AS CHARACTER  NO-UNDO.
DEF VAR cIndividNrTmp   AS CHARACTER  NO-UNDO.
DEF VAR bFullInnlev     AS LOG        NO-UNDO.
DEF VAR fTidlLevert     AS DEC        NO-UNDO.

{syspara.i 1 2 3 wEDB-System}
IF wEDB-System = "" THEN
  wEDB-System = "OVERFOR-LOCK".

DEF BUFFER bBestHode FOR BestHode.

DEF TEMP-TABLE ttBatchNr 
    FIELD ButikkNr  AS INT
    FIELD BatchNr   AS INT.

DEF NEW SHARED TEMP-TABLE TT_OvBuffer NO-UNDO LIKE OvBuffer.
  
ASSIGN cTransType = ENTRY(1,icParam,";")
       cUserId    = ENTRY(2,icParam,";")
       .

IF cTransType MATCHES "*best" THEN cBestNrListe = ENTRY(3,icParam,";").
ELSE DO ix = 1 TO NUM-ENTRIES(ENTRY(3,icParam,";"),"|"):
  FOR EACH BestHode NO-LOCK
      WHERE BestHode.OrdreNr = INT(ENTRY(ix,ENTRY(3,icParam,";"),"|")):
    cBestNrListe = cBestNrListe + STRING(BestHode.BestNr) + "|".
  END.
END.

/* Lager en komma-separert liste over bestillinger: */
cBestNrListe = TRIM(cBestNrListe,"|").

/* Sjekker først om det fins individvarer på bestillingene. I så fall må disse leveres inn manuelt: */
DO ix = 1 TO NUM-ENTRIES(cBestNrListe,"|"):
  FOR FIRST BestHode NO-LOCK
      WHERE BestHode.BestNr = INT(ENTRY(ix,cBestNrListe,"|"))
        AND BestHode.BestStat < 6:
    IF CAN-FIND(FIRST ArtBas NO-LOCK OF BestHode
                WHERE ArtBas.IndividType > 0) THEN
      cIndividBest = cIndividBest + "Ordre: " + STRING(BestHode.OrdreNr) + ", Bestnr: " + STRING(BestHode.BestNr) + CHR(10).
    IF BestHode.BestStat < 4 THEN
      cIkkeSendt = cIkkeSendt + "Ordre: " + STRING(BestHode.OrdreNr) + ", Bestnr: " + STRING(BestHode.BestNr) + CHR(10).
  END.
END.
IF cIndividBest NE "" THEN 
  ocReturn = "Bestillinger av individvarer må leveres inn pr bestilling: " + CHR(10) + cIndividBest + CHR(10).

IF cIkkeSendt NE "" THEN 
  ocReturn = ocReturn + "Bestillinger må først være sendt før varer kan mottas: " + CHR(10) + cIkkeSendt.


IF ocReturn NE "" THEN RETURN.

TRANSBLOKK:
DO ix = 1 TO NUM-ENTRIES(cBestNrListe,"|") TRANSACTION ON ERROR UNDO, LEAVE:                    

  EMPTY TEMP-TABLE TT_OvBuffer.

  FIND BestHode EXCLUSIVE-LOCK
       WHERE BestHode.BestNr = INT(ENTRY(ix,cBestNrListe,"|"))
       NO-ERROR.
  IF NOT AVAIL BestHode THEN DO:
    ocReturn = "Bestillingsnr " + ENTRY(ix,cBestNrListe) + " er ikke tilgjengelig for oppdatering" + CHR(10) + PROGRAM-NAME(1).
    UNDO, LEAVE TRANSBLOKK.
  END.

  IF BestHode.BestStat GE 6 THEN NEXT.

  FIND FIRST Ordre OF BestHode NO-LOCK NO-ERROR.
  IF NOT AVAIL Ordre THEN DO:
    ocReturn = "Bestillingsnr " + ENTRY(ix,cBestNrListe) + " mangler ordre" + CHR(10) + PROGRAM-NAME(1).
    UNDO, LEAVE TRANSBLOKK.
  END.
  iCL = ordre.CL.

  FIND FIRST ArtBas OF BestHode NO-LOCK NO-ERROR.
  IF NOT AVAIL ArtBas THEN DO:
    ocReturn = "Finner ikke artikkel for bestillingsnr " + ENTRY(ix,cBestNrListe) + CHR(10) + PROGRAM-NAME(1).
    UNDO, LEAVE TRANSBLOKK.
  END.
  
  ASSIGN wBestHodeRecid = RECID(BestHode)
         wArtBasRecid   = RECID(ArtBas)
         iLinjeNr       = 1
         iTotInnLev     = 0
         .

  IF NOT BestHode.DirekteLev THEN lRegistreratButik = CAN-FIND(FIRST BestLinje OF BestHode WHERE BestLinje.Butik NE iCl).
  ELSE lRegistreratButik = FALSE.

  IF BestHode.BestStat = 4 THEN DO:
    RUN bytbeststatus.p (wBestHodeRecid,"+",?).
    IF RETURN-VALUE = "Avbryt" THEN DO:
      ocReturn = "Feil ved endring av status på bestillingsnr " + ENTRY(ix,cBestNrListe,"|") + CHR(10) + PROGRAM-NAME(1).
      UNDO TRANSBLOKK, LEAVE TRANSBLOKK. 
    END.
  END.
    
  IF NOT AVAILABLE ArtBas THEN
    FIND ArtBas NO-LOCK WHERE RECID(ArtBas) = wArtBasRecid.       
  IF ArtBas.Inn_Dato = ? THEN DO:
      FIND CURRENT ArtBas EXCLUSIVE-LOCK.
      ASSIGN ArtBAs.Inn_Dato = TODAY.
      FIND CURRENT ArtBas NO-LOCK.
  END.

  /* Plukker frem Opphavet på pakkseddelen.                                                       */
  /* Prisoppdatering skal ikke gjøres ved varemottak av pakksedler som stammer fra en overføring. */
  iPkSdlOpphav = 0.
  IF BestHode.Merknad BEGINS "PkSdlOpphav=" AND NUM-ENTRIES(BestHode.Merknad,'=') >= 2 THEN 
    ASSIGN iPkSdlOpphav = INT(ENTRY(2,BestHode.Merknad,'=')) NO-ERROR.
      
  /* Hvis innlevering gjøres fra w-gridinnlev.w, kjøres prisoppdatering der. */
  IF iPkSdlOpphav <> 4 AND CAN-DO(SOURCE-PROCEDURE:INTERNAL-ENTRIES,"PrisOppdatering") THEN 
  DO:      
    RUN PrisOppdatering IN SOURCE-PROCEDURE (BestHode.BestNr,OUTPUT ocReturn).
    IF ocReturn NE "" THEN
      UNDO TRANSBLOKK, LEAVE TRANSBLOKK.
  END.
  /* Ellers kjører vi prisoppdatering her. */
  ELSE IF iPkSdlOpphav <> 4 THEN DO:
      
    /* Oppdaterer kalkyle for alle profiler som det er registrert kalkyle på. */
    PRISOPPDATERING:
    FOR EACH BestPris NO-LOCK WHERE
      BestPris.BestNr   = BestHode.BestNr AND
      BestPris.BestStat = BestHode.BestStat:
      ASSIGN
        wSkjerm = STRING(BestPris.ValPris)       + ";" +
                  string(BestPris.InnkjopsPris)  + ";" +
                  string(BestPris.Rab1Kr)        + ";" +
                  string(BestPris.Rab1%)         + ";" +
                  string(BestPris.Rab2Kr)        + ";" +  
                  string(BestPris.Rab2%)         + ";" +
                  string(BestPris.Frakt)         + ";" +
                  string(BestPris.Frakt%)        + ";" + 
                  string(BestPris.DivKostKr)     + ";" + 
                  string(BestPris.DivKost%)      + ";" +
                  string(BestPris.Rab3Kr)        + ";" +
                  string(BestPris.Rab3%)         + ";" +
                  string(BestPris.VareKost)      + ";" +
                  string(BestPris.MvaKr)         + ";" +
                  string(BestPris.Mva%)          + ";" + 
                  string(BestPris.DBKr)          + ";" +
                  string(BestPris.DB%)           + ";" +
                  string(BestPris.Pris)          + ";" +
                  string(BestPris.EuroPris)      + ";" +
                  string(BestPris.EuroManuel)    + ";" + /* 20 */
                  string(TODAY)                  + ";" + /* 21 Aktiv fra */
                  "0"                            + ";" + /* 22 */
                  ""                             + ";" + /* 23 */
                  "0"                            + ";" + /* 24 */
                  ""                             + ";" + /* 25 */
                  "0"                            + ";" + /* 26 */
                  ""                             + ";" + /* 27 */
                  "no".
        
      IF NOT VALID-HANDLE(h_PrisKo) THEN RUN prisko.p PERSISTENT SET h_PrisKo.
  
      RUN LagreArtPris IN h_PrisKo
        (INPUT wArtBasRecid,
         INPUT BestPris.ProfilNr,
         INPUT-OUTPUT wSkjerm,
         INPUT FALSE,  /* wTilbud = false - Dvs ordinær kaflkyle.         */
         INPUT TRUE,   /* Direkte oppdatering av prisene som er kalkulert */
         INPUT 1,
         ?).
    END. /* PRISOPPDATERING */
  END.

  CREATE BestHLev.
  ASSIGN BestHLev.BestNr       = BestHode.BestNr
         BestHLev.LeveringsNr  = NEXT-VALUE(LeveringsNr)
         BestHLev.LevertDato   = TODAY
         BestHLev.LevTidspunkt = TIME
         BestHLev.LevertAv     = cUserid
         wBestHLevRec          = RECID(BestHLev)
         wLeveringsNr          = BestHLev.LeveringsNr
         bFullInnlev           = YES
         .


  FOR EACH BestStr NO-LOCK 
      WHERE BestStr.BestNr   = BestHode.BestNr 
        AND BestStr.BestStat = BestHode.BestStat:

    wButik   = BestStr.Butik.
    
    IF CAN-DO(SOURCE-PROCEDURE:INTERNAL-ENTRIES,"InnlevertButikk") AND 
       NOT DYNAMIC-FUNCTION("InnlevertButikk" IN SOURCE-PROCEDURE,wButik) THEN DO:
      bFullInnlev = NO.     
      NEXT.
    END.
    /*
    IF AVAILABLE StrKonv THEN RELEASE StrKonv.
    iStrKode = DYNAMIC-FUNCTION("getStrKode" IN SOURCE-PROCEDURE,BestHode.BestNr,BestStr.Butik,BestStr.Storl) NO-ERROR.    
    IF iStrKode > 0 THEN
      FIND StrKonv NO-LOCK WHERE StrKonv.StrKode = iStrKode NO-ERROR. 
    IF NOT AVAILABLE StrKonv THEN 
    */
    DO: 
        FIND FIRST StrKonv NO-LOCK
             WHERE TRIM(StrKonv.Storl) = TRIM(BestStr.Storl)
             NO-ERROR.
        IF AVAIL StrKonv THEN wStorlek = StrKonv.Storl.
        ELSE DO:
          FIND FIRST StrKonv NO-LOCK
               WHERE StrKonv.Storl = BestStr.Storl
             NO-ERROR.
          IF AVAIL strKonv THEN wStorlek = StrKonv.Storl.
          ELSE wStorlek = BestStr.Storl.
        END.
    END. 
    /*ELSE wStorlek = StrKonv.Storl.*/
    
    /* Henter kalkylen */
    FIND Butiker WHERE Butiker.Butik = wButik NO-LOCK NO-ERROR.
    IF NOT AVAIL Butiker THEN DO:
      ocReturn = "Finner ikke butikk for bestilling " + ENTRY(ix,cBestNrListe,"|") + CHR(10) + PROGRAM-NAME(1).
      UNDO, LEAVE TRANSBLOKK.
    END.

    FIND BestPris NO-LOCK
         WHERE BestPris.BestNr   = BestHode.BestNr 
           AND BestPris.BestStat = BestHode.BestStat
           AND BestPris.ProfilNr = Butiker.ProfilNr 
         NO-ERROR.
    IF NOT AVAIL BestPris THEN DO:
      FIND Butiker NO-LOCK
           WHERE Butiker.Butik = iCL 
           NO-ERROR.
      IF NOT AVAIL Butiker THEN DO:
        ocReturn = "Finner ikke sentrallager for bestilling " + ENTRY(ix,cBestNrListe,"|") + CHR(10) + PROGRAM-NAME(1).
        UNDO, LEAVE TRANSBLOKK.
      END.
      FIND BestPris NO-LOCK 
           WHERE BestPris.BestNr = BestHode.BestNr 
             AND BestPris.BestStat = BestHode.BestStat 
             AND BestPris.ProfilNr = Butiker.ProfilNr 
           NO-ERROR.
      IF NOT AVAIL BestPris THEN DO:
        ocReturn = "Finner ikke pris for bestilling " + ENTRY(ix,cBestNrListe,"|") + CHR(10) + PROGRAM-NAME(1).
        UNDO, LEAVE TRANSBLOKK.
      END.
    END.

    FIND LAST BestLevert NO-LOCK
         WHERE BestLevert.BestNr = BestHode.BestNr
           AND BestLevert.Butik  = wButik
           AND BestLevert.Storl  = BestStr.Storl
         NO-ERROR.
    IF AVAIL BestLevert THEN 
      wLevert = BestLevert.Rest.
    ELSE wLevert = BestStr.Bestilt.

    IF CAN-DO(SOURCE-PROCEDURE:INTERNAL-ENTRIES,"LevAntall") AND AVAIL StrKonv THEN DO:
      wLevert = DYNAMIC-FUNCTION("LevAntall" IN SOURCE-PROCEDURE,BestHode.BestNr,BestStr.Butik,StrKonv.StrKode).
      IF wLevert = 0 THEN DO:
        bFullInnlev = NO.  
        NEXT.
      END. 
      ELSE IF wLevert = ? THEN 
      DO:
        iStrKode = DYNAMIC-FUNCTION("getStrKode" IN SOURCE-PROCEDURE,BestHode.BestNr,BestStr.Butik,BestStr.Storl) NO-ERROR.
        wLevert = DYNAMIC-FUNCTION("LevAntall" IN SOURCE-PROCEDURE,BestHode.BestNr,BestStr.Butik,iStrKode) NO-ERROR.        
        IF wLevert = ? THEN
        DO:
          NEXT.
        END.
      END.
    END.

    fTidlLevert = 0.
    FOR EACH BestLevert NO-LOCK
        WHERE BestLevert.BestNr = BestStr.BestNr
          AND BestLevert.Butik  = BestStr.Butik
          AND TRIM(BestLevert.Storl)  = TRIM(BestStr.Storl):
      fTidlLevert = fTidlLevert + BestLevert.Levert.
    END.

    IF wLevert < BestStr.Bestilt - fTidlLevert THEN bFullInnlev = NO.

    CREATE BestLevert.
    ASSIGN BestLevert.BestNr      = BestHLev.BestNr
           BestLevert.Butik       = wButik
           BestLevert.Storl       = BestStr.Storl
           BestLevert.LeveringsNr = BestHLev.LeveringsNr
           BestLevert.Levert      = wLevert
           BestLevert.LevertAv    = cUserid 
           BestLevert.LevertDato  = TODAY
           BestLevert.Avskrevet   = NO
           BestLevert.Rest        = BestStr.Bestilt - fTidlLevert - wLevert
           iTotInnLev             = iTotInnLev + wLevert
           .

    IF wButik <> iCL AND lRegistreratButik AND wLevert > 0 THEN DO:
      CREATE TT_OvBuffer.
      ASSIGN TT_OvBuffer.BuntNr      = 999 /* dummy */
             TT_OvBuffer.LinjeNr     = iLinjeNr
             TT_OvBuffer.ButikkNrFra = iCL
             TT_OvBuffer.ButikkNrTil = wButik        
             TT_OvBuffer.ArtikkelNr  = ArtBas.ArtikkelNr
             TT_OvBuffer.Vg          = ArtBas.vg   
             TT_OvBuffer.LopNr       = ArtBas.LopNr
             TT_OvBuffer.Antall      = wLevert
             TT_OvBuffer.Merknad     = "Best " + STRING(BestHode.BestNr)
             TT_OvBuffer.Storl       = BestStr.Storl
             TT_OvBuffer.TilStorl    = BestStr.Storl
             TT_OvBuffer.Varekost    = IF AVAILABLE BestPris
                                           THEN BestPris.VareKost
                                           ELSE 0
             iLinjeNr                = iLinjeNr + 1.
      RELEASE TT_OvBuffer.
    END.
                                        
    /* Oppretter evt ny batch for buikken: */
    FIND FIRST ttBatchNr 
         WHERE ttBatchNr.ButikkNr = (IF (NOT BestHode.DirekteLev AND lRegistreratButik) 
                                            THEN iCL 
                                            ELSE wButik)
         NO-ERROR.
    IF NOT AVAIL ttBatchNr THEN DO:
      RUN batchlogg.w (PROGRAM-NAME(1), "Innleveranse av ordre - BestNr: " + string(BestHode.BestNr),
                       OUTPUT wBatchNr).
      CREATE ttBatchNr.
      ASSIGN ttBatchNr.ButikkNr = (IF (NOT BestHode.DirekteLev AND lRegistreratButik) 
                                            THEN iCL 
                                            ELSE wButik)
             ttBatchNr.BatchNr  = wBatchNr.
    END.
    ELSE wBatchNr = ttBatchNr.BatchNr.

    /* Transaksjonsnummer for butikken. */
    FIND LAST TransLogg NO-LOCK WHERE
      TransLogg.Butik = (IF (NOT BestHode.DirekteLev AND lRegistreratButik) 
                                            THEN iCL 
                                            ELSE wButik) use-index TransLogg no-error.
    IF AVAILABLE TransLogg THEN
      wTransNr = TransLogg.TransNr + 1.
    ELSE 
      wTransNr = 1.
      /* Oppretter transaksjon */
    IF wLevert > 0 THEN DO iCount = 1 TO (IF ArtBas.IndividType > 0 THEN wLevert ELSE 1):
      LAG_TRANS:
      DO:
        /* Sjekker at transnr er ledig */
        IF can-find(TransLogg where
                    TransLogg.Butik   = (IF (NOT BestHode.DirekteLev AND lRegistreratButik) THEN iCL ELSE wButik) and
                    TransLogg.TransNr = wTransNr) then
          NESTE_NR:
          DO WHILE TRUE:
            wTransNr = wTransNr + 1.
            IF can-find(TransLogg where
                        TransLogg.Butik   = (IF (NOT BestHode.DirekteLev AND lRegistreratButik) THEN iCL ELSE wButik) and
                        TransLogg.TransNr = wTransNr) then
              next NESTE_NR.
            else
              leave NESTE_NR.
          END. /* NESTE_NR */

        CREATE TransLogg.
        ASSIGN TransLogg.Butik        = IF (NOT BestHode.DirekteLev AND lRegistreratButik) 
                                          THEN iCL 
                                          ELSE wButik
               TransLogg.TransNr      = wTransNr
               TransLogg.SeqNr        = 1.
        ASSIGN TransLogg.BatchNr      = wBatchNr
               TransLogg.KundNr       = 0
               TransLogg.TTId         = 5 /* Varekjøp */
               TransLogg.TBId         = 1
               TransLogg.ArtikkelNr   = BestHode.ArtikkelNr
               TransLogg.LevNr        = BestHode.LevNr
               TransLogg.BongId       = 0
               TransLogg.BongLinjeNr  = 0
               TransLogg.KassaNr      = 0
               TransLogg.Vg           = ArtBas.Vg
               TransLogg.LopNr        = ArtBas.LopNr
               TransLogg.Antall       = IF ArtBas.IndividType > 0 THEN 1 ELSE wLevert
               TransLogg.Pris         = (IF AVAILABLE BestPris
                                          THEN BestPris.VareKost
                                          ELSE 0)
               Translogg.Pris         = (IF (TransLogg.Pris < 0 OR TransLogg.Pris = ?)
                                          THEN 0
                                          ELSE Translogg.Pris)
               TransLogg.RabKr        = 0
               TransLogg.Mva          = 0
               TransLogg.Plukket      = true /* Skal ikke ut på plukkliste */
               TransLogg.Dato         = today
               TransLogg.Tid          = time
               TransLogg.BestNr       = BestHode.BestNr
               TransLogg.Storl        = wStorlek
               TransLogg.Indivi       = dIndividNr
               TransLogg.Postert      = false
                 
               cEtiketterTmp          = cEtiketterTmp + (IF cEtiketterTmp <> "" THEN "," ELSE "") + TRIM(TransLogg.Storl)
               cAntallEtiTmp          = cAntallEtiTmp + (IF cAntallEtiTmp <> "" THEN "," ELSE "") + STRING(TransLogg.Antall)
               cIndividNrTmp          = cIndividNrTmp + (IF cIndividNrTmp <> "" THEN "," ELSE "") + STRING(TransLogg.IndividNr)
               .                                             
      END. /* LAG_TRANS */
    END.

  END.

  FIND CURRENT BestHode EXCLUSIVE NO-ERROR.
  BestHode.TotInnLev   = BestHode.TotInnLev + iTotInnLev.
  FIND CURRENT BestHode NO-LOCK NO-ERROR.
        
  IF BestHode.BestStat < 5 OR bFullInnlev OR (BestHode.TotInnLev GE BestHode.TotAntPar AND BestHode.BestStat < 6)  THEN
    RUN bytbeststatus.p (wBestHodeRecid,"+",?).

  RUN SettOrdreStatus (BestHode.OrdreNr).

  IF CAN-FIND(FIRST TT_OvBuffer) THEN
     RUN LagraOvBuffer.p (INPUT-OUTPUT iBuntNr,ArtBas.ArtikkelNr,"N" + CHR(1) + "Best " + STRING(BestHode.BestNr),wEDB-System,wTabell,iOpphav).

  RUN genStrekKode.p (BestHode.BestNr,3,"").

  IF cEtiketterTmp <> "" THEN DO:
    DO iCount = 1 TO NUM-ENTRIES(cEtiketterTmp):
      FIND StrKonv WHERE TRIM(StrKonv.Storl) = ENTRY(iCount,cEtiketterTmp) NO-LOCK NO-ERROR.
      IF AVAIL StrKonv THEN DO:
        FIND FIRST StrekKode OF ArtBas WHERE StrekKode.KodeType = 1 AND 
                                             StrekKode.StrKode = StrKonv.StrKode AND
                                             NOT StrekKode.Kode BEGINS "02" NO-LOCK NO-ERROR.
        IF NOT AVAIL StrekKode THEN
            FIND FIRST StrekKode OF ArtBas WHERE StrekKode.KodeType = 1 AND 
                                                 StrekKode.StrKode = StrKonv.StrKode NO-LOCK NO-ERROR.
        ASSIGN ENTRY(iCount,cEtiketterTmp) = IF AVAIL StrekKode THEN StrekKode.Kode ELSE "".
      END.
    END.
    ASSIGN cArtikkelEti = cArtikkelEti + (IF cArtikkelEti <> "" THEN CHR(1) ELSE "") + STRING(ArtBas.ArtikkelNr)
           cEtiketter   = cEtiketter   + (IF cEtiketter   <> "" THEN CHR(1) ELSE "") + cEtiketterTmp
           cAntallEti   = cAntallEti   + (IF cAntallEti   <> "" THEN CHR(1) ELSE "") + cAntallEtiTmp
           cIndividNr   = cIndividNr   + (IF cIndividNr   <> "" THEN CHR(1) ELSE "") + cIndividNrTmp.
  END.

  ASSIGN cEtiketterTmp = ""
         cAntallEtiTmp = ""
         cIndividNrTmp = "".

END. /* TRANSBLOKK TRANSACTION */

IF ocReturn = "" THEN DO:
  obOk = TRUE.
  FOR EACH ttBatchNr:
    RUN batchstatus.p (ttBatchNr.BatchNr, 2).
  END.
END.

IF cArtikkelEti NE "" THEN
  ocReturn = "etikett" + 
             cArtikkelEti + "|" + 
             cEtiketter   + "|" + 
             cAntallEti   + "|" + 
             cIndividNr.

PROCEDURE SettOrdreStatus:
  DEF INPUT PARAM iiOrdreNr AS INT NO-UNDO.

  DEF VAR bSettStat AS LOG NO-UNDO INIT TRUE.

  /* TN 25/6-06 */
  RUN ordre_sjekk_levert.p (INPUT iiOrdreNr).
  FIND Ordre WHERE Ordre.OrdreNr = iiOrdreNr NO-LOCK NO-ERROR.

/*   FIND Ordre WHERE Ordre.OrdreNr = iiOrdreNr NO-LOCK NO-ERROR. */
/*   IF Ordre.OrdreStatus = 4 THEN RETURN.                        */
/*                                                                */
/*   FOR EACH bBestHode NO-LOCK                                   */
/*       WHERE bBestHode.OrdreNr = iiOrdreNr:                     */
/*     IF bBestHode.BestStat NE 4 THEN DO:                        */
/*       bSettStat = FALSE.                                       */
/*       LEAVE.                                                   */
/*     END.                                                       */
/*   END.                                                         */
/*   IF bSettStat THEN DO:                                        */
/*     FIND CURRENT Ordre EXCLUSIVE-LOCK NO-ERROR.                */
/*     IF AVAIL Ordre THEN Ordre.OrdreStatus = 4.                 */
/*   END.                                                         */
END.

