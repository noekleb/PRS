/* Bibliotek for kalkulerte felter, pakklistelinje
  Opprettet: 09.08.07 av BHa
------------------------------------------------------------------------------*/  
DEF VAR fAntBest     AS DEC NO-UNDO.
DEF VAR fAntBestRest AS DEC NO-UNDO.

PROCEDURE pksdllinje_NyPris:
    DEF INPUT  PARAM irPkSdlLinje AS ROWID NO-UNDO.
    DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
    DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.

    FIND PkSdlLinje WHERE ROWID(PkSdlLinje) = irPkSdlLinje NO-LOCK NO-ERROR.
    IF AVAIL PkSdlLinje THEN 
    DO:
      FIND PkSdlPris NO-LOCK WHERE 
        PkSdlPris.PkSdlId = PkSdlLinje.PkSdlId AND 
        PkSdlPris.ArtikkelNr = PkSdlLinje.ArtikkelNr NO-ERROR.
      IF AVAILABLE PkSdlPris THEN 
          ocValue = STRING(PkSdlPris.NyPris).
      ELSE 
          ocValue = ''.
    END.
END PROCEDURE.

PROCEDURE pksdllinje_NyVareKost:
    DEF INPUT  PARAM irPkSdlLinje AS ROWID NO-UNDO.
    DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
    DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.

    FIND PkSdlLinje WHERE ROWID(PkSdlLinje) = irPkSdlLinje NO-LOCK NO-ERROR.
    IF AVAIL PkSdlLinje THEN 
    DO:
      FIND PkSdlPris NO-LOCK WHERE 
        PkSdlPris.PkSdlId = PkSdlLinje.PkSdlId AND 
        PkSdlPris.ArtikkelNr = PkSdlLinje.ArtikkelNr NO-ERROR.
      IF AVAILABLE PkSdlPris THEN 
          ocValue = STRING(PkSdlPris.NyVareKost).
      ELSE 
          ocValue = ''.
    END.
END PROCEDURE.
        
PROCEDURE strkonv_storl:
  DEF INPUT  PARAM irStrKonv AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId   AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue       AS CHAR  NO-UNDO.

  FIND StrKonv NO-LOCK
       WHERE ROWID(StrKonv) = irStrKonv
       NO-ERROR.

  IF AVAIL StrKonv THEN 
    ocValue = TRIM(StrKonv.Storl).
END PROCEDURE.

PROCEDURE pksdllinje_ant_bestilt:
  DEF INPUT  PARAM irPkSdlLinje  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.
  
  fAntBest = 0.
  FIND PkSdlLinje WHERE ROWID(PkSdlLinje) = irPkSdlLinje NO-LOCK NO-ERROR.
  IF AVAIL PkSdlLinje THEN DO:
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
  END.
  ocValue = STRING(fAntBest).
END.

PROCEDURE pksdllinje_ant_Antall:
  DEF INPUT  PARAM irPkSdlLinje  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.
  
  DEF VAR fAntLevert AS DEC NO-UNDO.
  
  FIND PkSdlLinje WHERE ROWID(PkSdlLinje) = irPkSdlLinje NO-LOCK NO-ERROR.
  IF AVAIL PkSdlLinje THEN DO:
    FIND FIRST StrKonv NO-LOCK
         WHERE StrKonv.StrKode = PkSdlLinje.StrKode
         NO-ERROR.

    IF AVAIL StrKonv THEN
      FOR EACH BestLevert NO-LOCK
          WHERE BestLevert.BestNr = PkSdlLinje.BestNr
            AND BestLevert.Butik  = PkSdlLinje.ButikkNr
            AND TRIM(BestLevert.Storl) = TRIM(StrKonv.Storl)
          :
        ASSIGN
            fAntLevert   = fAntLevert + BestLevert.Levert.            
      END.
  END.
  ocValue = STRING(fAntBest - fAntLevert).
END.

PROCEDURE pksdllinje_gyldig_kode:
  DEF INPUT  PARAM irPkSdlLinje AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.

  FIND PkSdlLinje WHERE ROWID(PkSdlLinje) = irPkSdlLinje NO-LOCK NO-ERROR.
  IF AVAIL PkSdlLinje THEN DO:
    IF PkSdlLinje.Kode = '' THEN 
    DO:
        FIND FIRST Strekkode NO-LOCK 
             WHERE Strekkode.ArtikkelNr = PkSdlLinje.ArtikkelNr
               AND Strekkode.StrKode    = PkSdlLinje.StrKode
               AND NOT Strekkode.Kode   BEGINS "02" 
             NO-ERROR.
         ocValue = STRING(AVAIL Strekkode).
     END.
     ELSE DO:
        FIND FIRST Strekkode NO-LOCK 
             WHERE Strekkode.Kode = PkSdlLinje.Kode NO-ERROR.
        IF NOT AVAILABLE(Strekkode) THEN 
           ocValue = STRING(AVAIL Strekkode).
        ELSE DO:
           IF (Strekkode.ArtikkelNr <> PkSdlLinje.ArtikkelNr OR 
               Strekkode.StrKode    <> PkSdlLinje.StrKode) THEN 
               ocValue = 'No'.
           ELSE ocValue = 'Yes'.
        END.     
     END.
  END.
END PROCEDURE.

PROCEDURE pksdllinje_feilkoblet_kode:
  DEF INPUT  PARAM irPkSdlLinje AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.

    FIND PkSdlLinje WHERE ROWID(PkSdlLinje) = irPkSdlLinje NO-LOCK NO-ERROR.
    IF AVAIL PkSdlLinje THEN DO:
      
      FIND FIRST Strekkode NO-LOCK 
           WHERE Strekkode.Kode = PkSdlLinje.Kode NO-ERROR.
           
      IF TRIM(PkSdlLinje.Kode) = '' OR NOT AVAILABLE Strekkode THEN 
          ocValue = 'No'.
      ELSE ocValue = IF Strekkode.ArtikkelNr <> PkSdlLinje.ArtikkelNr THEN 'Yes' ELSE 'No'.
    END.
    ELSE ocValue = 'No'.
END PROCEDURE.

PROCEDURE pksdllinje_opphav:
    DEF INPUT  PARAM irPkSdlLinje AS ROWID NO-UNDO.
    DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
    DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.

    FIND PkSdlLinje WHERE ROWID(PkSdlLinje) = irPkSdlLinje NO-LOCK NO-ERROR.
    IF AVAIL PkSdlLinje THEN 
    DO:
        FIND PkSdlHode OF PkSdlLinje NO-LOCK NO-ERROR.
        IF AVAILABLE PkSdlHode THEN 
            ocValue = STRING(PkSdlHode.PkSdlOpphav).
        ELSE 
            ocValue = 'No'.
    END.
    ELSE ocValue = 'No'.
END PROCEDURE.

PROCEDURE pksdllinje_pksdlstatus:
  DEF INPUT  PARAM irPkSdlLinje AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue AS CHAR NO-UNDO.

    FIND PkSdlLinje WHERE ROWID(PkSdlLinje) = irPkSdlLinje NO-LOCK NO-ERROR.
    IF AVAIL PkSdlLinje THEN 
    DO:
        FIND PkSdlHode OF PkSdlLinje NO-LOCK NO-ERROR.
        IF AVAILABLE PkSdlHode THEN 
            ocValue = STRING(PkSdlHode.PkSdlStatus).
        ELSE 
            ocValue = 'No'.
    END.
    ELSE ocValue = 'No'.
END PROCEDURE.

PROCEDURE pksdllinje_eTid:
    DEF INPUT  PARAM irPkSdlLinje AS ROWID NO-UNDO.
    DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
    DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.

    FIND PkSdlLinje WHERE ROWID(PkSdlLinje) = irPkSdlLinje NO-LOCK NO-ERROR.
    IF AVAIL PkSdlLinje THEN
        ocValue = STRING(PkSdlLinje.ETid,"HH:MM:SS").
    ELSE 
        ocValue = ''. 
END PROCEDURE.









