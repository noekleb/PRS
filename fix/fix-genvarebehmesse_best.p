/* Fix-rutine for generering av messeordre

  - Sett inn varebehnr, endringsdato på varebehlinjetrans og navn på loggfil
  - NB: Det tas ikke hensyn til om ordre allerede er generert
-----------------------------------------------------------------------------------*/
DEF VAR icParam     AS CHAR NO-UNDO.
DEF VAR ihBuffer    AS HANDLE NO-UNDO.
DEF VAR icSessionId AS CHAR NO-UNDO.
DEF VAR ocReturn    AS CHAR NO-UNDO.
DEF VAR obOK        AS LOG NO-UNDO.

DEF VAR ix              AS INT    NO-UNDO.
DEF VAR iy              AS INT    NO-UNDO.
DEF VAR cCLlist         AS CHAR   NO-UNDO.
DEF VAR fVarebehNr      AS DEC    NO-UNDO.
DEF VAR cButListe       AS CHAR   NO-UNDO.
DEF VAR bSend           AS LOG    NO-UNDO.
DEF VAR cUserId         AS CHAR   NO-UNDO.
DEF VAR cLevNrListe     AS CHAR   NO-UNDO.
DEF VAR cArtikkelNr     AS CHAR   NO-UNDO.
DEF VAR bRegenIkkeSendt AS LOG    NO-UNDO. 
DEF VAR bSendtBest      AS LOG    NO-UNDO.
DEF VAR dEndret         AS DATE   NO-UNDO.
DEF VAR cLogFile        AS CHAR   NO-UNDO.

DEF VAR cStrList        AS CHAR   NO-UNDO.
DEF VAR cSortFordList   AS CHAR   NO-UNDO.

ASSIGN fVarebehNr      = 90000003        /* <- Endre de tre første */
       dEndret         = TODAY
       cLogFile        = "c:\temp\genordre.csv"
       
       cUserId         = "tomn"
       .

DEF TEMP-TABLE ttSendt
    FIELD fSendtArtikkelNr AS DEC
    FIELD iSendtBestNr     AS INT
    FIELD iLevUke          AS INT
    INDEX idxSendtArt fSendtArtikkelNr
    .

/* DEF TEMP-TABLE TT_VareBehBestLinje NO-UNDO LIKE VareBehBestLinje RCODE-INFORMATION.  */
{incl/tt_varebehbestlinje.i}
DEF VAR hBufftt_varebehbestlinje AS HANDLE NO-UNDO.
hBufftt_varebehbestlinje = BUFFER tt_VarebehBestLinje:HANDLE.

DEF VAR hBuffVarebehBestHode AS HANDLE NO-UNDO.
hBuffVarebehBestHode = BUFFER VarebehBestHode:HANDLE.


FIND VarebehHode WHERE VarebehHode.VarebehNr = fVarebehNr
     NO-LOCK NO-ERROR.
IF NOT AVAIL VarebehHode THEN DO:
  ocReturn = "Varebehandlingsbok ikke tilgjengelig for oppdatering".    
  RETURN.
END.


/* Finner de involverte sentrallagere først: */ 
FOR EACH VarebehLinjeTrans NO-LOCK
    WHERE VarebehLinjeTrans.VarebehNr = fVarebehNr
      AND VarebehLinjeTrans.RegistrertBestilling
      AND VarebehLinjeTrans.GodkjentBestilling
      AND VarebehLinjeTrans.EDato = dEndret
    ,FIRST Butiker NO-LOCK
           WHERE Butiker.Butik = VarebehLinjeTrans.ButikkNr
    :

  IF NOT CAN-DO(cCLlist,STRING(Butiker.clButikkNr)) THEN
    cCLlist = cCLlist + (IF cCLlist NE "" THEN "," ELSE "") + STRING(Butiker.clButikkNr).
END.

DO ix = 1 TO NUM-ENTRIES(cCLlist):
  FOR EACH Butiker NO-LOCK
      WHERE Butiker.CLbutikkNr = INT(ENTRY(ix,cCLlist)),
      EACH VarebehLinjeTrans NO-LOCK
           WHERE VarebehLinjeTrans.VarebehNr = fVarebehNr
             AND VarebehLinjeTrans.ButikkNr  = Butiker.Butik
             AND VareBehLinjeTrans.EDato     = dEndret
             AND VarebehLinjeTrans.Bestilt1 NE 0
             AND VarebehLinjeTrans.GodkjentBestilling
     ,FIRST Artbas NO-LOCK OF VarebehLinjeTrans
     ,FIRST VarebehLinje NO-LOCK OF VarebehLinjeTrans
            WHERE VarebehLinje.ArtikkelNr = VarebehLinjeTrans.ArtikkelNr
              AND (IF cLevNrListe NE "" THEN CAN-DO(cLevNrListe,STRING(VarebehLinje.LevNr)) ELSE TRUE)
      BREAK BY VarebehLinjeTrans.ArtikkelNr
            BY VarebehLinjeTrans.Levuke1:

    IF FIRST-OF(VarebehLinjeTrans.Levuke1) THEN 
      EMPTY TEMP-TABLE TT_VareBehBestLinje.

    bSendtBest = NO.

    IF NOT bSendtBest THEN DO:
      FIND FIRST StrKonv OF VarebehLinjeTrans NO-LOCK NO-ERROR.
      IF AVAIL StrKonv THEN 
        RUN CreateTT (VarebehLinjeTrans.Bestilt1,TRIM(StrKonv.Storl)).
      ELSE DO:
        RUN Inndeling (VarebehLinjeTrans.Bestilt1).
        DO iy = 1 TO NUM-ENTRIES(cStrList):
          RUN CreateTT (VarebehLinjeTrans.Bestilt1 * INT(ENTRY(iy,cSortFordList)),
                        ENTRY(iy,cStrList)).
        END.
      END.

  
      IF LAST-OF(VarebehLinjeTrans.Levuke1) THEN DO:
        RUN varebehbest_lagrebestlinje.p
            (STRING(fVarebehNr) + "," +
             ENTRY(ix,cCLlist) + ",," +
             STRING(VarebehLinjeTrans.Levuke1) + ",genbest," + 
             STRING(VarebehLinje.ArtikkelNr) + "," +
             STRING(VarebehLinje.Levnr)
             ,
  
             hBufftt_varebehbestlinje,
             icSessionId,
             OUTPUT ocReturn,
             OUTPUT obOK
             ).
        IF ocReturn NE "" THEN UNDO, LEAVE.
        RUN Logg.
      END.
    END.
  END.  

  FOR EACH Butiker NO-LOCK
      WHERE Butiker.CLbutikkNr = INT(ENTRY(ix,cCLlist)),
      EACH VarebehLinjeTrans NO-LOCK
           WHERE VarebehLinjeTrans.VarebehNr = fVarebehNr
             AND VarebehLinjeTrans.ButikkNr  = Butiker.Butik
             AND VareBehLinjeTrans.EDato     = dEndret
             AND VarebehLinjeTrans.Bestilt2 NE 0
             AND VarebehLinjeTrans.GodkjentBestilling
     ,FIRST Artbas NO-LOCK OF VarebehLinjeTrans
     ,FIRST VarebehLinje NO-LOCK OF VarebehLinjeTrans
            WHERE VarebehLinje.ArtikkelNr = VarebehLinjeTrans.ArtikkelNr
              AND (IF cLevNrListe NE "" THEN CAN-DO(cLevNrListe,STRING(VarebehLinje.LevNr)) ELSE TRUE)
      BREAK BY VarebehLinjeTrans.ArtikkelNr
            BY VarebehLinjeTrans.Levuke2:

    IF FIRST-OF(VarebehLinjeTrans.Levuke2) THEN 
      EMPTY TEMP-TABLE TT_VareBehBestLinje.

    bSendtBest = NO.

    IF NOT bSendtBest THEN DO:
      FIND FIRST StrKonv OF VarebehLinjeTrans NO-LOCK NO-ERROR.
      IF AVAIL StrKonv THEN 
        RUN CreateTT (VarebehLinjeTrans.Bestilt2,TRIM(StrKonv.Storl)).
      ELSE DO:
        RUN Inndeling (VarebehLinjeTrans.Bestilt2).
        DO iy = 1 TO NUM-ENTRIES(cStrList):
          RUN CreateTT (VarebehLinjeTrans.Bestilt2 * INT(ENTRY(iy,cSortFordList)),
                        ENTRY(iy,cStrList)).
        END.
      END.

      IF LAST-OF(VarebehLinjeTrans.Levuke2) THEN DO:
        RUN varebehbest_lagrebestlinje.p
            (STRING(fVarebehNr) + "," +
             ENTRY(ix,cCLlist) + ",," +
             STRING(VarebehLinjeTrans.Levuke2) + ",genbest," + 
             STRING(VarebehLinje.ArtikkelNr) + "," +
             STRING(VarebehLinje.Levnr)
             ,
  
             hBufftt_varebehbestlinje,
             icSessionId,
             OUTPUT ocReturn,
             OUTPUT obOK
             ).
  
        IF ocReturn NE "" THEN UNDO, LEAVE.
        RUN Logg.
      END.
    END.
  END.  

  FOR EACH Butiker NO-LOCK
      WHERE Butiker.CLbutikkNr = INT(ENTRY(ix,cCLlist)),
      EACH VarebehLinjeTrans NO-LOCK
           WHERE VarebehLinjeTrans.VarebehNr = fVarebehNr
             AND VarebehLinjeTrans.ButikkNr  = Butiker.Butik
             AND VareBehLinjeTrans.EDato     = dEndret
             AND VarebehLinjeTrans.Bestilt3  NE 0
             AND VarebehLinjeTrans.GodkjentBestilling
     ,FIRST Artbas NO-LOCK OF VarebehLinjeTrans
     ,FIRST VarebehLinje NO-LOCK OF VarebehLinjeTrans
            WHERE VarebehLinje.ArtikkelNr = VarebehLinjeTrans.ArtikkelNr
              AND (IF cLevNrListe NE "" THEN CAN-DO(cLevNrListe,STRING(VarebehLinje.LevNr)) ELSE TRUE)
      BREAK BY VarebehLinjeTrans.ArtikkelNr
            BY VarebehLinjeTrans.Levuke3:

    IF FIRST-OF(VarebehLinjeTrans.Levuke3) THEN 
      EMPTY TEMP-TABLE TT_VareBehBestLinje.

    bSendtBest = NO.

    IF NOT bSendtBest THEN DO:
      FIND FIRST StrKonv OF VarebehLinjeTrans NO-LOCK NO-ERROR.
      IF AVAIL StrKonv THEN 
        RUN CreateTT (VarebehLinjeTrans.Bestilt3,TRIM(StrKonv.Storl)).
      ELSE DO:
        RUN Inndeling (VarebehLinjeTrans.Bestilt3).
        DO iy = 1 TO NUM-ENTRIES(cStrList):
          IF INT(ENTRY(iy,cSortFordList)) > 0 THEN DO:
            RUN CreateTT (VarebehLinjeTrans.Bestilt3 * INT(ENTRY(iy,cSortFordList)),
                          ENTRY(iy,cStrList)).
          END.
        END.
      END.

      IF LAST-OF(VarebehLinjeTrans.Levuke3) THEN DO:
        RUN varebehbest_lagrebestlinje.p
            (STRING(fVarebehNr) + "," +
             ENTRY(ix,cCLlist) + ",," +
             STRING(VarebehLinjeTrans.Levuke3) + ",genbest," + 
             STRING(VarebehLinje.ArtikkelNr) + "," +
             STRING(VarebehLinje.Levnr)
             ,
  
             hBufftt_varebehbestlinje,
             icSessionId,
             OUTPUT ocReturn,
             OUTPUT obOK
             ).
        IF ocReturn NE "" THEN UNDO, LEAVE.      
        RUN Logg.
      END.
    END.
  END.  

  FOR EACH Butiker NO-LOCK
      WHERE Butiker.CLbutikkNr = INT(ENTRY(ix,cCLlist)),
      EACH VarebehLinjeTrans NO-LOCK
           WHERE VarebehLinjeTrans.VarebehNr = fVarebehNr
             AND VarebehLinjeTrans.ButikkNr  = Butiker.Butik
             AND VareBehLinjeTrans.EDato     = dEndret
             AND VarebehLinjeTrans.Bestilt4 NE 0
             AND VarebehLinjeTrans.GodkjentBestilling
     ,FIRST Artbas NO-LOCK OF VarebehLinjeTrans
     ,FIRST VarebehLinje NO-LOCK OF VarebehLinjeTrans
            WHERE VarebehLinje.ArtikkelNr = VarebehLinjeTrans.ArtikkelNr
              AND (IF cLevNrListe NE "" THEN CAN-DO(cLevNrListe,STRING(VarebehLinje.LevNr)) ELSE TRUE)
      BREAK BY VarebehLinjeTrans.ArtikkelNr
            BY VarebehLinjeTrans.Levuke4:

    IF FIRST-OF(VarebehLinjeTrans.Levuke4) THEN 
      EMPTY TEMP-TABLE TT_VareBehBestLinje.

    bSendtBest = NO.

    IF NOT bSendtBest THEN DO:
      FIND FIRST StrKonv OF VarebehLinjeTrans NO-LOCK NO-ERROR.
      IF AVAIL StrKonv THEN 
        RUN CreateTT (VarebehLinjeTrans.Bestilt4,TRIM(StrKonv.Storl)).
      ELSE DO:
        RUN Inndeling (VarebehLinjeTrans.Bestilt4).
        DO iy = 1 TO NUM-ENTRIES(cStrList):
          RUN CreateTT (VarebehLinjeTrans.Bestilt4 * INT(ENTRY(iy,cSortFordList)),
                        ENTRY(iy,cStrList)).
        END.
      END.
    
      IF LAST-OF(VarebehLinjeTrans.Levuke4) THEN DO:
        RUN varebehbest_lagrebestlinje.p
            (STRING(fVarebehNr) + "," +
             ENTRY(ix,cCLlist) + ",," +
             STRING(VarebehLinjeTrans.Levuke4) + ",genbest," + 
             STRING(VarebehLinje.ArtikkelNr) + "," +
             STRING(VarebehLinje.Levnr)
             ,
  
             hBufftt_varebehbestlinje,
             icSessionId,
             OUTPUT ocReturn,
             OUTPUT obOK
             ).
        IF ocReturn NE "" THEN UNDO, LEAVE.
        RUN Logg.
      END.
    END.
  END.  

END.

IF ocReturn = "" THEN obOk = TRUE.

PROCEDURE Inndeling:
  DEF INPUT PARAM iiAntSort AS INT NO-UNDO.

  ASSIGN cStrList      = ""
         cSortFordList = ""
         .
  FOR EACH ArtSort NO-LOCK
      WHERE ArtSort.ArtikkelNr = VarebehLinje.ArtikkelNr
        AND ArtSort.SortId     = VarebehLinjeTrans.Kode
     ,FIRST LevSort OF ArtSort NO-LOCK:

    FOR EACH LevSAnt OF LevSort NO-LOCK BY SeqNr:
      ASSIGN cStrList      = cStrList + TRIM(LevSAnt.SoStorl) + ","
             cSortFordList = cSortFordList + STRING(LevSAnt.SoAnt) + ","
             .
    END.
    ASSIGN cStrList      = TRIM(cStrList,",")
           cSortFordList = TRIM(cSortFordList,",")
           .
    RUN CreateTT (iiAntSort,"Sortiment|" + ArtSort.SortId).
  END.
END PROCEDURE.

PROCEDURE CreateTT:
  DEF INPUT PARAM iiAntall AS INT  NO-UNDO.
  DEF INPUT PARAM icStorl  AS CHAR NO-UNDO.

  IF iiAntall = 0 THEN RETURN.

  FIND FIRST TT_VareBehBestLinje
       WHERE TT_VareBehBestLinje.VareBehNr       = fVarebehNr                
         AND TT_VareBehBestLinje.CLButikkNr      = INT(ENTRY(ix,cCLlist))    
         AND TT_VareBehBestLinje.BestiltButikkNr = VarebehLinjeTrans.ButikkNr
         AND TT_VareBehBestLinje.Storl           = icStorl
       NO-ERROR.
  IF NOT AVAIL TT_VareBehBestLinje THEN DO:
    CREATE TT_VareBehBestLinje.
    ASSIGN TT_VareBehBestLinje.VareBehNr       = fVarebehNr
           TT_VareBehBestLinje.CLButikkNr      = INT(ENTRY(ix,cCLlist))
           TT_VareBehBestLinje.BestiltButikkNr = VarebehLinjeTrans.ButikkNr
           TT_VareBehBestLinje.HodeLinjeId     = TIME
           TT_VareBehBestLinje.Storl           = icStorl
           NO-ERROR.
    IF ERROR-STATUS:ERROR THEN 
      MESSAGE "Kunne ikke opprette overføringstabell for artikkelnr " VarebehLinje.ArtikkelNr 
              VIEW-AS ALERT-BOX ERROR.
  END.
  TT_VareBehBestLinje.Bestilt = TT_VareBehBestLinje.Bestilt + iiAntall.
END PROCEDURE.

PROCEDURE Logg:

/*    RETURN. */

   OUTPUT TO VALUE(cLogFile) APPEND.
   FOR EACH TT_VareBehBestLinje:
     EXPORT DELIMITER ";"
            TT_VareBehBestLinje.VareBehNr
            TT_VareBehBestLinje.CLButikkNr
            TT_VareBehBestLinje.BestiltButikkNr
            TT_VareBehBestLinje.HodeLinjeId
            TT_VareBehBestLinje.Bestilt
            TT_VareBehBestLinje.Storl
            STRING(fVarebehNr)
            ENTRY(ix,cCLlist)
            STRING(VarebehLinjeTrans.Levuke1)
            STRING(VarebehLinje.ArtikkelNr)
            STRING(VarebehLinje.Levnr)
            .
   END.
   OUTPUT CLOSE.
   
END PROCEDURE.
