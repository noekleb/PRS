/* Generer bestillinger for sentrallager fra messeregistrering
   Parametere:  <Varebehnr>,<liste over CL butikker> i parameterstreng
   Kommentar: Benytter eksisterende rutiner, varetilvpielogg.p og (evt) eloggtilvpivare.p
   
   Opprettet: 18.02.05 av BHa                  
   Endret:    23.08.05 av BHa
              - Oppretter inndelinger som egne poster i TT_VareBehBestLinje 
                etter følgende regel:
                  Storl settes til "Sortiment|"<SortId>
                  Bestilt settes til antall av sortiment
              22.11.05 av BHa
              - Kan sende ordre direkte. Ending i parametere:
                <Varebehnr>,"send"|"",userid,liste over CL butikker
              28.11.06 av BHa
              - Genererer kun artikler med flagg Kjedevare eller Gjennomfaktureres satt
              03.05.07 av BHa
              - Oppslag mot strekkode gjøres via StrKode. StrekKode felt på VareBehLinjeTrans er ikke lenger i bruk
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

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

DEF VAR cStrList        AS CHAR   NO-UNDO.
DEF VAR cSortFordList   AS CHAR   NO-UNDO.

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

ASSIGN fVarebehNr      = DEC(ENTRY(1,icParam))
       bSend           = ENTRY(2,icParam) = "send"
       cUserId         = ENTRY(3,icParam)
       cCLlist         = REPLACE(ENTRY(4,icParam),"|",",")
       cLevNrListe     = REPLACE(ENTRY(5,icParam),"|",",")
/*        cArtikkelNr     = ENTRY(6,icParam) */
       cArtikkelNr     = "0"
       bRegenIkkeSendt = ENTRY(7,icParam) = "regen"
       .

FIND VarebehHode WHERE VarebehHode.VarebehNr = fVarebehNr
     NO-LOCK NO-ERROR.
IF NOT AVAIL VarebehHode THEN DO:
  ocReturn = "Varebehandlingsbok ikke tilgjengelig for oppdatering".    
  RETURN.
END.
ELSE cButListe = VarebehHode.ButikkListe + ",". 

/* Sjekker først om det allerede er generert ordre. Ikke sendte ordre kan evt re-genereres: */ 
TRANSBLOKK:
DO ix = 1 TO NUM-ENTRIES(cCLlist):
  FOR EACH VarebehBestHode
     WHERE VarebehBestHode.VarebehNr  = fVarebehNr
       AND VarebehBestHode.CLbutikkNr = INT(ENTRY(ix,cCLlist))
       AND (IF cLevNrListe NE "" THEN CAN-DO(cLevNrListe,STRING(VarebehBestHode.LevNr)) ELSE TRUE)
       AND (IF cArtikkelNr NE "0" THEN VarebehBestHode.ArtikkelNr = DEC(cArtikkelNr) ELSE TRUE)
       EXCLUSIVE-LOCK:
    RUN delval_varebehbesthode.p ("",
        STRING(ROWID(VarebehBestHode)),
        icSessionId,
        OUTPUT ocReturn).
    IF ENTRY(1,ocReturn,";") = "sendt" AND bRegenIkkeSendt THEN DO:
      CREATE ttSendt.
      ASSIGN ttSendt.fSendtArtikkelNr = VarebehBestHode.ArtikkelNr
             ttSendt.iSendtBestNr     = VarebehBestHode.BestNr
             ttSendt.iLevUke          = VarebehBestHode.LevUke
             ocReturn                 = "".
    END.
    ELSE IF ocReturn NE "" THEN UNDO, LEAVE TRANSBLOKK.
    ELSE DELETE VarebehBestHode NO-ERROR.
  END.
END.  

IF NUM-ENTRIES(ocReturn,";") > 1 THEN ocReturn = ENTRY(2,ocReturn,";").

IF ocReturn = "" THEN DO ix = 1 TO NUM-ENTRIES(cCLlist):
  FOR EACH Butiker NO-LOCK
      WHERE Butiker.CLbutikkNr = INT(ENTRY(ix,cCLlist)),
      EACH VarebehLinjeTrans NO-LOCK
           WHERE VarebehLinjeTrans.VarebehNr = fVarebehNr
             AND VarebehLinjeTrans.ButikkNr  = Butiker.Butik
             AND (IF cArtikkelNr NE "0" THEN VarebehLinjeTrans.ArtikkelNr = DEC(cArtikkelNr) ELSE TRUE)
             AND VarebehLinjeTrans.Bestilt1 NE 0
             AND VarebehLinjeTrans.GodkjentBestilling
     ,FIRST Artbas NO-LOCK OF VarebehLinjeTrans
/*             WHERE Artbas.Kjedevare OR Artbas.Gjennomfaktureres bha 04.05.07 */
     ,FIRST VarebehLinje NO-LOCK OF VarebehLinjeTrans
            WHERE VarebehLinje.ArtikkelNr = VarebehLinjeTrans.ArtikkelNr
              AND (IF cLevNrListe NE "" THEN CAN-DO(cLevNrListe,STRING(VarebehLinje.LevNr)) ELSE TRUE)
      BREAK BY VarebehLinjeTrans.ArtikkelNr
            BY VarebehLinjeTrans.Levuke1:

    IF FIRST-OF(VarebehLinjeTrans.Levuke1) THEN 
      EMPTY TEMP-TABLE TT_VareBehBestLinje.

    bSendtBest = NO.
    FOR EACH ttSendt 
        WHERE ttSendt.fSendtArtikkelNr = VarebehLinjeTrans.Artikkelnr
          AND ttSendt.iLevUke          = VarebehLinjeTrans.LevUke1:
      IF CAN-FIND(FIRST BestLinje WHERE BestLinje.BestNr = ttSendt.iSendtBestNr
                                    AND BestLinje.Butik  = VarebehLinjeTrans.ButikkNr) THEN
        bSendtBest = YES.
    END.

    IF NOT bSendtBest THEN DO:
/*       FIND FIRST Strekkode OF VareBehLinjeTrans NO-LOCK NO-ERROR. */
/*       IF AVAIL Strekkode THEN DO:                                 */
/*       FIND FIRST StrKonv OF Strekkode NO-LOCK NO-ERROR. */
      FIND FIRST StrKonv OF VarebehLinjeTrans NO-LOCK NO-ERROR.
      IF AVAIL StrKonv THEN 
        RUN CreateTT (VarebehLinjeTrans.Bestilt1,TRIM(StrKonv.Storl)).
/*       END. */
      ELSE DO:
/*         IF LENGTH(VarebehLinjeTrans.Kode) = 13 THEN DO:                           */
/*           FIND FIRST VPIStrekkode                                                 */
/*                WHERE VPIStrekkode.EkstVPILevnr = VarebehLinje.Levnr               */
/*                  AND VPIStrekkode.kode = VareBehLinjeTrans.kode NO-LOCK NO-ERROR. */
/*           IF AVAIL VPIStrekkode THEN DO:                                          */
/*             FIND FIRST StrKonv NO-LOCK                                            */
/*                  WHERE StrKonv.StrKode = VPIStrekkode.StrKode NO-ERROR.           */
/*             IF AVAIL StrKonv THEN                                                 */
/*               RUN CreateTT (VarebehLinjeTrans.Bestilt1,TRIM(StrKonv.Storl)).      */
/*           END.                                                                    */
/*         END.                                                                      */
        RUN Inndeling (VarebehLinjeTrans.Bestilt1).
        DO iy = 1 TO NUM-ENTRIES(cStrList):
          RUN CreateTT (VarebehLinjeTrans.Bestilt1 * INT(ENTRY(iy,cSortFordList)),
                        ENTRY(iy,cStrList)).
        END.
      END.

      IF NOT CAN-DO(cButListe,STRING(Butiker.butik)) THEN
        cButListe = cButListe + STRING(Butiker.butik) + ",".
  
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
             AND (IF cArtikkelNr NE "0" THEN VarebehLinjeTrans.ArtikkelNr = DEC(cArtikkelNr) ELSE TRUE)
             AND VarebehLinjeTrans.Bestilt2 NE 0
             AND VarebehLinjeTrans.GodkjentBestilling
     ,FIRST Artbas NO-LOCK OF VarebehLinjeTrans
/*             WHERE Artbas.Kjedevare OR Artbas.Gjennomfaktureres bha 04.05.07 */
     ,FIRST VarebehLinje NO-LOCK OF VarebehLinjeTrans
            WHERE VarebehLinje.ArtikkelNr = VarebehLinjeTrans.ArtikkelNr
              AND (IF cLevNrListe NE "" THEN CAN-DO(cLevNrListe,STRING(VarebehLinje.LevNr)) ELSE TRUE)
      BREAK BY VarebehLinjeTrans.ArtikkelNr
            BY VarebehLinjeTrans.Levuke2:

    IF FIRST-OF(VarebehLinjeTrans.Levuke2) THEN 
      EMPTY TEMP-TABLE TT_VareBehBestLinje.

    bSendtBest = NO.
    FOR EACH ttSendt 
        WHERE ttSendt.fSendtArtikkelNr = VarebehLinjeTrans.Artikkelnr
          AND ttSendt.iLevUke          = VarebehLinjeTrans.LevUke2:
      IF CAN-FIND(FIRST BestLinje WHERE BestLinje.BestNr = ttSendt.iSendtBestNr
                                    AND BestLinje.Butik  = VarebehLinjeTrans.ButikkNr) THEN
        bSendtBest = YES.
    END.

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

      IF NOT CAN-DO(cButListe,STRING(Butiker.butik)) THEN
        cButListe = cButListe + STRING(Butiker.butik) + ",".
  
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
             AND (IF cArtikkelNr NE "0" THEN VarebehLinjeTrans.ArtikkelNr = DEC(cArtikkelNr) ELSE TRUE)
             AND VarebehLinjeTrans.Bestilt3  NE 0
             AND VarebehLinjeTrans.GodkjentBestilling
     ,FIRST Artbas NO-LOCK OF VarebehLinjeTrans
/*             WHERE Artbas.Kjedevare OR Artbas.Gjennomfaktureres bha 04.05.07 */
     ,FIRST VarebehLinje NO-LOCK OF VarebehLinjeTrans
            WHERE VarebehLinje.ArtikkelNr = VarebehLinjeTrans.ArtikkelNr
              AND (IF cLevNrListe NE "" THEN CAN-DO(cLevNrListe,STRING(VarebehLinje.LevNr)) ELSE TRUE)
      BREAK BY VarebehLinjeTrans.ArtikkelNr
            BY VarebehLinjeTrans.Levuke3:

    IF FIRST-OF(VarebehLinjeTrans.Levuke3) THEN 
      EMPTY TEMP-TABLE TT_VareBehBestLinje.

    bSendtBest = NO.
    FOR EACH ttSendt 
        WHERE ttSendt.fSendtArtikkelNr = VarebehLinjeTrans.Artikkelnr
          AND ttSendt.iLevUke          = VarebehLinjeTrans.LevUke3:
      IF CAN-FIND(FIRST BestLinje WHERE BestLinje.BestNr = ttSendt.iSendtBestNr
                                    AND BestLinje.Butik  = VarebehLinjeTrans.ButikkNr) THEN
        bSendtBest = YES.
    END.

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

      IF NOT CAN-DO(cButListe,STRING(Butiker.butik)) THEN
        cButListe = cButListe + STRING(Butiker.butik) + ",".
  
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
             AND (IF cArtikkelNr NE "0" THEN VarebehLinjeTrans.ArtikkelNr = DEC(cArtikkelNr) ELSE TRUE)
             AND VarebehLinjeTrans.Bestilt4 NE 0
             AND VarebehLinjeTrans.GodkjentBestilling
     ,FIRST Artbas NO-LOCK OF VarebehLinjeTrans
/*             WHERE Artbas.Kjedevare OR Artbas.Gjennomfaktureres bha 04.05.07 */
     ,FIRST VarebehLinje NO-LOCK OF VarebehLinjeTrans
            WHERE VarebehLinje.ArtikkelNr = VarebehLinjeTrans.ArtikkelNr
              AND (IF cLevNrListe NE "" THEN CAN-DO(cLevNrListe,STRING(VarebehLinje.LevNr)) ELSE TRUE)
      BREAK BY VarebehLinjeTrans.ArtikkelNr
            BY VarebehLinjeTrans.Levuke4:

    IF FIRST-OF(VarebehLinjeTrans.Levuke4) THEN 
      EMPTY TEMP-TABLE TT_VareBehBestLinje.

    bSendtBest = NO.
    FOR EACH ttSendt 
        WHERE ttSendt.fSendtArtikkelNr = VarebehLinjeTrans.Artikkelnr
          AND ttSendt.iLevUke          = VarebehLinjeTrans.LevUke4:
      IF CAN-FIND(FIRST BestLinje WHERE BestLinje.BestNr = ttSendt.iSendtBestNr
                                    AND BestLinje.Butik  = VarebehLinjeTrans.ButikkNr) THEN
        bSendtBest = YES.
    END.

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
    
      IF NOT CAN-DO(cButListe,STRING(Butiker.butik)) THEN
        cButListe = cButListe + STRING(Butiker.butik) + ",".
  
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

  IF bSend THEN 
    FOR EACH Ordre NO-LOCK
        WHERE Ordre.VarebehNr = fVarebehNr
          AND Ordre.CL        = INT(ENTRY(ix,cCLlist)):
      RUN ordre_sett_status.p("sendt;" + cUserId + ";" + STRING(Ordre.Ordrenr),
                               ?,
                               icSessionId,
                               OUTPUT ocReturn,
                               OUTPUT obOk).
    END.

END.

DO TRANSACTION:
  FIND VarebehHode WHERE VarebehHode.VarebehNr = fVarebehNr
       EXCLUSIVE-LOCK NO-ERROR.
  IF AVAIL VarebehHode THEN 
    VarebehHode.ButikkListe = TRIM(cButListe,",").
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

   RETURN.

/*    OUTPUT TO c:\tmp\brynjar\ttbest90000010.csv APPEND. */
/*    FOR EACH TT_VareBehBestLinje:                       */
/*      EXPORT DELIMITER ";"                              */
/*             TT_VareBehBestLinje.VareBehNr              */
/*             TT_VareBehBestLinje.CLButikkNr             */
/*             TT_VareBehBestLinje.BestiltButikkNr        */
/*             TT_VareBehBestLinje.HodeLinjeId            */
/*             TT_VareBehBestLinje.Bestilt                */
/*             TT_VareBehBestLinje.Storl                  */
/*             STRING(fVarebehNr)                         */
/*             ENTRY(ix,cCLlist)                          */
/*             STRING(VarebehLinjeTrans.Levuke1)          */
/*             STRING(VarebehLinje.ArtikkelNr)            */
/*             STRING(VarebehLinje.Levnr)                 */
/*             .                                          */
/*    END.                                                */
/*    OUTPUT CLOSE.                                       */
   
END PROCEDURE.
