/* Makulering av ordre og/eller bestilling.  
   Parametere: 
               - Transtype: "fullbest" eller "fullordre";userid;Pipe-separert liste av bestillinger eller liste av ordrenr
                    
   Opprettet: 09.11.05 av BHa                  
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.
  
DEF VAR wLevert          AS DEC   NO-UNDO.
DEF VAR fTotMakulert     AS DEC   NO-UNDO.
DEF VAR cTransType       AS CHAR  NO-UNDO.
DEF VAR cBestNrListe     AS CHAR  NO-UNDO.
DEF VAR cUserId          AS CHAR  NO-UNDO.
DEF VAR ix               AS INT   NO-UNDO.
  
DEF BUFFER bBestHode FOR BestHode.

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

cBestNrListe = TRIM(cBestNrListe,"|").

TRANSBLOKK:
DO ix = 1 TO NUM-ENTRIES(cBestNrListe,"|") TRANSACTION ON ERROR UNDO, LEAVE:                    

  FIND BestHode NO-LOCK
       WHERE BestHode.BestNr = INT(ENTRY(ix,cBestNrListe,"|"))
       NO-ERROR.
  IF NOT AVAIL BestHode THEN DO:
    ocReturn = "Bestillingsnr " + ENTRY(ix,cBestNrListe) + " er ikke tilgjengelig for oppdatering" + CHR(10) + PROGRAM-NAME(1).
    UNDO, LEAVE TRANSBLOKK.
  END.

  IF BestHode.BestStat = 9 THEN NEXT.

  CREATE BestHLev.
  ASSIGN BestHLev.BestNr       = BestHode.BestNr
         BestHLev.LeveringsNr  = NEXT-VALUE(LeveringsNr)
         BestHLev.LevertDato   = TODAY
         BestHLev.LevTidspunkt = time
         BestHLev.LevertAv     = cUserid
         fTotMakulert          = 0
         .


  FOR EACH BestStr EXCLUSIVE-LOCK 
      WHERE BestStr.BestNr   = BestHode.BestNr 
        AND BestStr.BestStat = BestHode.BestStat:

    BestStr.BestStat = 9.

    FIND LAST BestLevert NO-LOCK
         WHERE BestLevert.BestNr = BestHode.BestNr
           AND BestLevert.Butik  = BestStr.Butik
           AND BestLevert.Storl  = BestStr.Storl
         NO-ERROR.
    IF AVAIL BestLevert THEN 
      wLevert = BestLevert.Rest.
    ELSE wLevert = BestStr.Bestilt.

    CREATE BestLevert.
    ASSIGN BestLevert.BestNr      = BestHLev.BestNr
           BestLevert.Butik       = BestStr.Butik
           BestLevert.Storl       = BestStr.Storl
           BestLevert.LeveringsNr = BestHLev.LeveringsNr
           BestLevert.Rest        = wLevert
           BestLevert.LevertAv    = cUserid 
           BestLevert.LevertDato  = TODAY
           BestLevert.Avskrevet   = YES
           fTotMakulert           = fTotMakulert + wLevert
           .
  END.

  FOR EACH BestPris EXCLUSIVE-LOCK 
      WHERE BestPris.BestNr   = BestHode.BestNr 
        AND BestPris.BestStat = BestHode.BestStat:

    BestPris.BestStat = 9.
  END.

  FIND CURRENT BestHode EXCLUSIVE NO-ERROR.
  ASSIGN BestHode.BestStat    = 9
         BestHode.TotMakulert = BestHode.TotMakulert + fTotMakulert.
  FIND CURRENT BestHode NO-LOCK NO-ERROR.
        
  RUN SettOrdreStatus (BestHode.OrdreNr).
END. /* TRANSBLOKK TRANSACTION */

IF ocReturn = "" THEN 
  obOk = TRUE.

PROCEDURE SettOrdreStatus:
  DEF INPUT PARAM iiOrdreNr AS INT NO-UNDO.

  DEF VAR bSettStat AS LOG NO-UNDO INIT TRUE.

  FIND Ordre WHERE Ordre.OrdreNr = iiOrdreNr NO-LOCK NO-ERROR.
  IF Ordre.OrdreStatus = 9 THEN RETURN.

  FOR EACH bBestHode NO-LOCK
      WHERE bBestHode.OrdreNr = iiOrdreNr:
    IF bBestHode.BestStat NE 9 THEN DO:
      bSettStat = FALSE.
      LEAVE.
    END.
  END.
  IF bSettStat THEN DO:
    FIND CURRENT Ordre EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL Ordre THEN Ordre.OrdreStatus = 9.
  END.
END.
