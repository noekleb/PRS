/* Endre lev.dato for ordre og tilhørende bestillinger.  
   Parametere: 
               - Transtype: "fullordre";userid;Pipe-separert liste av bestillinger eller liste av ordrenr
                    
   Opprettet: 19.05.06 av BHa
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
DEF VAR dNyLevDato       AS DATE  NO-UNDO.
DEF VAR cUserId          AS CHAR  NO-UNDO.
DEF VAR ix               AS INT   NO-UNDO.
  
ASSIGN cTransType = ENTRY(1,ENTRY(1,icParam,";"),"|")
       dNyLevDato = DATE(ENTRY(2,ENTRY(1,icParam,";"),"|"))
       cUserId    = ENTRY(2,icParam,";")
       .

DO ix = 1 TO NUM-ENTRIES(ENTRY(3,icParam,";"),"|"):
  FOR EACH BestHode NO-LOCK
      WHERE BestHode.OrdreNr = INT(ENTRY(ix,ENTRY(3,icParam,";"),"|")):
    cBestNrListe = cBestNrListe + STRING(BestHode.BestNr) + "|".
  END.
END.

cBestNrListe = TRIM(cBestNrListe,"|").

TRANSBLOKK:
DO ix = 1 TO NUM-ENTRIES(cBestNrListe,"|") TRANSACTION ON ERROR UNDO, LEAVE:                    

  FIND BestHode EXCLUSIVE-LOCK
       WHERE BestHode.BestNr = INT(ENTRY(ix,cBestNrListe,"|"))
       NO-ERROR.
  IF NOT AVAIL BestHode THEN DO:
    ocReturn = "Bestillingsnr " + ENTRY(ix,cBestNrListe) + " er ikke tilgjengelig for oppdatering" + CHR(10) + PROGRAM-NAME(1).
    UNDO, LEAVE TRANSBLOKK.
  END.

  IF BestHode.BestStat GE 4 THEN DO:
    ocReturn = 'Minst en ordre kunne ikke endres fordi den allerede er sendt'.
    UNDO, LEAVE TRANSBLOKK.
  END.

  BestHode.LevDato = dNyLevDato.

  FIND FIRST Ordre OF BestHode 
       EXCLUSIVE-LOCK NO-ERROR.
  IF AVAIL Ordre THEN
    Ordre.LeveringsDato = dNyLevDato.
  ELSE DO:
    ocReturn = "Ordrenr " + STRING(BestHode.Ordrenr) + " er ikke tilgjengelig for oppdatering" + CHR(10) + PROGRAM-NAME(1).
    UNDO, LEAVE TRANSBLOKK.
  END.

END. /* TRANSBLOKK TRANSACTION */

IF ocReturn = "" THEN 
  obOk = TRUE.

