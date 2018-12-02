/* Endre status på ordre 
   Parametere: 
               - statustekst;brukerid;liste over ordrenr
   
   Opprettet: 08.11.05 av BHa              
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR cStatus      AS CHAR NO-UNDO.
DEF VAR cOrdreNrList AS CHAR NO-UNDO.
DEF VAR ix           AS INT  NO-UNDO.

ASSIGN cStatus      = ENTRY(1,icParam,";")
       cOrdreNrList = REPLACE(ENTRY(3,icParam,";"),"|",",")
       .

DO ix = 1 TO NUM-ENTRIES(cOrdreNrList) TRANSACTION:
  FIND Ordre EXCLUSIVE-LOCK
       WHERE Ordre.OrdreNr = INT(ENTRY(ix,cOrdreNrList)) 
       NO-ERROR.
  IF AVAIL Ordre THEN DO:
    IF cStatus = "sendt" AND Ordre.OrdreStatus = 1 THEN DO:
      for each BestHode no-lock where
        BestHode.OrdreNr  = Ordre.OrdreNr and  
        BestHode.BestStat = 3: /* På orde --> Ordre sendt */
  
        run bytbeststatus.p (recid(BestHode),"+",Ordre.OrdreNr).
      end. /* SET_RADER_SOM_SENDT */ 
      ASSIGN Ordre.OrdreStatus = 2
             Ordre.SendtDato   = TODAY.
    END.
  END.
  IF AVAILABLE Ordre THEN
      RELEASE Ordre.
/*   ELSE DO:                                                */
/*     ocReturn = "Ordre ikke tilgjengelig for oppdatering". */
/*     UNDO, LEAVE.                                          */
/*   END.                                                    */
END.

IF ocReturn = "" THEN 
  obOk = TRUE.

