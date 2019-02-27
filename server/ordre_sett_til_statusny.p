/* Sett ordrestatus tilbake til NY 
   Parametere: 
               - statustekst;brukerid;liste over ordrenr
   
   Opprettet: 16.03.07 av BHa              
   Endret:    08.08.07 av BHa
              - Fjernet feilhåndtering
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR cOrdreNrList AS CHAR NO-UNDO.
DEF VAR ix           AS INT  NO-UNDO.
DEF VAR bIkkeRor     AS LOG  NO-UNDO.

cOrdreNrList = REPLACE(icParam,"|",",").

/* SettNy:                              */
/* DO TRANSACTION ON ERROR UNDO,LEAVE:  */
  DO ix = 1 TO NUM-ENTRIES(cOrdreNrList):
    FIND Ordre EXCLUSIVE-LOCK
         WHERE Ordre.OrdreNr = INT(ENTRY(ix,cOrdreNrList)) 
         NO-ERROR.
    IF AVAIL Ordre THEN DO:
      bIkkeRor = NO.
      FOR EACH BestHode EXCLUSIVE-LOCK 
          WHERE BestHode.OrdreNr  = Ordre.OrdreNr:
/*         IF BestHode.BekreftetDato NE ? THEN DO:                                                                                       */
/*           ocReturn = "Ordre " + ENTRY(ix,cOrdreNrList) + " er bekreftet. Kan ikke settes tilbake til NY. Hele endringen kanselleres". */
/*           UNDO, LEAVE SettNy.                                                                                                         */
/*         END.                                                                                                                          */
/*         IF BestHode.BestStat > 4 THEN DO:                                                                                             */
/*           ocReturn = "Ordre " + ENTRY(ix,cOrdreNrList) + " er mottatt. Kan ikke settes tilbake til NY. Hele endringen kanselleres".   */
/*           UNDO, LEAVE SettNy.                                                                                                         */
/*         END.                                                                                                                          */
        IF BestHode.BekreftetDato = ? AND BestHode.BestStat < 5 THEN
          RUN bytbeststatus.p (RECID(BestHode),"-",Ordre.OrdreNr).
        ELSE bIkkeRor = YES.
      END.
      IF NOT bIkkeRor THEN
        ASSIGN Ordre.OrdreStatus = 1
               Ordre.SendtDato   = ?.
    END.
/*     ELSE DO:                                                */
/*       ocReturn = "Ordre ikke tilgjengelig for oppdatering". */
/*       UNDO, LEAVE SettNy.                                   */
/*     END.                                                    */
  END.
/* END. */

IF ocReturn = "" THEN 
  obOk = TRUE.

