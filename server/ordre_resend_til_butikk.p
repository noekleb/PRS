/* Resending av ordre til butikk 
   Parametere: 
               - statustekst;brukerid;liste over ordrenr
   
   Opprettet: 23.05.08 av TN              
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR cOrdreNrList AS CHAR NO-UNDO.
DEF VAR ix           AS INT  NO-UNDO.

ASSIGN cOrdreNrList = REPLACE(ENTRY(3,icParam,";"),"|",",")
       .

DO ix = 1 TO NUM-ENTRIES(cOrdreNrList) TRANSACTION:
  FIND Ordre EXCLUSIVE-LOCK
       WHERE Ordre.OrdreNr = INT(ENTRY(ix,cOrdreNrList)) 
       NO-ERROR.
  IF AVAIL Ordre THEN DO:
    IF Ordre.BekreftetOrdre THEN
    DO FOR ELOGG:
        FIND ELogg EXCLUSIVE-LOCK WHERE 
             ELogg.TabellNavn     = "OrdHK" AND
             ELogg.EksterntSystem = "POS"    AND
             ELogg.Verdier        = STRING(Ordre.OrdreNr) NO-ERROR.
        IF NOT AVAIL Elogg THEN DO:
            CREATE Elogg.
            ASSIGN ELogg.TabellNavn     = "OrdHK"
                   ELogg.EksterntSystem = "POS"   
                   ELogg.Verdier        = STRING(Ordre.OrdreNr).
        END.
        ASSIGN ELogg.EndringsType = 1
               ELogg.Behandlet    = FALSE.
        RELEASE ELogg.
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

