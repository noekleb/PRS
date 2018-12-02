/* Bekreft ordre og/eller bestilling.  
   Parametere: 
               - Transtype: "fullbest" eller "fullordre";userid;Pipe-separert liste av bestillinger eller liste av ordrenr
                    
   Opprettet: 09.11.05 av BHa                  
   Endret:    19.05.06 av BHa
            - Melding gis dersom ordre ikke kunne bekreftes fordi den ikke var sent eller fordi den allerede var bekreftet
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
DEF VAR cOrdreNrListe    AS CHAR  NO-UNDO. /* TN 25/6-06 */
DEF VAR dBekreftet       AS DATE  NO-UNDO.
DEF VAR cUserId          AS CHAR  NO-UNDO.
DEF VAR ix               AS INT   NO-UNDO.
  
/* TN 25/6-06 Feil håndtering av Entry. Var satt opp som streng i streng. Noe som var feil. */
/* Det sendes bare en liste fra avsendende program.                                         
ASSIGN cTransType = ENTRY(1,icParam,";")
       dBekreftet = DATE(ENTRY(2,icParam,";"))
       cUserId    = ENTRY(2,icParam,";")                                                    
       .                                                                                    */
/* BHa 12.10.06 Lagt tilbake orginal kode: */
ASSIGN cTransType = ENTRY(1,ENTRY(1,icParam,";"),"|")
       dBekreftet = DATE(ENTRY(2,ENTRY(1,icParam,";"),"|"))
       cUserId    = ENTRY(2,icParam,";")
       .

/* Kommer fra bekreftelse av bestillingsrader. */
IF cTransType MATCHES "*best" THEN cBestNrListe = ENTRY(3,icParam,";").
/* Kommer fra bekreftelse av ordrehoder, og vi må finne alle bestillingene på ordren. */
ELSE DO:
  ASSIGN cOrdreNrListe = ENTRY(3,icParam,";")
         cBestNrListe  = ""
         .
  DO ix = 1 TO NUM-ENTRIES(cOrdreNrListe,"|"):
    FOR EACH BestHode NO-LOCK
        WHERE BestHode.OrdreNr = INT(ENTRY(ix,cOrdreNrListe,"|")):
      cBestNrListe = cBestNrListe + STRING(BestHode.BestNr) + "|".
    END.
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
  /* TN 25/6-06 - Allerede bekreftet */
  IF BestHode.BekreftetOrdre THEN
      NEXT TRANSBLOKK.

  IF BestHode.BestStat < 4 OR BestHode.BekreftetDato NE ? THEN DO:
    ocReturn = 'Minst en bestilling kunne ikke bekreftes fordi den ikke hadde status "Sendt"'.
    NEXT.
  END.

  ASSIGN BestHode.BekreftetOrdre = TRUE
         BestHode.BekreftetDato  = dBekreftet
         BestHode.BekreftetAv    = cUserId
         .

END. /* TRANSBLOKK TRANSACTION */

IF ocReturn = "" THEN 
  obOk = TRUE.

