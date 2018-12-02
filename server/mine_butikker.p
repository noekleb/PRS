/* Henter mine butikker 
   Parametere:  Brukerid,teamtype (1: Ordre, 2: Rapport)
   
   Opprettet: 15.10.04 av BHa                  
   Endret:    09.07.07 av BHa
            - Hvis brukertype 2 så returneres butikknr knyttet til bruker
-----------------------------------------------------------------------------------*/

DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

FIND bruker WHERE bruker.brukerid = ENTRY(1,icParam) NO-LOCK NO-ERROR.
IF NOT AVAIL bruker THEN DO:
  ocReturn = "Finner ikke bruker".
  RETURN.
END.

IF Bruker.BrukerType = 2 THEN DO:
  IF bruker.ButikkNr NE 0 THEN
    ocReturn = STRING(Bruker.ButikkNr).
  ELSE ocReturn = "Bruker " + Bruker.BrukerID + " er 'Butikk bruker' og mangler butikknr".
END.
ELSE
  RUN setbutikkliste.p (bruker.brukerid,OUTPUT ocReturn).

/* IF Bruker.Brukertype NE 1 THEN                                 */
/*   FOR EACH ButikkTeam NO-LOCK                                  */
/*       WHERE ButikkTeam.BrGrpNr    = Bruker.BrGrpNr             */
/*         AND ButikkTeam.TeamTypeId = INT(ENTRY(2,icParam)):     */
/*   FOR EACH ButikkKobling OF ButikkTeam NO-LOCK:                */
/*     IF NOT CAN-DO(ocReturn,STRING(ButikkKobling.butik)) THEN   */
/*       ocReturn = ocReturn + STRING(ButikkKobling.butik) + ",". */
/*   END.                                                         */
/* END.                                                           */

ocReturn = TRIM(ocReturn,",").

IF ocReturn NE "" THEN obOk = TRUE.
