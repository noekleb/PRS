/* Leser translogg for angitte butikker og oppretter plukkliste
   Opprettet: 31.03.2007 Tom Nøkleby
 -----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR   NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR   NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR   NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG    NO-UNDO.

DEF VAR iAntall     AS INT  NO-UNDO.
DEF VAR cButikkLst  AS CHAR NO-UNDO.
DEF VAR dFraDato    AS DATE NO-UNDO.
DEF VAR dTilDato    AS DATE NO-UNDO.
DEFINE VARIABLE dDato AS DATE NO-UNDO.
DEF VAR cAvdlingLst AS CHAR NO-UNDO.
DEF VAR cHuvGrLst   AS CHAR NO-UNDO.
DEF VAR cVarGrLst   AS CHAR NO-UNDO.
DEF VAR iLoop       AS INT  NO-UNDO.
DEF VAR iButikkLoop AS INT  NO-UNDO.
DEF VAR iVarGrLoop  AS INT  NO-UNDO.
DEF VAR lPlListeId  AS DEC  NO-UNDO.
DEF VAR iCL         AS INT  NO-UNDO.
DEF VAR lSumAnt     AS DEC  NO-UNDO.
DEFINE VARIABLE iArUke1 AS INTEGER NO-UNDO.
DEFINE VARIABLE iArUke2 AS INTEGER NO-UNDO.
DEFINE VARIABLE iUkeDag AS INTEGER NO-UNDO.
DEFINE VARIABLE iLevNr  AS INTEGER NO-UNDO.
DEFINE VARIABLE cMail   AS CHARACTER NO-UNDO.

DEF BUFFER bufPlListeLinje FOR plListeLinje.

ASSIGN
    cButikkLst  = ENTRY(1,icParam,"|") 
    dFraDato    = DATE(ENTRY(2,icParam,"|"))
    dTilDato    = DATE(ENTRY(3,icParam,"|"))
    obOk        = TRUE
    .

/*Validate*******************************/
DO iLoop = 1 TO NUM-ENTRIES(cButikkLst):
  IF INT(ENTRY(iLoop,cButikkLst)) = 0 THEN NEXT.
  IF NOT CAN-FIND(Butiker WHERE Butiker.Butik = INT(ENTRY(iLoop,cButikkLst))) THEN
  DO:
    ASSIGN
      ocReturn = ocReturn + (IF NOT ocReturn = '' THEN '\n' ELSE '') + 'FEIL: Kan ikke nullstille flagg på salgstransaksjonene. ' + ENTRY(iLoop,cButikkLst)
      obOk     = FALSE
    .
  END.
END.
IF NOT obOk THEN RETURN.
/***********************************Validate*/

/* Nullstiller */
obOk = FALSE.

/* Leser postene fra Translogg */
BUTIKKLOOP:
DO iButikkLoop = 1 TO NUM-ENTRIES(cButikkLst):
  IF INT(ENTRY(iButikkLoop,cButikkLst)) = 0 THEN NEXT.
  DATOLOOP:
  DO dDato = dFraDato TO dTilDato:

    TRANSLOGG:
    FOR EACH TransLogg EXCLUSIVE-LOCK WHERE
      TransLogg.Dato  = dDato AND
      TransLogg.TTId  = 1 AND
      TransLogg.Butik = INTEGER(ENTRY(iButikkLoop,cButikkLSt)):
      /* Flagger translogg posten som IKKE plukket */
      ASSIGN
        Translogg.Ordreforslag = FALSE.
 
    END. /* TRANSLOGG */
  END. /* DATOLOOP */
END. /* BUTIKKLOOP */

/* MAIN BLOKK */
ASSIGN 
  ocReturn = ''
  obOk     = TRUE
.
RETURN ocReturn.

