DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

/* Alternativ måte med dynamisk buffer 
DEF VAR bh AS HANDLE NO-UNDO.
bh = BUFFER tellehode:HANDLE. 

bh:FIND-BY-ROWID(TO-ROWID(ENTRY(1,icParam,'|'))).
IF bh:AVAILABLE THEN
DO:
    
END.
obOk = ocReturn = ''.
*/

/* .---------
DEF VAR wToday    AS DATE   NO-UNDO.
DEF VAR wNedskriv AS LOG    NO-UNDO.

  DEF VAR piAntallPar     AS DEC  NO-UNDO.
  DEF VAR piAntallTalt    AS DEC  NO-UNDO.
  DEF VAR piOpptVerdi     AS DEC  NO-UNDO.
  DEF VAR piVerdiDiff     AS DEC  NO-UNDO.
  DEF VAR piAntallDiff    AS DEC  NO-UNDO.
  DEF VAR piOpprVerdi     AS DEC  NO-UNDO.
  DEF VAR piAntLinjer     AS DEC  NO-UNDO.
  DEF VAR plOppdatert     AS DATE NO-UNDO.
  DEF VAR piAntallNegDiff AS DEC NO-UNDO.
  DEF VAR piAntallPosDiff AS DEC NO-UNDO.
  DEF VAR piVerdiNegDiff  AS DEC NO-UNDO.
  DEF VAR piVerdiPosDiff  AS DEC NO-UNDO.

  DEF BUFFER bTelleHode FOR TelleHode.

  FIND bTelleHode no-lock WHERE rowid(bTelleHode) = TO-ROWID(ENTRY(1,icParam,'|')) NO-ERROR.

  IF AVAIL bTelleHode THEN
  DO:
    assign
        piAntallPar     = 0
        piAntallTalt    = 0
        piOpptVerdi     = 0
        piVerdiDiff     = 0
        piAntallDiff    = 0
        piOpprVerdi     = 0
        piAntLinjer     = 0
        piAntallNegDiff = 0
        piAntallPosDiff = 0
        piVerdiNegDiff  = 0
        piVerdiPosDiff  = 0
        .

    /* Sumerer opp linjene. */
    FOR EACH TelleLinje of bTelleHode no-lock:
      assign
        piAntallPar  = piAntallPar  + TelleLinje.AntallPar
        piAntallTalt = piAntallTalt + TelleLinje.AntallTalt
        piOpprVerdi  = piOpprVerdi  + TelleLinje.OpprVerdi      
        piOpptVerdi  = piOpptVerdi  + TelleLinje.OpptVerdi
        piAntLinjer  = piAntLinjer  + 1      
        .
      /* Sumerer positive differanser. */
      IF (TelleLinje.AntallPar - TelleLinje.AntallTalt) < 0 THEN
          ASSIGN
          piAntallNegDiff = piAntallNegDiff + (TelleLinje.AntallPar - TelleLinje.AntallTalt)
          piVerdiNegDiff  = piVerdiNegDiff  + (TelleLinje.OpprVerdi - TelleLinje.OpptVerdi).
      /* Sumerer negative differanser. */
      IF (TelleLinje.AntallPar - TelleLinje.AntallTalt) > 0 THEN
          ASSIGN
          piAntallPosDiff = piAntallPosDiff + (TelleLinje.AntallPar - TelleLinje.AntallTalt)
          piVerdiPosDiff  = piVerdiPosDiff  + (TelleLinje.OpprVerdi - TelleLinje.OpptVerdi).
    END.
    /* Differanser */
    ASSIGN
        piVerdiDiff  = piOpprVerdi  - piOpptVerdi
        piAntallDiff = piAntallPar  - piAntallTalt
        .      

    DO TRANSACTION:
      FIND CURRENT bTelleHode EXCLUSIVE-LOCK NO-ERROR.
      IF AVAIL bTelleHode THEN
      DO:
        ASSIGN
            bTelleHode.AntallPar  = piAntallPar  
            bTelleHode.AntallTalt = piAntallTalt 
            bTelleHode.OpptVerdi  = piOpptVerdi  
            bTelleHode.VerdiDiff  = piVerdiDiff  
            bTelleHode.AntallDiff = piAntallDiff 
            bTelleHode.OpprVerdi  = piOpprVerdi  
            bTelleHode.AntLinjer  = piAntLinjer.
        /* Skal ikke sumeres for lokasjonslister. */
        IF bTelleHode.TelleType = 1 THEN
            ASSIGN
            bTelleHode.AntallNegDiff = piAntallNegDiff
            bTelleHode.AntallPosDiff = piAntallPosDiff
            bTelleHode.VerdiNegDiff  = piVerdiNegDiff
            bTelleHode.VerdiPosDiff  = piVerdiPosdiff
            .

/*         MESSAGE                                                          */
/*             "bTelleHode.AntallPar     " bTelleHode.AntallPar  SKIP       */
/*             "bTelleHode.AntallTalt    " bTelleHode.AntallTalt SKIP       */
/*             "bTelleHode.OpptVerdi     " bTelleHode.OpptVerdi  SKIP       */
/*             "bTelleHode.VerdiDiff     "  bTelleHode.VerdiDiff  SKIP      */
/*             "bTelleHode.AntallDiff    " bTelleHode.AntallDiff   SKIP     */
/*             "bTelleHode.OpprVerdi     " bTelleHode.OpprVerdi     SKIP    */
/*             "bTelleHode.AntLinjer     " bTelleHode.AntLinjer     SKIP    */
/*             "bTelleHode.TelleType      " bTelleHode.TelleType      SKIP  */
/*             "bTelleHode.AntallNegDiff " bTelleHode.AntallNegDiff SKIP    */
/*             "bTelleHode.AntallPosDiff " bTelleHode.AntallPosDiff SKIP    */
/*             "bTelleHode.VerdiNegDiff  " bTelleHode.VerdiNegDiff  SKIP    */
/*             "bTelleHode.VerdiPosDiff  " bTelleHode.VerdiPosDiff  SKIP    */
/*             VIEW-AS ALERT-BOX INFO BUTTONS OK.                           */

      END.
    END. /*Transaction*/
    RELEASE bTelleHode.
  END.
*/    

