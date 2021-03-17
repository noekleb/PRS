/* SESSION-PARAMETER SKALL VARA "BUTIK=123" */
DEFINE VARIABLE dFraDato    AS DATE NO-UNDO.
DEFINE VARIABLE dTilDato    AS DATE NO-UNDO.
DEFINE VARIABLE cMinusDar   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iButik      AS INTEGER    NO-UNDO.

IF SESSION:PARAMETER BEGINS "BUTIK" AND NUM-ENTRIES(SESSION:PARAMETER,"=") = 2 THEN
    ASSIGN iButik = INT(ENTRY(2,SESSION:PARAMETER,"=")).
IF CAN-FIND(butiker WHERE butiker.butik = ibutik) THEN DO:
    ASSIGN cMinusDar = "6,0,1,2,3,4,5"
           dFraDato  = TODAY - 7
           dFraDato  = dFraDato - INT(ENTRY(WEEKDAY(dFradato),cMinusDar))
           dTilDato  = dFraDato + 6.
    RUN eksportvagabond.p (dFraDato,dTilDato,iButik).
END.
QUIT.
