DEFINE VARIABLE dFraDato    AS DATE NO-UNDO.
DEFINE VARIABLE dTilDato    AS DATE NO-UNDO.
DEFINE VARIABLE cMinusDar   AS CHARACTER  NO-UNDO.
DO:
    ASSIGN cMinusDar = "6,0,1,2,3,4,5"
           dFraDato  =  TODAY - 7
           dFraDato  = dFraDato - INT(ENTRY(WEEKDAY(dFradato),cMinusDar))
           dTilDato  = dFraDato + 6.
    RUN eksportACNielsenMenigo.p (dFraDato,dTilDato).
END.
QUIT.
