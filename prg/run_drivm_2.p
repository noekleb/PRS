DEFINE VARIABLE dFraDato AS DATE    NO-UNDO.
DEFINE VARIABLE dTilDato AS DATE    NO-UNDO.
DEFINE VARIABLE cEmailTo AS CHARACTER   NO-UNDO.
DEFINE VARIABLE dDat1 AS DATE        NO-UNDO.
DEFINE VARIABLE dTmpDato AS DATE        NO-UNDO.
  
{syspara.i 210 272 10 cEmailTo}

    /* 1-15 */
    /* Vi måste testa körningsdag att dagens datum inte är mindre än 15 */
    /* då måste vi ta förra månaden */
IF DAY(TODAY) < 16 THEN DO:
    /* hämta sista dagen i förra månaden */
    dTmpDato = DATE(MONTH(TODAY),1,YEAR(TODAY)) - 1.
    /* Sätt startdatum till 1:a i den månaden */
    dFraDato = DATE(MONTH(dTmpDato),1,YEAR(dTmpDato)).
    /* sätt tilldatum till 15:e */
    dTilDato = DATE(MONTH(dTmpDato),15,YEAR(dTmpDato)).
END.
ELSE DO:
    /* Sätt startdatum till 1:a i den månaden */
    dFraDato = DATE(MONTH(TODAY),1,YEAR(TODAY)).
    /* sätt tilldatum till 15:e */
    dTilDato = DATE(MONTH(TODAY),15,YEAR(TODAY)).
END.
RUN drivmrapp_bfd_23.p (cEmailTo,2,dFraDato,dTilDato).

QUIT.
