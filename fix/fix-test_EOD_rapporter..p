DEF VAR iButNr AS INT NO-UNDO.
DEF VAR dDato AS DATE NO-UNDO.
DEF VAR cFilNavn AS CHAR FORMAT "x(50)" NO-UNDO.

ASSIGN
    iButNr = 14
    dDato = TODAY - 1
    .

/*
/* Finansrapport */
RUN dagsrapp_utskrift.p ("1", iButNr, dDato, dDato, TRUE, OUTPUT cFilnavn)
*/

/* Bokføringsbilag */
RUN dagsrapp_utskrift.p ("2", iButNr, dDato, dDato, TRUE, OUTPUT cFilnavn).

MESSAGE cFilNavn
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
