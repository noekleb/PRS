/* fix-skrivOgSendDagsoppgjor.p */

DEF VAR cButLst AS CHAR NO-UNDO.
DEF VAR ibutNr AS INT NO-UNDO.
DEF VAR iLoop AS INT NO-UNDO.
DEF VAR dDato AS DATE NO-UNDO.
DEF VAR cFilNavn AS CHAR NO-UNDO.

ASSIGN 
    cButLst = '2,3,6,9,10,11,12,13,14,15,17,18,40'
    .

DO dDato = 08/10/2020 TO 08/11/2020:
    DO iLoop = 1 TO NUM-ENTRIES(cButLst):
        ibutNr = INT(ENTRY(iLoop,cButLst)).
      RUN dagsrapp_utskrift.p ("1", iButNr, dDato, dDato, TRUE, OUTPUT cFilnavn).
      RUN dagsrapp_utskrift.p ("2", iButNr, dDato, dDato, TRUE, OUTPUT cFilnavn).
    END.
END.

