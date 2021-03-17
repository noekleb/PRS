
CURRENT-WINDOW:WIDTH = 300.

DEF VAR cFilNavn AS CHAR NO-UNDO.
DEF VAR cTekst AS CHAR FORMAT "x(40)" NO-UNDO.
DEF VAR cEAN AS CHAR NO-UNDO.
DEF VAR cERPNr AS CHAR NO-UNDO.


ASSIGN
    cFilNavn = 'c:\polygon\EAN_ERPNr.csv'
    .

DEF STREAM Inn.
DEF STREAM Avvist.
DEF STREAM Ut.

INPUT STREAM Inn FROM VALUE(cFilNavn).

OUTPUT STREAM Ut TO VALUE('c:\polygon\EAN_ERPNr2.csv') CONVERT TARGET 'ibm850'.

REPEAT :
    IMPORT STREAM Inn UNFORMATTED cTekst.
    IF NUM-ENTRIES(cTekst,';') < 2 THEN NEXT.
    ASSIGN
        cEAN   = entry(1,cTekst,';')
        cERPNr = entry(2,cTekst,';')
        .

    PUT STREAM Ut UNFORMATTED cEAN ';' cERPNr SKIP.
    /*cTekst = CODEPAGE-CONVERT(cTekst, SESSION:CHARSET, "UTF-32").*/

END.

