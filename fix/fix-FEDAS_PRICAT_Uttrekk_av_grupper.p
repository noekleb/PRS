/*
  fix-FEDAS_PRICAT_Uttrekk_av_grupper.p
*/

DEF VAR cFil AS CHAR NO-UNDO.
DEF VAR cUtFil AS CHAR NO-UNDO.
DEF VAR cKatalog AS CHAR NO-UNDO.
DEF VAR cLinje AS CHAR FORMAT "x(40)" NO-UNDO.

DEF TEMP-TABLE tmpFedas
    FIELD VarGr AS CHAR FORMAT "x(10)"
    .

CURRENT-WINDOW:WIDTH = 350.

DEF STREAM Inn.
DEF STREAM Ut.

ASSIGN
    cFil     = 'BERGANS_TEKSTIL04102017.csv'
    cUtFil   = 'Fedas_grupper_BergansTekstil4102017.csv'
    cKatalog = 'D:\ArkivDokument\Kunder\Sport Norge\'
    .

INPUT STREAM Inn FROM VALUE(cKatalog + cFil).
OUTPUT STREAM Ut TO VALUE(cKatalog + cUtFil).

PUT STREAM Ut UNFORMATTED
    'Varemerke;Gruppe;Varetekst;Egen varegruppe'
    SKIP.

REPEAT:
    IMPORT STREAM Inn UNFORMATTED
        cLinje.

    /*
    DISPLAY
        ENTRY(3,cLinje,';') FORMAT "x(30)"
        ENTRY(4,cLinje,';') FORMAT "x(30)"
        ENTRY(10,cLinje,';') FORMAT "x(30)"
    WITH WIDTH 350.
    */

    IF NOT CAN-FIND(tmpFedas WHERE 
                    tmpFedas.Varg = ENTRY(4,cLinje,';')) THEN
    DO:
        CREATE tmpFedas.
        ASSIGN 
            tmpFedas.VarGr = ENTRY(4,cLinje,';')
            .
        PUT STREAM Ut UNFORMATTED
            ENTRY(3,cLinje,';') ';'
            ENTRY(4,cLinje,';') ';'
            ENTRY(10,cLinje,';') ';'
            SKIP.
    END.

    
END.
OUTPUT STREAM Ut CLOSE.
INPUT STREAM Inn CLOSE.
