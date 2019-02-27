DEF VAR cLinje AS CHAR NO-UNDO.

DEF VAR cFilNAvn AS CHAR NO-UNDO.
DEF STREAM inn.
DEF VAR cFarg AS CHAR NO-UNDO.
DEF VAR cFarBeskr AS CHAR NO-UNDO.

INPUT STREAM Inn FROM VALUE('C:\ArkivDokument\Kunder\GANT\Boomerang\EAN_lista_EXCEL.csv').

REPEAT :
    IMPORT STREAM Inn UNFORMATTED
        cLinje.

    ASSIGN
        cFarg = ENTRY(2,cLinje,';')
        cFarBeskr = ENTRY(3,cLinje,';')
        .
    DISPLAY 
        cFArg
        cFarBeskr.

    IF NOT CAN-FIND(Farg WHERE 
                    Farg.Farg = INT(cFarg)) THEN
    DO:
        CREATE Farg.
        ASSIGN
            Farg.Farg = INT(cFarg)
            Farg.FarBeskr = cFarBeskr
            .
    END.
END.
