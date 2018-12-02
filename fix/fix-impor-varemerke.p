DEF VAR cLinje AS CHAR NO-UNDO.

DEF VAR cFilNAvn AS CHAR NO-UNDO.
DEF STREAM inn.
DEF VAR cFarg AS CHAR NO-UNDO.
DEF VAR cFarBeskr AS CHAR NO-UNDO.

INPUT STREAM Inn FROM VALUE('C:\ArkivDokument\Kunder\GANT\Boomerang\Prisfila_stepp_2.csv').

REPEAT :
    IMPORT STREAM Inn UNFORMATTED
        cLinje.

    ASSIGN
        cFarg = ENTRY(2,cLinje,';')
        cFarg = REPLACE(cLinje,'USC','')
        cFarg = SUBSTRING(cFarg,3,1)
        .
    DISPLAY 
        cFArg
        .

    IF NOT CAN-FIND(Varemerke WHERE 
                    Varemerke.VmId  = INT(cFarg)) THEN
    DO:
        CREATE Varemerke.
        ASSIGN
            Varemerke.VmId = INT(cFarg)
            Varemerke.Beskrivelse  = cFarg
            .
    END.
END.
