DEF VAR cLinje AS CHAR NO-UNDO.

DEF VAR cFilNAvn AS CHAR NO-UNDO.
DEF STREAM inn.
DEF VAR cFarg AS CHAR NO-UNDO.
DEF VAR cFarBeskr AS CHAR NO-UNDO.
DEF VAR iAnt AS INT NO-UNDO.       

INPUT STREAM Inn FROM VALUE('C:\ArkivDokument\Kunder\GANT\Boomerang\Prisfila_stepp_2.csv').

DEF BUFFER bufHuvGr FOR HuvGr.

REPEAT :
    IMPORT STREAM Inn UNFORMATTED
        cLinje.

    ASSIGN
        cFarg = TRIM(ENTRY(5,cLinje,';'))
        .
    DISPLAY 
        cFArg
        .

    FIND LAST bufHuvGr NO-LOCK NO-ERROR.
    IF AVAILABLE bufHuvGr 
        THEN iAnt = bufHuvGr.Hg + 1.
    ELSE iAnt = 1.

    IF NOT CAN-FIND(FIRST HuvGr WHERE 
                    HuvGr.HgBeskr = cFarg) THEN
    DO:
        CREATE HuvGr.
        ASSIGN
            HuvGr.Hg      = iant
            HuvGr.HgBeskr = cFarg
            HuvGr.AvdelingNr = 1
            .
    END.
END.
