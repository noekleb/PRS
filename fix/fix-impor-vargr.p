DEF VAR cLinje AS CHAR NO-UNDO.

DEF VAR cFilNAvn AS CHAR NO-UNDO.
DEF STREAM inn.
DEF VAR cFarg AS CHAR NO-UNDO.
DEF VAR cFarBeskr AS CHAR NO-UNDO.
DEF VAR iAnt AS INT NO-UNDO.       
DEF VAR cVg AS CHAR NO-UNDO.

INPUT STREAM Inn FROM VALUE('C:\ArkivDokument\Kunder\GANT\Boomerang\Prisfila_stepp_2.csv').

DEF BUFFER bufHuvGr FOR HuvGr.

REPEAT :
    IMPORT STREAM Inn UNFORMATTED
        cLinje.

    ASSIGN
        cFarg = TRIM(ENTRY(5,cLinje,';'))
        cVg   = substring(TRIM(ENTRY(2,cLinje,';')),4,3)
        .
    DISPLAY 
        cFArg
        cVg
        .

    FIND FIRST bufHuvGr NO-LOCK WHERE 
        bufHuvGr.HgBeskr = cFarg NO-ERROR.

    IF NOT CAN-FIND(FIRST VarGr WHERE 
                    VarGr.Vg = int(cVg)) THEN
    DO:
        CREATE VarGr.
        ASSIGN
            VarGr.Vg = int(cVg)
            VarGr.VgBeskr = "** Automatisk opprettet"
            VarGr.Hg = bufHuvGr.Hg
            VarGr.Kost_Proc = 65
            VarGr.MomsKod = 1
            .
    END.
END.
