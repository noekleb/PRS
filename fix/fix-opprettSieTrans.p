DEF VAR cButLst AS CHAR  NO-UNDO.
DEF VAR iButNr AS INT NO-UNDO.
DEF VAR iLoop AS INT NO-UNDO.

DEF BUFFER bufSieTranstype FOR SieTransType.

CURRENT-WINDOW:WIDTH = 350.

ASSIGN 
    cButLst = '2,3,4,5,6,9,10,11,12,13,15,40'
    cbutLst = '15'
    .
FIND sieTranstype WHERE 
    sieTranstype.Butik = 0 AND 
    sieTranstype.TTId = 52 AND 
    sieTranstype.tbid = 4.

DISPLAY
    Butik ttid tbid kontonr
    WITH WIDTH 350.

IF sietranstype.butik = 0 THEN
DO iLoop = 1 TO NUM-ENTRIES(cButLst):
    CREATE bufSieTransType.
    BUFFER-COPY 
        sietransType
        EXCEPT Butik
        TO bufSieTransType
        ASSIGN
            bufSieTransType.Butik = INT(ENTRY(iLoop,cButLst)).
END.
ELSE UPDATE sietranstype.kontonr WITH WIDTH 350.

