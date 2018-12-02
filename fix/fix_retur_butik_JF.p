DEFINE VARIABLE dDatum  AS DATE       NO-UNDO.
DEFINE VARIABLE iButik  AS INTEGER    NO-UNDO.
DEFINE VARIABLE iKassa  AS INTEGER    NO-UNDO.
DEFINE VARIABLE iBongNr AS INTEGER    NO-UNDO.
DEFINE VARIABLE iOrgBong AS INTEGER    NO-UNDO.
DEFINE VARIABLE iOrgbutik AS INTEGER    NO-UNDO.
DEFINE VARIABLE iOrgselger AS INTEGER    NO-UNDO.
DEFINE VARIABLE cStr AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cDatum AS CHARACTER  NO-UNDO.
DEFINE VARIABLE ii AS INTEGER    NO-UNDO.
INPUT FROM "D:\appdir\skotex\PRSupdate\Returbutikfiler\retur.txt".
OUTPUT TO "CLIPBOARD".
REPEAT :
    ii = ii + 1.
    IMPORT UNFORMATTED cStr.
    cStr = TRIM(cStr).
    IF cStr = "" THEN
        NEXT.
    cDatum     = ENTRY(1,cStr,";").
    dDatum     = DATE(INT(SUBSTR(cDatum,5,2)),INT(SUBSTR(cDatum,7,2)),INT(SUBSTR(cDatum,1,4))).
    iButik     = INT(ENTRY(2,cStr,";")).
    iKassa     = INT(ENTRY(3,cStr,";")).
    iBongNr    = INT(ENTRY(4,cStr,";")).
    iOrgBong   = INT(ENTRY(5,cStr,";")).
    iOrgButik  = INT(ENTRY(6,cStr,";")).
    iOrgSelger = INT(ENTRY(7,cStr,";")).

    FIND bonghode WHERE bonghode.butikknr = iButik AND
                        bonghode.gruppenr = 1      AND
                        bonghode.kassenr  = iKassa AND
                        bonghode.dato     = dDatum AND 
                        bonghode.bongnr   = iBongnr NO-LOCK NO-ERROR.
    IF AVAIL bonghode THEN DO:
        FOR EACH bonglinje WHERE bonglinje.b_id = bonghode.b_id:
             IF bonglinje.ttid = 10 THEN DO:
                ASSIGN bonglinje.RefNr         = iOrgBong
                       bonglinje.ReturButikk   = iOrgButik
                       bonglinje.ReturKasserer = iOrgSelger.
             END.
        END.
    END.
END.
INPUT CLOSE.

/* 20100305;1;1;274;3556;6;601 */
/* 20100414;3;1;101;4593;3;309 */
/* 20100414;3;2;66;4593;3;309  */
/* 20100420;5;1;342;317;5;507  */
/* 20100415;6;1;58;5338;6;627  */
/* 20100418;7;2;305;6937;7;704 */
/* 20100414;8;2;10;9516;8;896  */
/* 20100422;9;1;548;691;4;2501 */
/* 20100425;9;2;493;180;9;932  */

