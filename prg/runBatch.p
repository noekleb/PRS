DEFINE VARIABLE cPf       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cDb       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cPgm      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cCmd      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE ii        AS INTEGER     NO-UNDO.
DEFINE VARIABLE cPara     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cEntry    AS CHARACTER   NO-UNDO.
/* cPara = "DB:skotex:db\batchskotex.pf,PGM:POS_regdump.p,CMD:kom\ut\flyttareg.cmd". */
cPara = SESSION:PARAMETER.

DOO:
DO:
    DO ii = 1 TO NUM-ENTRIES(SESSION:PARAMETER):
        cEntry = ENTRY(ii,cPara).
        CASE ENTRY(1,cEntry,":"):
            WHEN "DB" THEN DO:
                IF NUM-ENTRIES(cEntry,":") = 3 THEN DO:
                    cDb = ENTRY(2,cEntry,":").
                    cPf = ENTRY(3,cEntry,":").
                    IF SEARCH(cPf) = ? THEN
                        LEAVE DOO.
                END.
                ELSE
                    LEAVE DOO.
            END.
            WHEN "PGM" THEN DO:
                IF NUM-ENTRIES(cEntry,":") = 2 THEN DO:
                    cPgm = ENTRY(2,cEntry,":").
                    IF SEARCH(cPgm) = ? THEN
                        LEAVE DOO.
                END.
            END.
            WHEN "CMD" THEN DO:
                IF NUM-ENTRIES(cEntry,":") = 2 THEN DO:
                    cCmd = ENTRY(2,cEntry,":").
                    IF SEARCH(cCmd) = ? THEN
                        LEAVE DOO.
                END.
            END.
        END CASE.
    END.
    RUN connectdb.p (cPf).
    IF CONNECTED(cDb) THEN
        RUN VALUE(cPgm).
    IF cCmd <> "" THEN
        OS-COMMAND SILENT VALUE(cCmd).
END.

QUIT.
