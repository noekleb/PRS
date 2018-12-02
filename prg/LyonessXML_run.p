DEFINE VARIABLE dRunDate     AS DATE        NO-UNDO.
DEFINE VARIABLE dLastRunDate AS DATE        NO-UNDO.
DEFINE VARIABLE cLastRundate AS CHARACTER   NO-UNDO.
{syspara.i 210 262 5 cLastRundate}

IF cLastRundate = "" THEN
    RETURN.
dLastRunDate = DATE(INT(ENTRY(2,cLastRundate,"-")),
                    INT(ENTRY(3,cLastRundate,"-")),
                    INT(ENTRY(1,cLastRundate,"-"))) NO-ERROR.
IF ERROR-STATUS:ERROR THEN
    RETURN.
DO dRundate = dLastRunDate + 1 TO TODAY - 1:
    RUN LyonessXML.p (dRundate).
    dLastRunDate = dRunDate.
    PAUSE 10 NO-MESSAGE.
END.

cLastRundate = STRING(YEAR(dLastRunDate),"9999") + "-" +
               STRING(MONTH(dLastRunDate),"99")  + "-" +
               STRING(DAY(dLastRunDate),"99").

{setsyspara.i 210 262 5 cLastRundate}
QUIT.
