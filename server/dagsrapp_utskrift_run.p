DEFINE VARIABLE dDato AS DATE    NO-UNDO.
DEFINE VARIABLE cIdag AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cFil  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cInitSidor AS CHARACTER INIT "1,2,3,4"  NO-UNDO.
DEFINE VARIABLE cSidor AS CHARACTER  NO-UNDO.
ASSIGN dDato = TODAY - 1.
       cIdag = STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99").

FOR EACH butiker NO-LOCK WHERE Butiker.EODRapporter = TRUE:
    ASSIGN cSidor = cInitSidor.
    FIND Syspara WHERE SysPara.SysHId = 210 AND
                       SysPara.SysGr  = 270 AND
                       SysPara.ParaNr = butiker.butik NO-LOCK NO-ERROR.
    IF AVAIL SysPara AND TRIM(SysPara.Parameter1) <> "" THEN
        cSidor = SysPara.Parameter1.
    IF Butiker.RAPPrinter <> "" AND CAN-DO(SESSION:GET-PRINTERS(),Butiker.RAPPrinter) THEN DO:
        IF CAN-FIND(FIRST kas_rap WHERE kas_rap.dato = dDato AND kas_rap.butikk = butiker.butik) THEN DO:
            OUTPUT TO VALUE(SESSION:TEMP-DIR + "Batch_samling_" + cIdag + ".txt") APPEND.
            PUT UNFORMATTED butiker.butik SKIP.
            OUTPUT CLOSE.
            RUN dagsrapp_utskrift.p (cSidor,butiker.butik,dDato,dDato,TRUE,OUTPUT cFil) NO-ERROR.
        END.
    END.
END.
QUIT.
