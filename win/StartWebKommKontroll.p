DEFINE VARIABLE cPF AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cMailTo AS CHARACTER   NO-UNDO.
DEFINE VARIABLE ii AS INTEGER     NO-UNDO.
DO ii = 1 TO NUM-ENTRIES(SESSION:PARAMETER):
    CASE ENTRY(1,ENTRY(ii,SESSION:PARAMETER),"="):
        WHEN "PF" THEN
            cPF = ENTRY(2,ENTRY(ii,SESSION:PARAMETER),"=").
        WHEN "MAILTO" THEN
            cMailTo = ENTRY(2,ENTRY(ii,SESSION:PARAMETER),"=").
    END CASE.
END.

RUN connectdb.p(cPF) NO-ERROR.
IF NOT CONNECTED("skotex") THEN DO:
    MESSAGE "Gick inte att connecta 'kk"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RUN WebKommKontroll_STDsendmail.p.
    
END.
ELSE DO:
/*     connecta appserver */
    IF NOT CONNECTED THEN
        MESSAGE
    ELSE DO:
        /* kör kontroll mot appserver */
    END.

END.

