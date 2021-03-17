DEFINE VARIABLE hServer AS HANDLE      NO-UNDO.
DEFINE VARIABLE cConnectionString AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lConnected AS LOGICAL     NO-UNDO.

DEFINE VAR cKundnamn  AS CHARACTER   NO-UNDO.
DEFINE VAR dtAskTime  AS DATETIME    NO-UNDO.
DEFINE VAR dtLastTime AS DATETIME    NO-UNDO.
DEFINE VAR deMSgrens  AS DECIMAL     NO-UNDO.
DEFINE VAR lOK        AS LOGICAL     NO-UNDO.
DEFINE VAR cMessage   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cBody AS CHARACTER   NO-UNDO.

cConnectionString = SESSION:PARAMETER.
CREATE SERVER hServer. 
lConnected = hServer:CONNECT(cConnectionString) NO-ERROR.
IF lConnected THEN DO: 
    RUN asWebKommKontroll.p ON hServer (OUTPUT cKundnamn, OUTPUT dtAskTime, OUTPUT dtLastTime,OUTPUT deMSgrens, OUTPUT lOK, OUTPUT cMessage).
    cBody = cMessage.
    hServer:DISCONNECT().
END. 
ELSE DO:
    lOK = FALSE.
    cBody = "Ingen kontakt med appserver.".
END.
DELETE OBJECT hServer.
/* MESSAGE cKundnamn   skip               */
/*         cMailTo     SKIP               */
/*         dtAskTime   skip               */
/*         dtLastTime  skip               */
/*         deMSgrens   skip               */
/*         lOK         skip               */
/*         cMessage                       */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK. */
IF NOT lOK THEN
    RUN sendmail_tsl.p ("WEBKOMMKONTROLL","WebKommKontroll " + cKundnamn,"", cBody,"","") NO-ERROR.
ELSE DO:
    IF dtAskTime - dtLastTime > deMSgrens THEN DO:
        cBody = "Senaste kommunikation " + STRING(dtLastTime) + ". Gräns passerad".
        RUN sendmail_tsl.p ("WEBKOMMKONTROLL","WebKommKontroll " + cKundnamn,"", cBody,"","") NO-ERROR.
    END.
END.

/* cConnectionString = "-H nooslfw501.dyndns.org -S 3095 -DirectConnect". */
/* cConnectionString = "-H 192.168.0.18 -AppService asbroker1". */

