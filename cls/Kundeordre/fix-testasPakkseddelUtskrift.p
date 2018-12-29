DEF VAR hServer           AS HANDLE    NO-UNDO.
DEF VAR cConnectionString AS CHARACTER NO-UNDO.
DEF VAR lConnected        AS LOGICAL   NO-UNDO.
DEF VAR obOk              AS LOG       NO-UNDO.
DEF VAR cIpAdr            AS CHAR      NO-UNDO.
DEF VAR iX                AS INT       NO-UNDO.
DEF VAR cTekst            AS CHAR      NO-UNDO.
DEFINE VARIABLE ocReturn AS CHARACTER NO-UNDO.
DEFINE VARIABLE cKOrdre_IdLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE bBrukAppServer AS LOG NO-UNDO.
DEFINE VARIABLE iAntEks AS INTEGER NO-UNDO.

ASSIGN
    cIpAdr            = 'localhost'
    cConnectionString = "-H " + cIpAdr + " -AppService asPRS -S 3045 -DirectConnect"
/*    cConnectionString = "-H " + cIpAdr + " -AppService asbroker1 -S 3090 -DirectConnect"*/
    cKOrdre_IdLst     = '1180000003,1180000004'
    bBrukAppServer    = FALSE
    iAntEks           = 2  
    .


DO:
    IF bBrukAppServer THEN 
    DO:
        CREATE SERVER hServer. 
        lConnected = hServer:CONNECT(cConnectionString) NO-ERROR.
        cTekst = RETURN-VALUE.
        IF ERROR-STATUS:ERROR THEN 
        DO ix = 1 TO ERROR-STATUS:NUM-MESSAGES:    
            cTekst = cTekst + 
                     (IF cTekst <> '' THEN CHR(10) ELSE '') + 
                     STRING(ERROR-STATUS:GET-NUMBER(ix)) + ' ' + ERROR-STATUS:GET-MESSAGE(ix).
        END.
    END.

    IF lConnected AND bBrukAppServer THEN 
    DO: 
        RUN cls\kundeordre\asPakkseddelUtskrift.p ON hServer (cKOrdre_IdLst, 'utlev',iAntEks,'aree', OUTPUT ocReturn). 
        hServer:DISCONNECT().
        obOk = TRUE.
    END.
    ELSE DO: 
        RUN cls\kundeordre\asPakkseddelUtskrift.p (cKOrdre_IdLst, 'utlev',iAntEks,'aree', OUTPUT ocReturn). 
        obOk = FALSE.
    END.

    IF bBrukAppServer THEN 
        DELETE OBJECT hServer. 
END.

MESSAGE
    obOk SKIP(1)
    cTekst SKIP
    ocReturn
    VIEW-AS ALERT-BOX.
