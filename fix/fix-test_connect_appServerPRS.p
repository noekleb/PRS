DEF VAR hServer           AS HANDLE    NO-UNDO.
DEF VAR cConnectionString AS CHARACTER NO-UNDO.
DEF VAR lConnected        AS LOGICAL   NO-UNDO.
DEF VAR obOk              AS LOG       NO-UNDO.
DEF VAR cIpAdr            AS CHAR      NO-UNDO.
DEF VAR iX                AS INT       NO-UNDO.
DEF VAR cTekst            AS CHAR      NO-UNDO.

ASSIGN
    cIpAdr            = 'localhost'
    cConnectionString = "-H " + cIpAdr + " -AppService asPRS -S 3045 -DirectConnect"
    .


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

    IF lConnected THEN 
    DO: 
        hServer:DISCONNECT().
        obOk = TRUE.
    END.
    ELSE obOk = FALSE.
    DELETE OBJECT hServer. 
END.

message
    obOk SKIP(1)
    cTekst
    view-as alert-box.
