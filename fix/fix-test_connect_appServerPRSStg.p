DEF VAR hServer           AS HANDLE    NO-UNDO.
DEF VAR cConnectionString AS CHARACTER NO-UNDO.
DEF VAR lConnected        AS LOGICAL   NO-UNDO.
DEF VAR obOk              AS LOG       NO-UNDO.
DEF VAR cIpAdr            AS CHAR      NO-UNDO.

ASSIGN
    cIpAdr            = '192.168.200.2'
    cConnectionString = "-H " + cIpAdr + " -AppService asbroker1"
    .


DO:
    CREATE SERVER hServer. 
    lConnected = hServer:CONNECT(cConnectionString) NO-ERROR.
    IF lConnected THEN 
    DO: 
        hServer:DISCONNECT().
        obOk = TRUE.
    END.
    ELSE obOk = FALSE.
    DELETE OBJECT hServer. 
END.

message
    obOk
    view-as alert-box.