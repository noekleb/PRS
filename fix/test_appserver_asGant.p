DEFINE VARIABLE lformatted        AS LOG       NO-UNDO.
DEFINE VARIABLE cTargetType       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cJSonFile         AS CHARACTER NO-UNDO.
DEFINE VARIABLE lWriteOk          AS LOG       NO-UNDO.

DEFINE VARIABLE hasGant           AS HANDLE    NO-UNDO.
DEFINE VARIABLE hProc             AS HANDLE    NO-UNDO.
DEFINE VARIABLE lReturn           AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cConnectionString AS CHARACTER NO-UNDO.

DEFINE VAR      cReturn           AS CHAR      NO-UNDO.
DEF    VAR      iLoop             AS INT       NO-UNDO.
DEF    VAR      cTekst            AS CHAR      NO-UNDO.
DEFINE VARIABLE bOk               AS LOG NO-UNDO.

ASSIGN 
    cConnectionString = "-H sp1tomn-14 -AppService asGant -sessionModel Session-free".

CREATE SERVER hasGant.
lReturn = hasGant:CONNECT(cConnectionString).

IF ERROR-STATUS:ERROR OR lReturn = FALSE THEN 
DO:
    lReturn = hasGant:DISCONNECT().
    DELETE OBJECT hasGant NO-ERROR.
    
    MESSAGE 'Feli ved connect til server.'
        VIEW-AS ALERT-BOX.    
    RETURN ERROR RETURN-VALUE.
END.

RUN asPakkseddel.p ON hasGant (
    10,
    143390,
    FALSE,
    FALSE,
    OUTPUT bOk,
    OUTPUT cReturn
    ) .

lReturn = hasGant:DISCONNECT().
DELETE OBJECT hasGant NO-ERROR.
