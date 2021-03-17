DEFINE INPUT PARAMETER cFile-Name   AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER iFraButikkNr AS INT  NO-UNDO.
DEFINE INPUT PARAMETER iTilButikkNr AS INT  NO-UNDO.
DEFINE INPUT PARAMETER cFelt        AS CHAR NO-UNDO.

/* DETTE SER UT TIL Å FUNGERE OK */ 

DEFINE VARIABLE lDebugFile AS LOGICAL INIT TRUE.
DEF VAR lLogKatalog AS CHAR NO-UNDO.
DEF VAR lLogFilNavn AS CHAR NO-UNDO.
DEFINE STREAM outStr. 
DEFINE VARIABLE bOk        AS LOG  NO-UNDO.
DEFINE VARIABLE hQuery     AS HANDLE NO-UNDO.
DEFINE VARIABLE hBuffer    AS HANDLE NO-UNDO.
DEFINE VARIABLE bfField AS HANDLE NO-UNDO. 
DEFINE VARIABLE iCounter AS INT NO-UNDO. 

CREATE BUFFER hBuffer  FOR TABLE cFile-Name  BUFFER-NAME 'record1'.
hBuffer:DISABLE-DUMP-TRIGGERS(). 
hBuffer:DISABLE-LOAD-TRIGGERS(FALSE). 

lDebugFile = TRUE.

lLogFilNavn = '\bytt_butikknr.txt'.
{syspara.i 1 1 59 lLogKatalog}
IF lLogKatalog = '' THEN
    ASSIGN lLogKatalog = ".\log" + lLogfilNavn.
ELSE
    ASSIGN lLogKatalog = RIGHT-TRIM(lLogKatalog,'\') + lLogFilNavn.

IF lDebugFile THEN 
DO:
    OUTPUT STREAM outStr TO VALUE(lLogKatalog) UNBUFFERED APPEND. 
    PUT STREAM outStr UNFORMATTED FILL("-",50) SKIP "Table:" cFile-Name " Felt:" cFelt " " iTilButikknr " <- " iFraButikknr SKIP.
END. 

CREATE QUERY hQuery.
hQuery:ADD-BUFFER(hBuffer).
hQuery:QUERY-PREPARE("PRESELECT EACH " + hBuffer:NAME + ' WHERE ' + hBuffer:NAME + '.' + cFelt + ' = ' + STRING(iFraButikkNr)).
hQuery:QUERY-OPEN().

IF lDebugFile THEN PUT STREAM outStr UNFORMATTED "Query: PRESELECT EACH " + hBuffer:NAME + ' WHERE ' + hBuffer:NAME + '.' + cFelt + ' = ' + STRING(iFraButikkNr) SKIP.

REPEAT WHILE hQuery:GET-NEXT(NO-LOCK) AND NOT hQuery:QUERY-OFF-END:
    DO FOR ELogg TRANSACTION:             
        hQuery:GET-CURRENT(EXCLUSIVE-LOCK).
        
        ASSIGN 
           bfField = hBuffer:BUFFER-FIELD(cFelt)
           bfField:BUFFER-VALUE = iTilButikkNr 
           iCounter = iCounter + 1 
           NO-ERROR. 

        hBuffer:BUFFER-RELEASE() .
        IF AVAILABLE ELogg THEN RELEASE ELogg NO-ERROR.
    END.
END.

IF lDebugFile THEN PUT STREAM outStr UNFORMATTED cFile-Name " Total:" iCounter " Records" SKIP.


hQuery:QUERY-CLOSE()   NO-ERROR.
DELETE OBJECT hQuery   NO-ERROR.
DELETE OBJECT hBuffer  NO-ERROR.
IF lDebugFile THEN OUTPUT STREAM outstr CLOSE. 

bOk = TRUE.
