
DEF VAR cFile-Name   AS CHAR INIT "skotex.bruker" NO-UNDO.

DEF VAR iFraButikkNr AS INT  INIT 299 NO-UNDO.
DEF VAR iTilButikkNr AS INT  INIT 218 NO-UNDO.
DEF VAR cFelt        AS CHAR INIT "butikknr" NO-UNDO.


DEF VAR cDumpFil   AS CHAR NO-UNDO.
DEF VAR cDump-Name AS CHAR NO-UNDO.
DEF VAR cDesc      AS CHAR NO-UNDO.
DEF VAR bOk        AS LOG  NO-UNDO.
DEF VAR hQuery     AS HANDLE NO-UNDO.
DEF VAR hBuffer    AS HANDLE NO-UNDO.
DEF VAR hBuffer2   AS HANDLE NO-UNDO.
DEF VAR cString    AS CHAR NO-UNDO.
DEF VAR hField     AS HANDLE NO-UNDO.
DEF VAR bh         AS HANDLE NO-UNDO.
DEF VAR ix         AS INT NO-UNDO.

DEFINE VARIABLE cFindWhere AS CHAR NO-UNDO.

CREATE BUFFER hBuffer  FOR TABLE cFile-Name  BUFFER-NAME 'record1'.
CREATE BUFFER hBuffer2 FOR TABLE cFile-Name  BUFFER-NAME 'record2'. 

CREATE QUERY hQuery.
hQuery:ADD-BUFFER(hBuffer).
hQuery:QUERY-PREPARE("PRESELECT EACH " + hBuffer:NAME + ' WHERE ' + hBuffer:NAME + '.' + cFelt + ' = ' + STRING(iFraButikkNr)).
hQuery:QUERY-OPEN().

REPEAT WHILE hQuery:GET-NEXT(NO-LOCK) AND NOT hQuery:QUERY-OFF-END:

    DO TRANSACTION:             
        cFindWhere = "WHERE ROWID(" + hBuffer2:NAME + ") = TO-ROWID(" +  QUOTER(hBuffer:ROWID) + ")". 
        bOK = hBuffer2:FIND-FIRST(cFindWhere,EXCLUSIVE-LOCK) NO-ERROR.
        
        IF bOK AND hBuffer2:AVAILABLE THEN
           ASSIGN hBuffer2:BUFFER-FIELD(cFelt):BUFFER-VALUE = iTilButikkNr NO-ERROR. 

        hBuffer2:BUFFER-RELEASE() .
    END.
END.
hQuery:QUERY-CLOSE() NO-ERROR.
DELETE OBJECT hQuery NO-ERROR.

bOk = TRUE.

DELETE OBJECT hBuffer NO-ERROR.
DELETE OBJECT hBuffer2 NO-ERROR.
