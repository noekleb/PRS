/* File       : getappsmeminfo2.p
  Author     : Peter van Dam (Netsetup)
  Description: Return memory usage information from the current session.

               This program is meant to be run on an AppServer but will
               obviously work in any Progress client session.

               Information is returned as standard debug information
               according to the following levels:

               - details level 65
               - totals level 35
               - warnings level 15

               See 'A Practical Use For Named Events' on www.v9stuff.com
               for information on logging levels.
*/


DEFINE TEMP-TABLE t-info NO-UNDO
      FIELD iLevel AS INTEGER
      FIELD nInfo AS CHAR.

DEFINE OUTPUT PARAMETER TABLE FOR t-info.

DEF VAR iDatabase AS INT NO-UNDO.
DEF VAR hBuffer AS HANDLE NO-UNDO.
DEF VAR hQuery AS HANDLE NO-UNDO.
DEF VAR iQuery AS INTEGER NO-UNDO.
DEF VAR iTables AS INT NO-UNDO.
DEF VAR iRecords AS INT NO-UNDO.
DEF VAR iRecordLength AS INT NO-UNDO.
DEF VAR iRecordLengthTotal AS INT NO-UNDO.
DEF VAR hTt AS HANDLE NO-UNDO.
DEF VAR hBufferTt AS HANDLE NO-UNDO.
DEF VAR cBuffer AS CHAR NO-UNDO.
DEF VAR iBuffers AS INT NO-UNDO.
DEF VAR hServer AS HANDLE NO-UNDO.
DEF VAR hSocket AS HANDLE NO-UNDO.
DEF VAR hProcedure AS HANDLE NO-UNDO.
DEF VAR iProcedure AS INTEGER NO-UNDO.
DEF VAR hWindow AS WIDGET-HANDLE NO-UNDO.
DEF VAR iWindow AS INTEGER NO-UNDO.
DEF VAR iNumDbs AS INTEGER NO-UNDO.
DEF VAR cDatabase AS CHAR NO-UNDO.
DEF VAR hLock AS HANDLE NO-UNDO.
DEF VAR hUsr AS HANDLE NO-UNDO.
DEF VAR hRecordId AS HANDLE NO-UNDO.
DEF VAR hTableNum AS HANDLE NO-UNDO.
DEF VAR hTable AS HANDLE NO-UNDO.
DEF VAR hTableName AS HANDLE NO-UNDO.
DEF VAR hQuery2 AS HANDLE NO-UNDO.
DEF VAR iLock AS INT NO-UNDO.

/* Progress version */

CREATE t-info.
ASSIGN t-info.iLevel = 55
      t-info.ninfo = SUBSTITUTE("AppServer PROGRESS version: &1",
                                PROVERSION).

/* PROPATH and database connections */

CREATE t-info.
ASSIGN t-info.iLevel = 55
      t-info.ninfo = SUBSTITUTE("PROPATH:~n&1",
                                REPLACE(PROPATH,",","~n")).


CREATE t-info.
ASSIGN t-info.iLevel = 55.
IF NUM-DBS > 0 THEN DO:
 t-info.ninfo = "Connected DATABASES:".

 DO iDatabase = 1 TO NUM-DBS:
   ASSIGN t-info.ninfo = SUBSTITUTE("&1~n&2",
                                    t-info.ninfo,
                                    DBPARAM(iDatabase)).
 END.
END.
ELSE
 ASSIGN t-info.ninfo = "No DATABASES connected".




/* dynamic temp-tables and buffers */

ASSIGN iTables = 0
      iRecordLengthTotal = 0.

hBufferTt = SESSION:FIRST-BUFFER.
DO WHILE VALID-HANDLE(hBufferTt):

 IF VALID-HANDLE(hBufferTt:TABLE-HANDLE) THEN DO:

   /* This is a dynamic temp-table */

   ASSIGN hTt = hBufferTt:TABLE-HANDLE
          iTables = iTables + 1
          iRecords = 0
          iRecordLength = 0.

   IF hTt:HAS-RECORDS = TRUE THEN DO:

     CREATE QUERY hQuery.

     /* Do NOT use the DEFAULT-BUFFER-HANDLE here because
        it might be positioned!!! */

     CREATE BUFFER hBuffer FOR TABLE hTt:DEFAULT-BUFFER-HANDLE.

     hQuery:SET-BUFFERS(hBuffer).
     hQuery:QUERY-PREPARE(SUBSTITUTE("for each &1", hBuffer:NAME)).
     hQuery:QUERY-OPEN().
     hQuery:GET-FIRST().

     DO WHILE hBuffer:AVAILABLE:

       ASSIGN iRecords = iRecords + 1
              iRecordLength = iRecordLength + hBuffer:RECORD-LENGTH
              iRecordLengthTotal = iRecordLengthTotal +
hBuffer:RECORD-LENGTH.

       hQuery:GET-NEXT().

     END.

     hQuery:QUERY-CLOSE().
     DELETE OBJECT hQuery.
     DELETE OBJECT hBuffer.

   END.

   CREATE t-info.
   ASSIGN t-info.iLevel = 65
          t-info.ninfo = SUBSTITUTE("Dynamic temp-table &1: &2 records",
                                    hTt:NAME,
                                    iRecords).
   IF SESSION:CLIENT-TYPE = "AppServer" AND iRecordLength NE 0 THEN
     ASSIGN t-info.iLevel = 15
            t-info.ninfo = SUBSTITUTE("&1, LENGTH=&2!",
                                      t-info.ninfo,
                                      iRecordLength).

   IF hTt:UNDO = TRUE THEN
     ASSIGN t-info.iLevel = 15
            t-info.ninfo = SUBSTITUTE("&1, UNDO!",
                                      t-info.ninfo).

 END.
 ELSE DO:
   /* It is a buffer, not a table */

   ASSIGN iBuffers = iBuffers + 1.

   ASSIGN cBuffer = SUBSTITUTE("Buffer &1, TABLE &2.&3",
                               hBufferTt:NAME,
                               hBufferTt:DBNAME,
                               hBufferTt:TABLE).

   IF hBufferTt:PRIVATE-DATA NE ? THEN
     ASSIGN cBuffer = SUBSTITUTE("&1, PRIVATE-DATA: &2",
                                 cBuffer,
                                 hBufferTt:PRIVATE-DATA).

   CREATE t-info.
   IF hBufferTt:AVAILABLE THEN
     ASSIGN t-info.iLevel = 65
            t-info.ninfo = SUBSTITUTE("&1, ROWID &2",
                                      cBuffer,
                                      STRING(hBufferTt:ROWID)).
   ELSE
     ASSIGN t-info.iLevel = 65
            t-info.ninfo = cBuffer.

   IF hBufferTt:LOCKED THEN DO:
     ASSIGN t-info.iLevel = 15
            t-info.ninfo = SUBSTITUTE("&1 LOCKED!",
                                      t-info.ninfo).
   END.
 END.

 hBufferTt = hBufferTt:NEXT-SIBLING.
END.


IF iTables > 0 THEN DO:
 CREATE t-info.
 ASSIGN t-info.iLevel = 35
        t-info.ninfo = SUBSTITUTE("&1 dynamic temp-tables found, total
size
&2",
                                  iTables,
                                  iRecordLengthTotal).
END.


IF iBuffers > 0 THEN DO:
 CREATE t-info.
 ASSIGN t-info.iLevel = 35
        t-info.ninfo = SUBSTITUTE("&1 buffers found", iBuffers).
END.


/* Queries */
/* v10 and higher */

&IF PROVERSION BEGINS "1" &THEN

 hQuery = SESSION:FIRST-QUERY.

 DO WHILE VALID-HANDLE(hQuery):

   ASSIGN iQuery = iQuery + 1.

   hQuery = hQuery:NEXT-SIBLING.
 END.

 IF iQuery > 0 THEN DO:
   CREATE t-info.
   ASSIGN t-info.iLevel = 35
          t-info.ninfo = SUBSTITUTE("&1 queries found", iQuery).
 END.

&ENDIF




/* VST info */

DO iNumDbs = 1 TO NUM-DBS:

 cDatabase = LDBNAME(iNumDbs).

 CREATE BUFFER hLock FOR TABLE cDatabase + "._Lock":U.

 CREATE BUFFER hTable FOR TABLE cDatabase + "._File":U.

 ASSIGN hUsr = hLock:BUFFER-FIELD("_Lock-usr":U)
        hRecordId = hLock:BUFFER-FIELD("_Lock-Recid":U)
        hTableNum = hLock:BUFFER-FIELD("_Lock-Table":U)
        hTableName = hTable:BUFFER-FIELD("_File-name":U).

 CREATE QUERY hQuery.
 hQuery:SET-BUFFERS(hLock).
 hQuery:QUERY-PREPARE(SUBSTITUTE("FOR EACH &1._Lock NO-LOCK":U,
cDatabase)).
 hQuery:QUERY-OPEN().
 hQuery:GET-FIRST().

 /* In this VST we always get lots of records */
 /* make sure we stop reading when hUsr = ? */

 DO WHILE hUsr:BUFFER-VALUE NE ?:

   ASSIGN iLock = iLock + 1.

   /* Find the file by file number. Note: the file number is NOT unique in
_File! */
   CREATE QUERY hQuery2.
   hQuery2:SET-BUFFERS(hTable).
   hQuery2:QUERY-PREPARE(SUBSTITUTE("FOR EACH &1._File where
&1._File._File-Number = &2 NO-LOCK",
                                    cDatabase,
                                    hTableNum:BUFFER-VALUE)).
   hQuery2:QUERY-OPEN().
   hQuery2:GET-FIRST().

   CREATE t-info.
   ASSIGN t-info.iLevel = 15
          t-info.nInfo = SUBSTITUTE("Record LOCK found for table &1.&2,
RECID &3",
                                    cDatabase,
                                    hTableName:BUFFER-VALUE,
                                    hRecordId:BUFFER-VALUE).

   DELETE OBJECT hQuery2.

   hQuery:GET-NEXT().
 END.

 DELETE OBJECT hQuery.
 DELETE OBJECT hLock.
 DELETE OBJECT hTable.
END.


IF iLock > 0 THEN DO:
 CREATE t-info.
 ASSIGN t-info.iLevel = 35
        t-info.ninfo = SUBSTITUTE("&1 record locks found",
                                  iLock).
END.



/* Servers, sockets and persistent procedures */

ASSIGN hServer = SESSION:FIRST-SERVER.
DO WHILE VALID-HANDLE(hServer):
 CREATE t-info.
 ASSIGN t-info.iLevel = 35
        t-info.ninfo = SUBSTITUTE("SERVER &1 found", hServer:NAME).
 ASSIGN hServer = hServer:NEXT-SIBLING.
END.


ASSIGN hSocket = SESSION:FIRST-SERVER-SOCKET.
DO WHILE VALID-HANDLE(hSocket):
 CREATE t-info.
 ASSIGN t-info.iLevel = 35
        t-info.ninfo = SUBSTITUTE("SERVER-SOCKET &1 found", hSocket:NAME).
 ASSIGN hSocket = hSocket:NEXT-SIBLING.
END.

ASSIGN hSocket = SESSION:FIRST-SOCKET.
DO WHILE VALID-HANDLE(hSocket):
 CREATE t-info.
 ASSIGN t-info.iLevel = 35
        t-info.ninfo = SUBSTITUTE("SOCKET &1 found", hSocket:NAME).
 ASSIGN hSocket = hSocket:NEXT-SIBLING.
END.

ASSIGN hProcedure = SESSION:FIRST-PROCEDURE.
DO WHILE VALID-HANDLE(hProcedure):
 ASSIGN iProcedure = iProcedure + 1.
   CREATE t-info.
   ASSIGN t-info.iLevel = 65
          t-info.ninfo = SUBSTITUTE("PERSISTENT PROCEDURE &1 running",
hProcedure:FILE-NAME).
 ASSIGN hProcedure = hProcedure:NEXT-SIBLING.
END.
CREATE t-info.
ASSIGN t-info.iLevel = 35
      t-info.ninfo = SUBSTITUTE("&1 persistent procedures running",
iProcedure).


ASSIGN hWindow = SESSION:FIRST-CHILD.
DO WHILE VALID-HANDLE(hWindow):
 ASSIGN iWindow = iWindow + 1.
   CREATE t-info.
   ASSIGN t-info.iLevel = 65
          t-info.ninfo = SUBSTITUTE("WINDOW &1", hWindow:PRIVATE-DATA).
 ASSIGN hWindow = hWindow:NEXT-SIBLING.
END.
IF iWindow NE 0 THEN DO:
 CREATE t-info.
 ASSIGN t-info.iLevel = 35
        t-info.ninfo = SUBSTITUTE("&1 windows active", iWindow).
END.
