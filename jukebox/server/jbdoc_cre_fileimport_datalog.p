/* Create log files from data import. Called from jbdoc_savedoc.p
   Separation done so document handling can work without file import tables */
   
DEF INPUT   PARAM icLogHeaderLogIdList AS CHAR   NO-UNDO.
DEF INPUT   PARAM ihBuffer             AS HANDLE NO-UNDO.

DEF VAR ix              AS INT    NO-UNDO.
DEF VAR cImportHeaderId AS CHAR   NO-UNDO.
DEF VAR iImportLogId    AS INT    NO-UNDO.
DEF VAR hQuery          AS HANDLE NO-UNDO.
DEF VAR cCurrLogName    AS CHAR   NO-UNDO INIT "<<<".
DEF VAR cUserid         AS CHAR   NO-UNDO.
DEF VAR iCurrHeaderId   AS INT    NO-UNDO.

cUserid = DYNAMIC-FUNCTION("getAsUserId" IN SOURCE-PROCEDURE) NO-ERROR.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).

DO ix = 1 TO NUM-ENTRIES(icLogHeaderLogIdList,";"):
  ASSIGN cImportHeaderId = ENTRY(1,ENTRY(ix,icLogHeaderLogIdList,";"),"|")
         iImportLogId    = INTEGER(ENTRY(2,ENTRY(ix,icLogHeaderLogIdList,";"),"|"))
         .
  hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME 
                     + " WHERE iImportHeaderId = " + cImportHeaderId
                     + " BY cLogName").
  hQuery:QUERY-OPEN().
  hQuery:GET-FIRST().
  REPEAT WHILE NOT hQuery:QUERY-OFF-END:
    IF ihBuffer:BUFFER-FIELD("cLogName"):BUFFER-VALUE NE cCurrLogName THEN DO:
      CREATE JBoxFileImportLogFileHeader.
      ASSIGN JBoxFileImportLogFileHeader.cImportLogFileName      = ihBuffer:BUFFER-FIELD("cLogName"):BUFFER-VALUE
             JBoxFileImportLogFileHeader.cCreatedBy              = cUserid
             JBoxFileImportLogFileHeader.dCreated                = TODAY
             JBoxFileImportLogFileHeader.iCreTime                = TIME
             JBoxFileImportLogFileHeader.cImportLogFileType      = ihBuffer:BUFFER-FIELD("cLogType"):BUFFER-VALUE
             JBoxFileImportLogFileHeader.cImportLogFileDesc      = ihBuffer:BUFFER-FIELD("cLogDesc"):BUFFER-VALUE
             JBoxFileImportLogFileHeader.iJBoxFileImportLogId    = iImportLogId
             JBoxFileImportLogFileHeader.iJBoxFileImportHeaderId = ihBuffer:BUFFER-FIELD("iImportHeaderId"):BUFFER-VALUE
             iCurrHeaderId                                       = JBoxFileImportLogFileHeader.iJBoxFileImportLogFileHeaderId
             .
    END.
    CREATE JBoxFileImportLogFile.
    ASSIGN JBoxFileImportLogFile.iImportLogLineNum = ihBuffer:BUFFER-FIELD("iLineNum"):BUFFER-VALUE
           JBoxFileImportLogFile.cImportLogText    = ihBuffer:BUFFER-FIELD("cLogText"):BUFFER-VALUE 
           JBoxFileImportLogFile.iJBoxFileImportLogFileHeaderId = iCurrHeaderId
           cCurrLogName                            = ihBuffer:BUFFER-FIELD("cLogName"):BUFFER-VALUE
           .
    hQuery:GET-NEXT().
  END.
END.

DELETE OBJECT hQuery.
