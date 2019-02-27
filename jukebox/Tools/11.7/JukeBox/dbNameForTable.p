/* Purpose: FIND the correct db name for table 
   Created 20.may.2016 by brynjar@chemistry.no
*/
DEF INPUT  PARAM icTableName AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocDbName    AS CHAR NO-UNDO.

DEF VAR ix           AS INT NO-UNDO.
DEF VAR bNoRecursion AS LOG NO-UNDO.
DEF VAR cDbName      AS CHAR NO-UNDO.

IF NUM-ENTRIES(icTableName,"|") > 1 THEN DO:
    
  ASSIGN cDbName      = ENTRY(2,icTableName,"|")
         icTableName  = ENTRY(1,icTableName,"|")
         bNoRecursion = YES
         .
  IF icTableName MATCHES "buf*_*" THEN
    icTableName = SUBSTR(icTableName,6).

  DEFINE QUERY qDb FOR DICTDB._file.
  
  
  OPEN QUERY qDB FOR 
    EACH DICTDB._file
         WHERE DICTDB._file._file-name = icTableName
         AND   LOOKUP(DICTDB._FILE._OWNER,"PUB,_FOREIGN":U) > 0
         NO-LOCK
         .
  GET FIRST qDB.
  
  IF AVAIL DICTDB._file THEN
    ocDbName = cDbName.
END.   

ELSE DO ix = 1 TO NUM-DBS:
  CREATE ALIAS "DICTDB":u FOR DATABASE VALUE(SDBNAME(ix)).
  RUN "JukeBox/dbNameForTable.p" (icTableName + "|" + LDBNAME(ix),OUTPUT ocDbName).
  IF ocDbName NE "" THEN LEAVE.
END.


