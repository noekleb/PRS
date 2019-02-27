/* Make a copy of this file to the <project>/server/incl catalog for adding your own
   standard procedures for frequently used calculated fields.
   Extra well suited if the procedures are not bound to a particular source buffer. */
  
/* This is an example:

Usage (sequence is important in this case):

        + ";+UdfId|CHARACTER|x(16)|udfId(ROWIDsite)|UdfId"
        + ";+UdfDataExist|LOGICAL|y/n|udfDataExist(site-id)|Udf data"

DEF VAR cCalitaTable   AS CHAR NO-UNDO.
DEF VAR cCalitaStudyId AS CHAR NO-UNDO.
DEF VAR cCalitaUdfId   AS CHAR NO-UNDO.
                      
startASlib().

cCalitaStudyId = ENTRY(1,DYNAMIC-FUNCTION("getSessionContext")).

PROCEDURE udfId:
  DEF INPUT  PARAM irTable       AS ROWID NO-UNDO.
  DEF INPUT  PARAM icCalitaTable AS CHAR  NO-UNDO.
  DEF INPUT  PARAM icSessionId   AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocReturn      AS CHAR  NO-UNDO.

  cCalitaTable = icCalitaTable.

  FIND FIRST USERDEF NO-LOCK
       WHERE USERDEF.table-name = cCalitaTable
         AND USERDEF.proj-id    = cCalitaStudyId
       NO-ERROR.
  IF AVAIL USERDEF THEN
    ocReturn = USERDEF.userdef-id.

  cCalitaUdfId = ocReturn.
END PROCEDURE.

PROCEDURE udfDataExist:    
  DEF INPUT  PARAM icRecordId    AS CHAR  NO-UNDO.
  DEF INPUT  PARAM icSessionId   AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocReturn      AS CHAR  NO-UNDO.

  FIND FIRST UDFDATA NO-LOCK
       WHERE UDFDATA.tab-pk     = icRecordId
         AND UDFDATA.table-name = cCalitaTable
         AND UDFDATA.proj-id    = cCalitaStudyId
         AND UDFDATA.f-data     NE ""
       NO-ERROR.
  
  ocReturn = IF AVAIL UDFDATA THEN "yes" ELSE "no".
END PROCEDURE.

*/   
