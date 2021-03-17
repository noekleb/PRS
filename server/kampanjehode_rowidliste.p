/* lager_butikkstr.p
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEFINE VARIABLE cIdList AS CHARACTER NO-UNDO.
DEFINE VARIABLE cRowIdList AS CHARACTER NO-UNDO.
DEFINE VARIABLE iLoop   AS INTEGER   NO-UNDO.
  
/*
• If you define a temp-table with the same name as a database table and 
  then you define a buffer for that name, the buffer will be associated 
  with the database table, not with the temp-table.
*/
ASSIGN 
  cIdList    = icParam
  cRowIdList = ''
  .
  
IF cIdList <> '' THEN 
DO iLoop = 1 TO NUM-ENTRIES(cIdList,'|'):
  
    FIND PrisProfil NO-LOCK WHERE 
      Prisprofil.ProfilNr = INT(ENTRY(iLoop,cIdList,'|')) NO-ERROR.
    IF AVAILABLE PrisProfil THEN 
      cRowIdList = cRowIdList + 
                   (IF cRowIdList <> '' THEN ',' ELSE '') + 
                   STRING(ROWID(PrisProfil)).
END.
ASSIGN 
  ocReturn = cRowIdList
  .

obOK = YES.
RETURN ocReturn.

/* **********************  Internal Procedures  *********************** */
