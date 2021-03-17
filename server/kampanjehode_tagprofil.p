/* lager_butikkstr.p
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEFINE VARIABLE cIdList AS CHARACTER NO-UNDO.
DEFINE VARIABLE iLoop   AS INTEGER   NO-UNDO.
DEFINE VARIABLE iKampanjeId AS INTEGER NO-UNDO.
  
/*
• If you define a temp-table with the same name as a database table and 
  then you define a buffer for that name, the buffer will be associated 
  with the database table, not with the temp-table.
*/
ASSIGN 
  iKampanjeId = INT(ENTRY(1,icParam,'|'))
  cIdList     = ENTRY(2,icParam,'|')
  .
  
FOR EACH KampanjeProfil EXCLUSIVE-LOCK WHERE 
  KampanjeProfil.KampanjeId = iKampanjeId:
  DELETE KampanjeProfil.    
END.
  
IF cIdList <> '' THEN
DO:    
  DO iLoop = 1 TO NUM-ENTRIES(cIdList,','):
    
      FIND PrisProfil NO-LOCK WHERE 
        Prisprofil.ProfilNr = INT(ENTRY(iLoop,cIdList,',')) NO-ERROR.
      IF AVAILABLE PrisProfil THEN 
      DO:
        CREATE KampanjeProfil.
        ASSIGN 
          KampanjeProfil.KampanjeId       = iKampanjeId
          KampanjeProfil.ProfilNr         = PrisProfil.ProfilNr
          KampanjeProfil.DatotidOpprettet = NOW
          .
      END.
  END.
END.
obOK = YES.
RETURN ocReturn.

/* **********************  Internal Procedures  *********************** */
