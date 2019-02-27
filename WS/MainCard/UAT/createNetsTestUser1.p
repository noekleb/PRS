
DEFINE VARIABLE ipcUserId   AS CHAR INIT "AntonSport" NO-UNDO. 
DEFINE VARIABLE ipcPassword AS CHAR INIT "netsIntegrasjon2013" NO-UNDO. 
                                                                                                   
                                                                                                    
FIND FIRST ClientUser WHERE ClientUser.ClientUserId = ipcUserid  NO-LOCK NO-ERROR. 
IF NOT AVAIL ClientUser THEN
DO:
      CREATE ClientUser. 
      ClientUser.ClientUserUID = GUID(GENERATE-UUID).
      ClientUser.Clientuserid = ipcUserid.
      ClientUser.ClientUserPassword = ENCODE(ipcPassword).
      ClientUser.iJBoxCompanyId = 801.
END.
