DEF VAR ipiButikknr AS INT NO-UNDO. 
DEF VAR ipiKassenr AS INT INIT 1 NO-UNDO. 
DEF VAR ldelete AS LOG INIT FALSE NO-UNDO. 
DEF VAR dDate AS DATETIME NO-UNDO.
ldelete = TRUE. 
              
DEFINE BUFFER bfDistributeFile FOR distributefile. 
dDate = TODAY - 1. 
ipiButikknr = 2. 

 MESSAGE dDate VIEW-AS ALERT-BOX. 

 FOR EACH  DistributeFile WHERE 
           DistributeFile.Butikk = ipiButikknr AND
           DATE(Distributefile.DATE)   = date(dDate)  NO-LOCK : 

 MESSAGE DistributeFile.DATE 
         DistributeFile.FILENAME VIEW-AS ALERT-BOX. 

 IF AVAIL distributefile THEN
 DO:
    FIND FIRST DistributeFileReceiver WHERE 
               DistributeFileReceiver.DistributeFileId  =  DistributeFile.id AND
               DistributeFileReceiver.kassenr = ipiKasseNr EXCLUSIVE-LOCK NO-ERROR. 

     IF AVAIL DistributeFileReceiver AND ldelete THEN
         DELETE DistributeFileReceiver NO-ERROR. 
         
     IF lDelete THEN
     DO:
        OS-DELETE VALUE(DistributeFile.FILENAME) NO-ERROR. 
        
        FIND bfDistributeFile WHERE 
             rowid(bfDistributeFile)  = 
             ROWID(distributefile) EXCLUSIVE-LOCK . 
        DELETE bfDistributeFile NO-ERROR.
     END. 
 END. 
 END. 
