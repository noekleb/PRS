

DEFINE VARIABLE lok AS LOGICAL NO-UNDO. 

DEFINE TEMP-TABLE ClientFile NO-UNDO
    FIELD FileID AS CHAR. 

DEFINE INPUT  PARAMETER ipiButikknr AS INT NO-UNDO. 
DEFINE INPUT  PARAMETER ipiKassenr  AS INT NO-UNDO. 
DEFINE INPUT  PARAMETER TABLE FOR ClientFile .
DEFINE OUTPUT PARAMETER lStatus AS LOG NO-UNDO. 


LOG-MANAGER:WRITE-MESSAGE("Delete File Request -START- From:Kasse=" + STRING(ipiKasseNr) + " Butikk=" + STRING(ipiButikknr),"RECEIVE-DATA").

lStatus = TRUE. 

FOR EACH ClientFile : 
    LOG-MANAGER:WRITE-MESSAGE("DistributeFile ID:" + ClientFile.FileId  ,"RECEIVE-DATA").

    FIND FIRST DistributeFile WHERE 
               DistributeFile.id     = ClientFile.FileId  AND 
               DistributeFile.Butikk = ipiButikknr NO-LOCK NO-ERROR. 

    LOG-MANAGER:WRITE-MESSAGE("DistributeFile Available:" + STRING(AVAIL DistributeFile)  ,"RECEIVE-DATA").

    IF AVAIL DistributeFile THEN
    DO:

        DO TRANSACTION : 
            FIND FIRST DistributeFileReceiver WHERE 
                DistributeFileReceiver.DistributeFileId  =  DistributeFile.id AND
                DistributeFileReceiver.kassenr = ipiKasseNr EXCLUSIVE-LOCK. 
            IF AVAIL DistributeFileReceiver THEN
            DO:
                LOG-MANAGER:WRITE-MESSAGE("Deleted DistributeFileReceiver for Kasse:" + STRING(ipiKasseNr) + " Filename:"
                                          + DistributeFile.FILENAME ,"RECEIVE-DATA").
                DELETE DistributeFileReceiver NO-ERROR. 
                IF ERROR-STATUS:ERROR THEN
                    LOG-MANAGER:WRITE-MESSAGE("Delete ERROR","ERROR").

            END. 
        END. 

        /* Clean-up DistributeFile table */ 
        IF NOT CAN-FIND(FIRST DistributeFileReceiver WHERE 
                DistributeFileReceiver.DistributeFileId  =  DistributeFile.id AND
                DistributeFileReceiver.kassenr NE ipiKasseNr) THEN 
        
        DO TRANSACTION:
            OS-DELETE VALUE(DistributeFile.FILENAME) NO-ERROR. 
            FIND CURRENT DistributeFile EXCLUSIVE-LOCK NO-ERROR. 
            
            LOG-MANAGER:WRITE-MESSAGE("Deleted DistributeFile Date:" + STRING(DistributeFile.DATE) ,"RECEIVE-DATA").
            DELETE DistributeFile NO-ERROR. 
            IF ERROR-STATUS:ERROR THEN
                LOG-MANAGER:WRITE-MESSAGE("Delete ERROR","ERROR").
        END.
    END.
END.
                             
LOG-MANAGER:WRITE-MESSAGE("Delete File Request -COMPLETE- From:Kasse=" + STRING(ipiKasseNr) + " Butikk=" + STRING(ipiButikknr),"RECEIVE-DATA").
