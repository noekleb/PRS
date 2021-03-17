
PROCEDURE Example_1 : 
                          
    DEFINE VARIABLE cHost     AS CHAR INIT 'localhost' NO-UNDO.
    DEFINE VARIABLE cUser     AS CHAR INIT 'tomn'   NO-UNDO.
    DEFINE VARIABLE cPassword AS CHAR INIT 'tomno'   NO-UNDO. 
    DEFINE VARIABLE cFileName AS CHAR INIT 'c:\temp\test.txt' NO-UNDO.
    DEFINE VARIABLE lSuccess  AS LOGICAL NO-UNDO. 
    
    /* bruker default name test.txt for transf file */
    /* Ingen rename to file */ 
    /* logfile default = ftpputfile.log */ 

    RUN FtpPutFile.p (cHost,cUser,cPassword,cFileName,'','','', OUTPUT lSuccess) . 
    MESSAGE lSuccess VIEW-AS ALERT-BOX. 
END. 


PROCEDURE Example_2 : 
                          
    DEFINE VARIABLE cHost     AS CHAR INIT '192.168.0.115' NO-UNDO.
    DEFINE VARIABLE cUser     AS CHAR INIT 'master'   NO-UNDO.
    DEFINE VARIABLE cPassword AS CHAR INIT 'donald'   NO-UNDO. 
    DEFINE VARIABLE cFileName AS CHAR INIT 'c:\temp\test.txt' NO-UNDO.
    DEFINE VARIABLE cTransferFilename AS CHAR INIT 'test__.txt' NO-UNDO.
    DEFINE VARIABLE cRenameToFileName AS CHAR INIT '__test.txt' NO-UNDO. 
    DEFINE VARIABLE cLogFileName AS CHAR INIT 'FTP_LOG.log' NO-UNDO. 

    DEFINE VARIABLE lSuccess  AS LOGICAL NO-UNDO. 
    
    RUN FtpPutFile.p (cHost,cUser,cPassword,cFileName,cTransferFilename,cRenameToFileName,cLogFileName, OUTPUT lSuccess) . 
    MESSAGE lSuccess VIEW-AS ALERT-BOX. 
END. 


RUN example_2.
