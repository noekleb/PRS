          

&SCOPED-DEFINE PackageName com.filetools

USING {&PackageName}.filexmlimport.
USING {&PackageName}.fileListDir.


DEFINE VARIABLE cDirectory AS CHAR NO-UNDO.
DEFINE VARIABLE filexmlImport AS fileXmlImport.
DEFINE VARIABLE lOK AS LOGICAL INIT FALSE .

    
    DEFINE TEMP-TABLE fileSpool NO-UNDO 
        FIELD id AS CHAR FORMAT "X(30)"         
        FIELD Sequence AS INTEGER 
        FIELD ConfigurationFile AS CHAR FORMAT "X(30)"
        FIELD FileInfoId AS CHAR 
        FIELD inProcess AS LOGICAL INIT FALSE
        FIELD Processed AS LOGICAL INIT FALSE 
        FIELD MultiTarget AS LOGICAL INIT FALSE 
        FIELD TargetFtpHostIp AS CHAR FORMAT "X(20)" 
        FIELD TargetFtpUser AS CHAR 
        FIELD TargetFtpPassword AS CHAR 
        
        FIELD TargetFileName AS CHAR FORMAT "X(30)"
        FIELD TargetFilePath AS CHAR FORMAT "X(30)"
        FIELD TargetFullPath AS CHAR FORMAT "X(30)"
        FIELD TargetFileOverWrite AS LOGICAL INIT FALSE 
        FIELD TargetTransferFileName AS CHAR FORMAT "X(30)"
        FIELD TargetTransferFullPath AS CHAR FORMAT "X(30)"
    
        FIELD SourceOriginalFileName AS CHAR FORMAT "X(30)"
        FIELD SourceOriginalFilePath AS CHAR FORMAT "X(30)"
        FIELD SourceOriginalFullPath AS CHAR FORMAT "X(30)"
        FIELD SourceOriginalCreateDataTime AS DATETIME 
        
        FIELD SourceCurrentFullPath AS CHAR FORMAT "X(30)"
        FIELD SourceCurrentFileName AS CHAR FORMAT "X(30)" 
    
        FIELD SourceSpoolFullPath AS CHAR  FORMAT "X(30)"
        FIELD SourceSpoolFileName AS CHAR  FORMAT "X(30)"
        FIELD SourceSpoolFileNameXML AS CHAR FORMAT "X(30)"
        FIELD SourceSpoolFullPathXML AS CHAR FORMAT "X(30)"
        
        FIELD SourceTransferFileRename AS LOGICAL INIT FALSE
        FIELD SourceTransferFileName AS CHAR FORMAT "X(30)"
        FIELD SourceTransferFullPath AS CHAR FORMAT "X(30)"
        FIELD SourceCompressFile AS LOGICAL INIT FALSE
        FIELD SourceComressed AS LOGICAL INIT FALSE
        FIELD SourceCompressFileName AS CHAR FORMAT "X(30)"
        FIELD SourceCompressFullPath AS CHAR FORMAT "X(30)"
    
        FIELD TransferFailure AS LOGICAL INIT FALSE 
        FIELD TransferFailureMessage AS CHAR FORMAT "X(30)"
        FIELD TransferFailureDirectory AS CHAR 
        FIELD TransferSuccessDirectory AS CHAR 
        FIELD TransferSuccess AS LOGICAL INIT FALSE
        FIELD TransferStartDateTime AS DATETIME
        FIELD TransferEndDateTime AS DATETIME 
        FIELD TransferTime AS INTEGER 
        FIELD TransferAttempts AS INTEGER INIT 0 
        FIELD TransferRetries AS INTEGER INIT 0 
        FIELD TransferRetryStartDateTime AS DATETIME 
        FIELD CreateDateTime AS DATETIME INIT NOW 
        FIELD ModDateTime AS DATETIME INIT NOW 
        INDEX idx1 id sequence MultiTarget. 
    

LOG-MANAGER:LOGFILE-NAME = "SpoolFileFix.log".
LOG-MANAGER:LOGGING-LEVEL = 3. 
LOG-MANAGER:CLEAR-LOG().     

DEFINE VARIABLE hbuffer as handle NO-UNDO. 

hBuffer = BUFFER fileSpool:handle. 

cDirectory = SESSION:PARAMETER.
IF cDirectory = ? OR cDirectory = "" THEN cDirectory = ".". 

DEFINE VARIABLE fileListDir AS fileListDir.

DO ON ERROR UNDO, LEAVE ON QUIT UNDO, LEAVE ON STOP UNDO, LEAVE:

    fileListDir   = NEW fileListDir  (cDirectory,"spool.*.xml",lok). 
    fileXmlImport = NEW filexmlImport( fileListDir, INPUT-OUTPUT hBuffer,? /*XSD-File */ ,OUTPUT lok).
 
END.

DELETE OBJECT fileXmlImport NO-ERROR.
DELETE OBJECT fileListDir NO-ERROR. 



DEFINE VARIABLE iCnt AS INT NO-UNDO.
DEFINE VARIABLE cDosFile AS CHAR INIT "spoolfilefix.bat". 

DEFINE STREAM ut.
OUTPUT STREAM ut TO VALUE (cDosFile). 


FOR EACH filespool NO-LOCK: 

    icnt = icnt + 1. 
    DISP icnt filespool.TargetFtpHostIp 
              SourceOriginalFileName 
              SourceSpoolFullPathXML . 
   
    put stream ut unformatted "del "   + SourceSpoolFullPathXML  SKIP.    
    put stream ut unformatted "move "  + SourceSpoolFullPath    + ' ' + SourceOriginalFullPath SKIP.

END.
