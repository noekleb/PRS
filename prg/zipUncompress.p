                                                      
/*Simple procedure for uncompress of zip files - using 7z.exe*/ 

DEFINE VARIABLE gcReturnMessage AS CHAR NO-UNDO. 

DEFINE STREAM out-str. 

FUNCTION getFileName RETURNS CHAR 
    (INPUT ipcName AS CHAR): 

    DEFINE VARIABLE iCnt AS INT NO-UNDO.      
    DEFINE VARIABLE cPath AS CHAR NO-UNDO. 
    DEFINE VARIABLE cFileName AS CHAR NO-UNDO. 
    
    ipcName = REPLACE(ipcName,CHR(92),CHR(1)).
    ipcName = REPLACE(ipcName,CHR(47),CHR(1)).
    cFileName = ENTRY(NUM-ENTRIES(ipcName,chr(1)),ipcName,CHR(1)). 
    RETURN cFileName.
END. 



FUNCTION UnCompress RETURNS LOGICAL 
    (INPUT  ipcFileName AS CHAR, 
     INPUT  ipcToDirectory AS CHAR, 
     OUTPUT lStatus AS LOGICAL ):
    

    DEFINE VARIABLE cGzip AS CHAR INIT "cmd\7z.exe" NO-UNDO. 
    DEFINE VARIABLE cGzipOrig AS CHAR NO-UNDO. 
    DEFINE VARIABLE cFileName AS CHAR NO-UNDO. 
    DEFINE VARIABLE cFileExtention AS CHAR NO-UNDO. 
    DEFINE VARIABLE cFileNameShort AS CHAR NO-UNDO. 
    DEFINE VARIABLE cContent AS CHAR NO-UNDO. 
    DEFINE VARIABLE cTmpFileName AS CHAR NO-UNDO. 
    DEFINE VARIABLE cUnpackPath AS CHAR NO-UNDO. 
    DEFINE VARIABLE cUnpackCommand AS CHAR NO-UNDO.

    lStatus = FALSE. 

    cGzipOrig = cGzip. 
    FILE-INFO:FILE-NAME = SEARCH(cGzip). 
    cGzip = FILE-INFO:FULL-PATHNAME.
    
    IF cGzip = ? THEN RETURN FALSE. 

    IF ipcToDirectory = '' OR 
       ipcToDirectory = ? THEN 
        cUnpackPath = REPLACE(cGzip,cGzipOrig,''). 

    ELSE cUnpackPath = ipcToDirectory + "\". 

    FILE-INFO:FILE-NAME = ipcFilename. 
    cFileName = FILE-INFO:FULL-PATHNAME.
    IF cFileName = ? THEN 
    DO:
        gcReturnMessage = "File not found!," + QUOTER(ipcFileName).
        RETURN FALSE.
    END. 

    cFileNameShort = getFileName(cFileName).
    cFileExtention = SUBSTRING(cFileNameShort,R-INDEX(cFileNameShort,'.')) NO-ERROR.
    IF NOT cFileExtention = ".zip" THEN 
    DO:
        gcReturnMessage = "File not a .zip file!," + QUOTER(ipcFilename). 
        RETURN FALSE. 
    END. 
    
    cTmpFileName =  cUnpackPath +  SUBSTRING(cFileNameShort,1,(R-INDEX(cFileNameShort,'.')) - 1) + '.log'.
    OUTPUT STREAM out-str TO VALUE(cTmpFileName) APPEND. 
    PUT STREAM out-str UNFORMATTED FILL('-',80) SKIP.
    PUT STREAM out-str UNFORMATTED '       Unpacking files from:' + cFileNameShort  '  Date: ' TODAY ' Time:' STRING(TIME,'HH:MM') SKIP.
    PUT STREAM out-str UNFORMATTED FILL('-',80).
    OUTPUT STREAM out-str CLOSE. 
    
    cUnpackCommand = 'cd ' + cUnpackPath + ' && ' + cGzip + ' x ' + cFileName + ' -y >> ' + cTmpFileName. 
   
    OS-COMMAND SILENT VALUE(cUnpackCommand).

    OUTPUT STREAM out-str TO VALUE(cTmpFileName) APPEND. 
    PUT STREAM out-str UNFORMATTED FILL('-',80) SKIP.
    PUT STREAM out-str UNFORMATTED '       Unpacking files -Completed- ' SKIP.
    PUT STREAM out-str UNFORMATTED FILL('-',80) SKIP.
    OUTPUT STREAM out-str CLOSE. 

    lStatus = TRUE. 
    RETURN TRUE. 
END. 


    /* -- main --- */ 

    DEFINE INPUT PARAMETER  ipcUncompressedFile AS CHAR    NO-UNDO. 
    DEFINE INPUT PARAMETER  ipcToDirectory      AS CHAR    NO-UNDO. 
    DEFINE OUTPUT PARAMETER oplStatus           AS LOGICAL NO-UNDO. 
    
    DEFINE VARIABLE lStatus AS LOGICAL NO-UNDO. 
        
    lStatus = UnCompress(ipcUncompressedFile,ipcToDirectory,OUTPUT lStatus).
    
    RUN prg/setClientConnect.p ("PATCH-STATUS") NO-ERROR.
    
    IF NOT lStatus  THEN RETURN ERROR gcReturnMessage.  

    oplStatus = lstatus. 
    
    RETURN. 


