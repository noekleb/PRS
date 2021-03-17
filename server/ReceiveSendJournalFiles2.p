


DEFINE VARIABLE giCnt    AS INT NO-UNDO. 
DEFINE VARIABLE lok AS LOGICAL NO-UNDO. 

FUNCTION  getFileName RETURNS CHAR (INPUT ipcFileName AS CHAR) : 
    DEF VAR pathDelimiter AS CHAR NO-UNDO. 
    pathDelimiter = IF OPSYS = "UNIX" THEN CHR(47) ELSE CHR(92).
    IF TRIM(ipcFileName) = ""  THEN RETURN ?.
    IF  R-INDEX(ipcFileName,PathDelimiter) = 0 THEN RETURN ipcFilename. 
    ipcFilename = SUBSTRING(ipcFileName,R-INDEX(ipcFileName,PathDelimiter) + 1).
    IF TRIM(ipcFileName) = ""  THEN RETURN ?. ELSE RETURN ipcFileName.
END. 


FUNCTION getFilePath RETURNS CHAR (INPUT ipcFileName AS CHAR) : 
    DEF VAR pathDelimiter AS CHAR NO-UNDO. 
    pathDelimiter = IF OPSYS = "UNIX" THEN CHR(47) ELSE CHR(92).
    IF TRIM(ipcFileName) = ""  THEN RETURN ?.
    IF  R-INDEX(ipcFileName,PathDelimiter) = 0 THEN RETURN "". 
    ipcFilename = SUBSTRING(ipcFileName,1,R-INDEX(ipcFileName,PathDelimiter)).
    IF TRIM(ipcFileName) = ""  THEN RETURN ?. ELSE RETURN ipcFileName.
END. 


DEFINE TEMP-TABLE ClientFile NO-UNDO
    FIELD FILESEQ    AS INT 
    FIELD FileID     AS CHAR 
    FIELD FILENAME   AS CHAR FORMAT "x(60)"
    FIELD FILEPATH   AS CHAR FORMAT "x(50)"
    FIELD FileObject AS BLOB . 

DEFINE TEMP-TABLE ServerFile  NO-UNDO LIKE ClientFile . 

DEFINE INPUT  PARAMETER ipiButikknr AS INT NO-UNDO. 
DEFINE INPUT  PARAMETER ipiKassenr  AS INT NO-UNDO. 
DEFINE INPUT  PARAMETER TABLE FOR ClientFile .
DEFINE INPUT  PARAMETER ipcToDirectory AS CHAR NO-UNDO. 
DEFINE OUTPUT PARAMETER TABLE FOR ServerFile .
DEFINE OUTPUT PARAMETER lStatus AS LOG NO-UNDO. 

DEFINE VARIABLE cFullPath AS CHAR NO-UNDO. 
DEFINE VARIABLE cFileName AS CHAR NO-UNDO. 

LOG-MANAGER:WRITE-MESSAGE("Data From:Kasse=" + STRING(ipiKasseNr) + " Butikk=" + STRING(ipiButikknr),"RECEIVE-DATA").

IF ipcToDirectory = ? OR 
   ipcToDirectory = "" THEN
   ipcToDirectory = "".

IF ipcToDirectory NE "" THEN ipcToDirectory = ipcToDirectory + '\'.

lStatus = TRUE. 

FOR EACH ClientFile : 
    LOG-MANAGER:WRITE-MESSAGE(ipcToDirectory + ClientFile.FILENAME,"RECEIVE-DATA").

    COPY-LOB ClientFile.FileObject TO FILE ipcToDirectory + ClientFile.FILENAME NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
       lStatus = FALSE. 
END.
                             
LOG-MANAGER:WRITE-MESSAGE("Transf. data to Kasse:" + STRING(ipiKasseNr),"SEND-DATA").

FIND FIRST DistributeFileReceiver WHERE 
           DistributeFileReceiver.kasse = ipiKasseNr NO-LOCK NO-ERROR. 

IF AVAIL DistributeFileReceiver THEN
DO:
     FOR EACH  DistributeFile, EACH 
               DistributeFileReceiver WHERE 
               DistributeFileReceiver.DistributeFileId =  DistributeFile.id AND
               DistributeFileReceiver.kassenr = ipiKasseNr AND 
               DistributeFile.Butikk   = ipiButikknr NO-LOCK BY DistributeFile.DATE:
         
         LOG-MANAGER:WRITE-MESSAGE("Available File:" + DistributeFile.FILENAME,"SEND-DATA").
         
         FILE-INFO:FILE-NAME = DistributeFile.FILENAME.
         cfullpath = FILE-INFO:FULL-PATHNAME.
         cfilename = getFileName(FILE-INFO:FILE-NAME).

         /*
         IF cfilename = ? THEN
         DO:
             LOG-MANAGER:WRITE-MESSAGE(DistributeFile.FILENAME + " Does not exist!" ,"ERRROR").
              NEXT . 
         END.
         */

         CREATE ServerFile. 
         ServerFile.FILEPATH = getFilePath(cFullPath).
         ServerFile.FILENAME = getFileName(DistributeFile.FILENAME).
         ServerFile.FIleSeq  = giCnt. 
         ServerFile.FileId   = DistributeFile.id . 

         LOG-MANAGER:WRITE-MESSAGE(ServerFile.FilePath + "\" + ServerFile.FILENAME,"SEND-DATA").

         giCnt = giCnt + 1. 
         /*
         COPY-LOB FILE ServerFile.FilePath + "\" + ServerFile.FILENAME TO ServerFile.FileObject NO-ERROR.
         */
         COPY-LOB FROM DistributeFile.FileObject TO ServerFile.FileObject NO-ERROR.

     END. 
END.





