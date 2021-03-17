


FUNCTION  getFileName RETURNS CHAR (INPUT ipcFileName AS CHAR) : 
    DEF VAR pathDelimiter AS CHAR NO-UNDO. 
    ipcFileName = ENTRY(1,ipcFileName,";").
    pathDelimiter = IF OPSYS = "UNIX" THEN CHR(47) ELSE CHR(92).
    IF TRIM(ipcFileName) = ""  THEN RETURN ?.
    IF  R-INDEX(ipcFileName,PathDelimiter) = 0 THEN RETURN ipcFilename. 
    ipcFilename = SUBSTRING(ipcFileName,R-INDEX(ipcFileName,PathDelimiter) + 1).
    IF TRIM(ipcFileName) = ""  THEN RETURN ?. ELSE RETURN ipcFileName.
END. 


FUNCTION getFilePath RETURNS CHAR (INPUT ipcFileName AS CHAR) : 
    DEF VAR cPara2 AS CHAR NO-UNDO.
    DEF VAR pathDelimiter AS CHAR NO-UNDO. 
    IF NUM-ENTRIES(ipcFileName,";") = 2 THEN
        ASSIGN cPara2 = ENTRY(2,ipcFileName,";")
               ipcFileName = ENTRY(1,ipcFileName,";").

    pathDelimiter = IF OPSYS = "UNIX" THEN CHR(47) ELSE CHR(92).
    IF TRIM(ipcFileName) = ""  THEN RETURN ?.
    IF  R-INDEX(ipcFileName,PathDelimiter) = 0 THEN RETURN "". 
    ipcFilename = SUBSTRING(ipcFileName,1,R-INDEX(ipcFileName,PathDelimiter)).
    IF NOT ipcFileName = "" AND cPara2 <> "" THEN
        ipcFileName = ipcFileName + ";" + cPara2.
    IF TRIM(ipcFileName) = ""  THEN RETURN ?. ELSE RETURN ipcFileName.
END. 


DEFINE TEMP-TABLE ClientFile NO-UNDO
    FIELD FILESEQ    AS INT 
    FIELD FileID     AS CHAR 
    FIELD FILENAME   AS CHAR FORMAT "x(60)"
    FIELD FILEPATH   AS CHAR FORMAT "x(50)"
    FIELD FileWriteStatus AS LOG 
    FIELD FileObject AS BLOB . 

DEFINE TEMP-TABLE ServerFile  NO-UNDO LIKE ClientFile . 

DEFINE INPUT         PARAMETER ipiButikknr AS INT NO-UNDO. 
DEFINE INPUT         PARAMETER ipiKassenr  AS INT NO-UNDO. 
DEFINE INPUT-OUTPUT  PARAMETER TABLE FOR ClientFile .
DEFINE INPUT         PARAMETER ipcToDirectory AS CHAR NO-UNDO. 
DEFINE OUTPUT        PARAMETER TABLE FOR ServerFile .
DEFINE OUTPUT        PARAMETER lStatus AS LOG NO-UNDO. 

DEFINE VARIABLE cFullPath   AS CHAR NO-UNDO. 
DEFINE VARIABLE cFileName   AS CHAR NO-UNDO. 
DEFINE VARIABLE lWriteError AS LOGICAL INIT FALSE. 
DEFINE VARIABLE giCnt       AS INT NO-UNDO. 
DEFINE VARIABLE lok         AS LOGICAL NO-UNDO. 
DEFINE VARIABLE Tmp_ClientFileName AS CHAR NO-UNDO. 
DEFINE VARIABLE OS-Error-Status AS INT NO-UNDO. 

LOG-MANAGER:WRITE-MESSAGE("Data From:Kasse=" + STRING(ipiKasseNr) + " Butikk=" + STRING(ipiButikknr),"RECEIVE-DATA").

IF ipcToDirectory = ? OR 
   ipcToDirectory = "" THEN
   ipcToDirectory = "".

IF ipcToDirectory NE "" THEN ipcToDirectory = ipcToDirectory + '\'.

lStatus = TRUE. 

DEFINE BUFFER bfKasse FOR Kasse. 

FIND FIRST bfKasse WHERE 
    bfKasse.Butikknr = ipiButikknr  AND 
    bfKasse.Kassenr  = ipiKasseNr NO-LOCK NO-ERROR. 

IF NOT AVAIL bfKasse THEN RETURN. 

ipcToDirectory = bfkasse.Eljournalkatalog.

FILE-INFO:FILE-NAME = ipcToDirectory.
ipcToDirectory = FILE-INFO:FULL-PATHNAME + CHR(92).


FOR EACH ClientFile BY ClientFile.FileSeq: 
    LOG-MANAGER:WRITE-MESSAGE("File Seq# " + STRING(ClientFile.FileSeq) + " " + QUOTER(ipcToDirectory + ClientFile.FILENAME),"RECEIVE-DATA").

    Tmp_ClientFileName = "_" + ClientFile.FILENAME.

    IF NOT lWriteError THEN
    COPY-LOB ClientFile.FileObject TO FILE ipcToDirectory + Tmp_ClientFileName NO-ERROR.

    FILE-INFO:FILENAME = ipcToDirectory + Tmp_ClientFileName. 

    IF ERROR-STATUS:ERROR OR FILE-INFO:FULL-PATHNAME = ?  THEN 
    DO:
       LOG-MANAGER:WRITE-MESSAGE("Can't write file to:" + QUOTER(ipcToDirectory + ClientFile.FILENAME),"ERRROR").
       lWriteError = TRUE.
    END.
    ELSE 
    DO:
        OS-RENAME VALUE(ipcToDirectory + Tmp_ClientFileName) VALUE(ipcToDirectory + ClientFile.FILENAME) .
        os-error-status = OS-ERROR. 
        IF os-error-status NE 0 THEN
        DO:
           LOG-MANAGER:WRITE-MESSAGE("Can't rename file - OS error#" + STRING(OS-ERROR,"99"),"ERRROR").
           lWriteError = TRUE.
        END. 
    END. 

    IF lWriteError THEN 
    DO:
        LOG-MANAGER:WRITE-MESSAGE("Canceling write to:" + QUOTER(ipcToDirectory + ClientFile.FILENAME),"ERRROR").
        ClientFile.FileWriteStatus = FALSE. 
    END. 
END.


LOG-MANAGER:WRITE-MESSAGE("Transf. Data To:Kasse=" + STRING(ipiKasseNr) + " Butikk=" + STRING(ipiButikknr),"SEND-DATA").


FOR EACH  DistributeFile WHERE 
          DistributeFile.Butikk   = ipiButikkNr 
          NO-LOCK BY DistributeFile.DATE :

    FOR EACH  DistributeFileReceiver WHERE 
              DistributeFileReceiver.DistributefileID = DistributeFile.Id AND
              DistributeFileReceiver.kassenr = ipiKasseNr
            NO-LOCK :                                            
        
         LOG-MANAGER:WRITE-MESSAGE("Available File:" + QUOTER(DistributeFile.FILENAME),"SEND-DATA").
         /* vi måste ta FILENAME först eftersom vi ändrar FILEPATH om vi skall göra */
         /* något annat än sända fil för inläsning i.e kanske spara ngn annan stans ;SAVE */
         CREATE ServerFile. 
         ServerFile.FILENAME = getFileName(DistributeFile.FILENAME).
         ServerFile.FILEPATH = getFilePath(DistributeFile.FILENAME).
         ServerFile.FIleSeq  = giCnt. 
         ServerFile.FileId   = DistributeFile.id . 

         LOG-MANAGER:WRITE-MESSAGE("File:" + QUOTER(ServerFile.FilePath + "\" + ServerFile.FILENAME),"SEND-DATA").
         giCnt = giCnt + 1. 
         
         COPY-LOB FROM DistributeFile.FileObject TO ServerFile.FileObject NO-ERROR.
         IF ERROR-STATUS:ERROR THEN 
         DO:
             LOG-MANAGER:WRITE-MESSAGE("Can't Copy blob to Temp-table" ,"ERRROR").
         END. 
    END.
END. 

/* TN 5/5-20 Logger siste send/hent av data fra kassen. */
RUN logPosBongOverfort.p (ipiButikkNr, ipiKasseNr) NO-ERROR.




