
DEFINE TEMP-TABLE ttFile NO-UNDO
    FIELD FILENAME   AS CHAR FORMAT "x(60)"
    FIELD FILEPATH   AS CHAR FORMAT "x(50)"
    FIELD FileObject AS BLOB . 

DEFINE INPUT  PARAMETER TABLE FOR ttFile .
DEFINE INPUT  PARAMETER ipcToDirectory AS CHAR NO-UNDO. 
DEFINE OUTPUT PARAMETER lStatus AS LOG NO-UNDO. 

IF ipcToDirectory = ? OR 
   ipcToDirectory = "" THEN
   ipcToDirectory = "".

IF ipcToDirectory NE "" THEN ipcToDirectory = ipcToDirectory + IF OPSYS = "UNIX" THEN CHR(47) ELSE CHR(92).


FOR EACH ttFile : 
    COPY-LOB ttFile.FileObject TO FILE ipcToDirectory + ttFile.FILENAME. 
END.

lStatus = TRUE. 

RETURN.
