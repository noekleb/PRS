          
&SCOPED-DEFINE PackageName com.filetools

USING {&PackageName}.filexmlimport.
USING {&PackageName}.fileListDir.

DEFINE VARIABLE filexmlImport AS fileXmlImport.
DEFINE VARIABLE lOK AS LOGICAL INIT FALSE .

LOG-MANAGER:LOGFILE-NAME = "filexmlImport.log".
LOG-MANAGER:LOGGING-LEVEL = 3. 
LOG-MANAGER:CLEAR-LOG().                        
def var hbuffer as handle no-undo. 


DEFINE VARIABLE fileListDir AS fileListDir.

DO ON ERROR UNDO, LEAVE ON QUIT UNDO, LEAVE ON STOP UNDO, LEAVE:
              
    fileListDir   = NEW fileListDir("C:\_cvs.test\_FtpClient\sport1.config","ftpfileupload*.config.xml",lok). 
    fileXmlImport = NEW filexmlImport( fileListDir, INPUT-OUTPUT hBuffer,? /*XSD-File */ ,OUTPUT lok).
 
END.
DELETE OBJECT fileXmlImport NO-ERROR.
DELETE OBJECT fileListDir NO-ERROR. 

message hbuffer view-as alert-box. 

DEFINE VARIABLE hQuery AS HANDLE NO-UNDO. 

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(hBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + hBuffer:NAME).
hQuery:QUERY-OPEN().
        
def var y as char. 

def var icnt as int . 
DO WHILE hQuery:GET-NEXT(NO-LOCK,NO-WAIT) AND NOT hQuery:QUERY-OFF-END with frame xx :
                      icnt = icnt + 1. 
   disp  hBuffer::ftpHost format "x(30)" with frame xx down. 

   y =  Y + hBuffer::ftpHost + "|". 
            
END. 

output to c:\tmp\list.txt. 
put unformatted y . 
