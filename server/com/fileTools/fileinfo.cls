
&SCOPED-DEFINE PackageName com.filetools
ROUTINE-LEVEL ON ERROR UNDO, THROW . 


CLASS {&PackageName}.fileinfo INHERITS {&PackageName}.fileutil USE-WIDGET-POOL: 
 
    DEFINE PUBLIC VARIABLE suffix AS CHAR NO-UNDO.
    DEFINE PUBLIC VARIABLE prefix AS CHAR NO-UNDO. 
    DEFINE PUBLIC VARIABLE filename AS CHAR NO-UNDO.
    DEFINE PUBLIC VARIABLE path AS CHAR NO-UNDO.
    DEFINE PUBLIC VARIABLE fullpath AS CHAR NO-UNDO.
    DEFINE PUBLIC VARIABLE type AS CHAR NO-UNDO.
    DEFINE PUBLIC VARIABLE valid AS LOG NO-UNDO.
    DEFINE PUBLIC VARIABLE size AS INT NO-UNDO.
    DEFINE PUBLIC VARIABLE createDateTime AS DATETIME. 
    DEFINE PUBLIC VARIABLE modDateTime AS DATETIME. 
    DEFINE PUBLIC VARIABLE signature AS CHAR.
    DEFINE VARIABLE lStatus AS LOGICAL NO-UNDO. 

    CONSTRUCTOR PUBLIC fileinfo(INPUT ipcFileName AS CHARACTER,OUTPUT plok AS LOGICAL):
                

        SUPER(lstatus).
        plOk = FALSE. 

        IF NOT lStatus THEN 
        DO:
            AddErrorMessage("SUPER() constructor failed").
            RETURN.
        END.

        DEFINE VARIABLE mObj AS MEMPTR NO-UNDO. 
        DEFINE VARIABLE rObj AS RAW NO-UNDO. 
        DEFINE VARIABLE iAt AS INT NO-UNDO. 

        IF NOT checkOsFile(ipcFileName) THEN 
        DO:
            AddErrorMessage("file does not exist!").
            RETURN . 
        END.                                
            
        FILE-INFO:FILE-NAME = getOsFileFullPath(ipcFileName).
        fullpath = FILE-INFO:FULL-PATHNAME.
        filename = getFileName(FILE-INFO:FILE-NAME).
        type = FILE-INFO:FILE-TYPE.
        size = FILE-INFO:FILE-SIZE.
        createdatetime = DATETIME(FILE-INFO:FILE-CREATE-DATE,FILE-INFO:FILE-CREATE-TIME * 1000).
        moddatetime =  DATETIME(FILE-INFO:FILE-MOD-DATE,FILE-INFO:FILE-MOD-TIME * 1000).
        
        IF size GE 20 AND TYPE BEGINS "F" THEN
        DO:
            SET-SIZE(mObj) = 10. 
            COPY-LOB FROM FILE fullPath STARTING AT 1 FOR 10 TO OBJECT mObj NO-CONVERT NO-ERROR.
            rObj = mObj. 
            signature = SUBSTRING(STRING(robj),6,12).
            iAt = size / 2.
            COPY-LOB FROM FILE fullPath STARTING AT iAt FOR 10 TO OBJECT mObj NO-CONVERT NO-ERROR.
            rObj = mObj. 
            signature = signature + SUBSTRING(STRING(robj),6,12).
            COPY-LOB FROM FILE fullPath STARTING AT (size - 10) FOR 10 TO OBJECT mObj NO-CONVERT NO-ERROR.
            rObj = mObj. 
            SET-SIZE(mObj) = 0. 
            signature = ENCODE(STRING(size) + ":" + signature + SUBSTRING(STRING(robj),6,12) + STRING(size)).
        END. 

        plOk = true. 

    END CONSTRUCTOR.
                             /*
    METHOD PRIVATE INTEGER PathDelimiter ():
	    RETURN IF OPSYS = "UNIX" THEN CHR(47) ELSE CHR(92). 
	END. 


    METHOD PRIVATE CHARACTER getFileName (INPUT ipcFileName AS CHAR) : 

	    IF TRIM(ipcFileName) = ""  THEN RETURN ?.
	    IF  R-INDEX(ipcFileName,PathDelimiter()) = 0 THEN RETURN ipcFilename. 
	    ipcFilename = SUBSTRING(ipcFileName,R-INDEX(ipcFileName,PathDelimiter()) + 1).
	    IF TRIM(ipcFileName) = ""  THEN RETURN ?. ELSE RETURN ipcFileName.
	END. 

                               */
    DESTRUCTOR PUBLIC fileinfo():
        DestroySOAPService().
    END DESTRUCTOR.
    

    /*
    METHOD PUBLIC BeginTransactionSuccess BeginTransaction(INPUT cCustomerId AS CHAR,INPUT cPassword AS CHAR,INPUT cProductId AS CHAR,OUTPUT oplOk AS LOG):
        
        DEFINE VARIABLE BeginTransactionSuccess AS BeginTransactionSuccess NO-UNDO.
        DEFINE VARIABLE lcSerializedRequest AS LONGCHAR NO-UNDO. 
        DEFINE VARIABLE lcXML AS LONGCHAR NO-UNDO.
        DEFINE VARIABLE lOk AS LOGICAL NO-UNDO. 
            
        SetSOAPService("-Service CAPServer -Port CAPServer", lOK).
        lcSerializedRequest = getSerializedRequest1(cPassword,cProductid,cshopid,ctillid,cReceiptid,cVersion,cCustomerid,cTransactionId,OUTPUT lok).
    
        IF LOG-MANAGER:LOGGING-LEVEL GE 3 THEN COPY-LOB lcSerializedRequest TO FILE "dump_Call_" + STRING(ETIME) + ".xml". 
    
        RUN beginTransaction IN SOAPPort(INPUT lcSerializedRequest, OUTPUT lcXML) .
    
        IF ERROR-STATUS:ERROR THEN 
        DO:
            AddErrorMessage("RUN 'beginTransaction' IN SOAPPort failed").
            oplOk = FALSE. 
            RETURN ?.
        END.                
    
        COPY-LOB lcXML TO FILE "dump_Result_" + STRING(ETIME) + ".xml". 
        BeginTransactionSuccess = NEW BeginTransactionSuccess(lcXML, lOK).
    
        IF NOT lOK THEN 
        DO:
             AddErrorMessage("NEW BeginTransactionSuccess failed").
             oplOk = FALSE. 
             RETURN ?.
        END.
    
       oplOk = TRUE.
       RETURN BeginTransactionSuccess.
    END METHOD.
    



    METHOD PRIVATE VOID SetSOAPService(INPUT pcArguments AS CHARACTER,
                                       OUTPUT plOK AS LOGICAL):
        ASSIGN plOK = FALSE.
        
        DestroySOAPService().


        /* Make sure we can access the WSDL: */
        FILE-INFO:FILE-NAME = SEARCH(WSDL).
        IF FILE-INFO:FILE-TYPE = ? OR
           (NOT FILE-INFO:FILE-TYPE MATCHES "*R*" AND
                (FILE-INFO:FILE-TYPE MATCHES "*F*" OR
                 FILE-INFO:FILE-TYPE MATCHES "*M*")) THEN DO:
            AddErrorMessage(
                SUBSTITUTE("Can't open WSDL '&1' (FILE-INFO:FILE-TYPE '&2')", WSDL, FILE-INFO:FILE-TYPE)).
            RETURN.
        END.
                                    
        SOAPService:CONNECT(SUBSTITUTE("-WSDL &1 &2", WSDL, pcArguments)) NO-ERROR.
                  
        /* SOAPPort:SET-CALLBACK("REQUEST-HEADER","InsertSessionIDHeader",RequestHeaderHandler).*/                      
        ASSIGN plOK = TRUE.
        RETURN.
    END METHOD.
    */

    METHOD PRIVATE VOID DestroySOAPService():
    
    END METHOD /* DestroySOAPService() */.
    
END CLASS.
