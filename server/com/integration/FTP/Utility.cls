 /*
  * This class provides a way to store and retrive error
  * message, and some miscellaneous utility functions.
  *
  * - PUBLIC VARIABLE ShortTypeName is the type name of this object,
  *   without the package name.
  * 
  * - METHOD PUBLIC CHARACTER PackagePath() returns the package path 
  *   of this object. The package path is the package name, with dots
  *   replaced by slashes. Useful for cases where you want to RUN
  *   a procedure stored in the same package as this Utility.
  *
  * - METHOD PUBLIC VOID AddErrorMessage(CHARACTER) adds an error 
  *   message to this object. Utility objects can have many error 
  *   messages.If ERROR-STATUS:ERROR, then messages available via 
  *   ERROR-STATUS:GET-MESSAGE() are also added.
  *
  * - METHOD PUBLIC VOID AddErrorMessagesFrom(Utility) takes an instance
  *   of Utility and adds the error messages from that instance to
  *   this instance. Useful for cases when one Utility calls
  *   methods on another Utility. Use AddErrorMessagesFrom()
  *   to capture any errors from the second object. For instance,
  *   give two objects, both derived from Utility:
  *
  *       Obj1.SomeMethod().
  *       result = Obj2.SomeOtherMethod().
  *       IF result NE "OK" THEN DO:
  *           Obj1.AddErrorMessage("Obj2.SomeOtherMethod() failed").
  *           Obj1.AddErrorMessagesFrom(Obj2).
  *       END.
  *      
  * - METHOD PUBLIC CHARACTER GetErrorMessage() returns all the
  *   error messages for this object, separated by the default
  *   delimiter (a newline character).
  *
  * - METHOD PUBLIC CHARACTER GetErrorMessage(CHARACTER) returns
  *   all the error messages for this object, separated by a
  *   delimiter you specify.
  *
  * - METHOD PUBLIC CHARACTER GetErrorMessage(INTEGER) returns
  *   only the error message for the index you provide. Error
  *   messages are indexed in the order added.
  *
  * - METHOD PUBLIC INTEGER GetNumErrorMessages() returns the
  *   number of error messages added to this object.
  *
  */
  
&SCOPED-DEFINE PackageName com.integration.FTP
&SCOPED-DEFINE DEFAULT_ERROR_MESSAGE_DELIMITER "~~n"
        
CLASS {&PackageName}.Utility USE-WIDGET-POOL:

    DEFINE PRIVATE VARIABLE ThisTypeName AS CHARACTER NO-UNDO.
    DEFINE PUBLIC VARIABLE ShortTypeName AS CHARACTER NO-UNDO.

    DEFINE PROTECTED TEMP-TABLE ErrorMessage NO-UNDO
        FIELD ErrorText AS CHARACTER.
    
    CONSTRUCTOR PUBLIC Utility (OUTPUT plOK AS LOGICAL):
        DEFINE VARIABLE ThisClass AS Progress.Lang.Class NO-UNDO.                        
        
        ASSIGN plOK = TRUE.
        
        ASSIGN ThisClass = GetClass()
               ThisTypeName = thisClass:TypeName.
        
        ASSIGN ShortTypeName = ENTRY(NUM-ENTRIES(ThisTypeName, "."), 
                                     ThisTypeName, ".").
                                                    
        DELETE OBJECT ThisClass.
    END CONSTRUCTOR.
    
    DESTRUCTOR PUBLIC Utility():
        EMPTY TEMP-TABLE ErrorMessage.
    END DESTRUCTOR.

    METHOD PUBLIC CHARACTER PackagePath():
        RETURN REPLACE("{&PackageName}", ".", "/") + "/".
    END.    
    
    METHOD PUBLIC VOID AddErrorMessage(INPUT pcMessage AS CHARACTER):
        DEFINE VARIABLE iErrorIndex AS INTEGER NO-UNDO.
        
        CREATE ErrorMessage.
        ASSIGN ErrorMessage.ErrorText = pcMessage.

        LOG (ErrorMessage.ErrorText).
        DO iErrorIndex = 1 TO ERROR-STATUS:NUM-MESSAGES:
            CREATE ErrorMessage.
            ASSIGN ErrorMessage.ErrorText =
                           ERROR-STATUS:GET-MESSAGE(iErrorIndex).
            LOG (ErrorMessage.ErrorText).
        END.
    END METHOD.

    METHOD PUBLIC VOID AddErrorMessagesFrom(INPUT pUtility AS {&PackageName}.Utility):
        DEFINE VARIABLE iLoginResultErrors AS INTEGER NO-UNDO.
        DEFINE VARIABLE iErrorIndex AS INTEGER NO-UNDO.
        
        IF NOT VALID-OBJECT(pUtility) THEN
            RETURN.
                
        ASSIGN iLoginResultErrors = pUtility:GetNumErrorMessages().
                
        DO iErrorIndex = 1 TO iLoginResultErrors:
            AddErrorMessage(pUtility:ShortTypeName + ": " +
                            pUtility:GetErrorMessage(iErrorIndex)).
        END.
    END METHOD.

    METHOD PUBLIC CHARACTER GetErrorMessage():
        RETURN GetErrorMessage({&DEFAULT_ERROR_MESSAGE_DELIMITER}).
    END METHOD.
    
    METHOD PUBLIC CHARACTER GetErrorMessage(INPUT pcDelimiter AS CHARACTER):
        DEFINE VARIABLE cErrorMessage AS CHARACTER NO-UNDO.
        
        FOR EACH ErrorMessage NO-LOCK:
            IF cErrorMessage > "" THEN
                ASSIGN cErrorMessage = cErrorMessage + pcDelimiter.
            ASSIGN cErrorMessage = cErrorMessage +
                                   ShortTypeName +
                                   ": " + ErrorMessage.ErrorText. 
        END.
        
        RETURN cErrorMessage.
    END METHOD.

    METHOD PUBLIC CHARACTER GetErrorMessage(INPUT piIndex AS INTEGER):
        DEFINE VARIABLE iErrorMessageIndex AS INTEGER NO-UNDO.
        
        FOR EACH ErrorMessage NO-LOCK:
            iErrorMessageIndex = iErrorMessageIndex + 1.
            IF iErrorMessageIndex = piIndex THEN
                RETURN ErrorMessage.ErrorText.
        END.
        
        RETURN "".
    END METHOD.

    METHOD PUBLIC INTEGER GetNumErrorMessages():
        DEFINE VARIABLE iErrorMessageCount AS INTEGER NO-UNDO.
        
        FOR EACH ErrorMessage NO-LOCK:
            iErrorMessageCount = iErrorMessageCount + 1.
        END.
        
        RETURN iErrorMessageCount.        
    END.
    
    METHOD PUBLIC VOID LOG(INPUT pcMessage AS CHARACTER):
        IF LOG-MANAGER:LOGGING-LEVEL GE 3 THEN
        LOG-MANAGER:WRITE-MESSAGE(pcMessage,''). 

    END METHOD.

    METHOD PUBLIC CHARACTER GET-ELEMENT (INPUT str AS CHAR , INPUT ipi AS INT,INPUT ipcS AS CHAR,INPUT ipcE AS CHAR):
        DEFINE VARIABLE iElements AS INTEGER NO-UNDO. 
        DEFINE VARIABLE cElement AS CHAR NO-UNDO. 
        DEFINE VARIABLE iIndex1 AS INT NO-UNDO. 
        DEFINE VARIABLE iLength AS INT NO-UNDO. 
    
        DO WHILE NUM-ENTRIES(str,ipcS) GT 1: 
           iIndex1 = INDEX(str,ipcs) + LENGTH(ipcs).
           iIndex1 = IF iIndex1 = 1 THEN ? ELSE iIndex1. 
           iLength = INDEX(SUBSTRING(str,iIndex1),ipcE) - 1.
           iLength = IF iLength = -1 THEN ? ELSE iLength. 
           cElement = SUBSTRING(str,iIndex1,iLength). 
           str = SUBSTRING(str,iIndex1 + iLength + 1).
           iElements = iElements + 1.    
           IF iElements = ipi THEN RETURN cElement. 
        END.
        RETURN ?.
    END.

    METHOD PUBLIC INTEGER NUM-ELEMENTS  (INPUT str AS CHAR, INPUT ipcS AS CHAR,INPUT ipcE AS CHAR):
        DEFINE VARIABLE iElements AS INTEGER NO-UNDO. 
        DEFINE VARIABLE cElement AS CHAR NO-UNDO. 
        DEFINE VARIABLE iIndex1 AS INT NO-UNDO. 
        DEFINE VARIABLE iLength AS INT NO-UNDO. 

        DO WHILE NUM-ENTRIES(str,ipcS) GT 1: 
           iIndex1 = INDEX(str,ipcs) + LENGTH(ipcs).
           iIndex1 = IF iIndex1 = 1 THEN ? ELSE iIndex1. 
           iLength = INDEX(SUBSTRING(str,iIndex1),ipcE) - 1.
           iLength = IF iLength = -1 THEN ? ELSE iLength. 
           cElement = SUBSTRING(str,iIndex1,iLength). 
           str = SUBSTRING(str,iIndex1 + iLength + 1).
           iElements = iElements + 1.    
        END.
        RETURN iElements.
    END.

    METHOD PUBLIC CHARACTER ISO-DATE-TODAY (INPUT i AS INT):
        IF i = 0 OR i = ? THEN RETURN ISO-DATE(today).
        RETURN ISO-DATE(today + i).
    END. 
    
    METHOD PUBLIC CHARACTER ISO-DATETIME-TODAY (INPUT i AS INT):
        DEFINE VARIABLE dtoday AS DATETIME NO-UNDO. 
        dToday = today + i. 
        IF i = 0 OR i = ? THEN RETURN ISO-DATE(dtoday).
        RETURN SUBSTRING(ISO-DATE(dtoday ),1,19) + "Z".
    END. 

    METHOD PUBLIC CHARACTER getStringValues (INPUT cString AS CHAR):
        DEFINE VARIABLE cFunction AS CHAR NO-UNDO. 
        DEFINE VARIABLE cFunctionResult AS CHAR NO-UNDO. 
        DEFINE VARIABLE cParameter AS CHAR NO-UNDO. 
        DEFINE VARIABLE iCounter AS INT NO-UNDO. 
    
        DO WHILE NUM-ELEMENTS(cString,"[","]") GE 1 :
            ASSIGN    
            cFunction       = GET-ELEMENT(cString,1,"[","]") 
            cString         = REPLACE(cString,"[" + cFunction + "]","&1")
            cParameter      = GET-ELEMENT(cFunction,1,"(",")")
            cFunction       = ENTRY(1,cFunction,"(").
            
            CASE cFunction : 
                WHEN "ISO-DATE-TODAY" THEN cFunctionResult = ISO-DATE-TODAY(INTEGER(cParameter)).
                WHEN "ISO-DATETIME-TODAY" THEN cFunctionResult = ISO-DATETIME-TODAY(INTEGER(cParameter)).
            END CASE.

            cString         = SUBSTITUTE(cString,cFunctionResult).
        END. 
        RETURN cString. 
    END. 

END CLASS.
