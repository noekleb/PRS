 
 /*------------------------------------------------------------------------
    File        : JBoxXML
    Purpose     : Read and write xml document(s)
    Syntax      : 
    Description : Management of relationships between fields and buffer
    Author(s)   : brynjar
    Created     : Thu feb 18 13:18:49 CEST 2016
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.


CLASS JBoxXml: 
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
  DEF VAR hSourceProc   AS HANDLE NO-UNDO.
  DEF VAR hXmlLib       AS HANDLE NO-UNDO.
  DEF VAR cDocName      AS CHAR   NO-UNDO INIT "class".
	  
/*
  DEF PUBLIC PROPERTY queryOnly AS LOG
      GET():
        RETURN DYNAMIC-FUNCTION("getAttribute",THIS-OBJECT:BUFFER-HANDLE,"queryOnly") = "yes".                    
      END GET. 
      SET(INPUT ibQueryOnly AS LOG):
        DYNAMIC-FUNCTION("setAttribute",THIS-OBJECT:BUFFER-HANDLE,"queryOnly",IF ibQueryOnly THEN "yes" ELSE "").
      END SET. 
	*/
		
  CONSTRUCTOR PUBLIC JBoxXML ():
	  SUPER ().
		
    IF PROVERSION < "11" THEN hSourceProc = DYNAMIC-FUNCTION ("getProcedureHandle",PROGRAM-NAME(2)).
    ELSE hSourceProc = SOURCE-PROCEDURE.
		
		InitializeComponent().       
	END CONSTRUCTOR.
	
	METHOD PRIVATE LOGICAL InitializeComponent():
	  RUN JBoxXmlLib.p PERSIST SET hXmlLib.
	  RUN initXMLdoc IN hXmlLib (cDocName).	
	  RETURN YES.
	END METHOD.

  METHOD PUBLIC LOGICAL loadDoc(INPUT ilcDoc AS LONGCHAR):
    
    RUN deleteDocs IN hXmlLib.
    RUN loadXML IN hXmlLib (cDocName,ilcDoc).
    RETURN YES.                                    
    
  END METHOD.  
	  
  METHOD PUBLIC LOGICAL loadDoc(INPUT icFileName AS CHAR):
    DEF VAR lcDoc AS LONGCHAR NO-UNDO.
     
    IF SEARCH(icFileName) NE ? THEN DO:
      FILE-INFO:FILE-NAME = SEARCH(icFileName).
      COPY-LOB FROM FILE FILE-INFO:FULL-PATHNAME TO lcDoc NO-ERROR.
      IF NOT ERROR-STATUS:ERROR THEN
        RETURN THIS-OBJECT:loadDoc(lcDoc).
      ELSE RETURN NO.
    END.
    ELSE RETURN NO.                                    
  END METHOD.  

  METHOD PUBLIC LOGICAL dumpNodes():
    
    RUN dumpXmlNodes IN hXmlLib (cDocName).
    RETURN YES.                                    
    
  END METHOD.  

  METHOD PUBLIC CHARACTER getValue(INPUT icNode AS CHAR):
    
    DEF VAR cValue AS CHAR NO-UNDO.
    
    RUN getNodeValue IN hXmlLib (cDocName,icNode,OUTPUT cValue).
    RETURN cValue.                                    
    
  END METHOD.  


/*
  METHOD PUBLIC LOGICAL saveNewRecord(INPUT icFields AS CHAR,
                                      INPUT icValues AS CHAR):
                                        
    THIS-OBJECT:objectState = "new".
    DYNAMIC-FUNCTION("setAttribute",BUFFER-HANDLE,"bufferExtraFields",icFields).
    DYNAMIC-FUNCTION("setAttribute",BUFFER-HANDLE,"bufferExtraValues",icValues).
                                        
    RUN InvokeMethod(BUFFER-HANDLE,"SaveRecord"). 
    RETURN DYNAMIC-FUNCTION("getTransactionMessage") = "".
  END METHOD.  

  METHOD PUBLIC LOGICAL refreshFields():
    RETURN DYNAMIC-FUNCTION("DisplayFieldMap",BUFFER-HANDLE). 
  END METHOD.  

  METHOD PUBLIC HANDLE getUpdateFieldHandle(INPUT icFieldName AS CHAR):
    IF LOOKUP(icFieldName,THIS-OBJECT:updateFields) > 0 THEN
      RETURN WIDGET-HANDLE(ENTRY(LOOKUP(icFieldName,THIS-OBJECT:updateFields),THIS-OBJECT:updateWidgets)).
    ELSE
      RETURN ?. 
  END METHOD.  
*/
  
	DESTRUCTOR PUBLIC JBoxXml ( ):
	  APPLY "close" TO hXmlLib.
	  DELETE PROCEDURE hXmlLib. 
	END DESTRUCTOR.

END CLASS.