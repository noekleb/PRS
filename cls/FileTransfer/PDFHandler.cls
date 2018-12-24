@program FILE(name="PDFHandler.cls", module="AppServer").
@openapi.openedge.export FILE(type="REST", executionMode="singleton", useReturnValue="false", writeDataSetBeforeImage="false").
 
 /*------------------------------------------------------------------------
    File        : PDFHandler
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Administrator
    Created     : Wed Oct 15 15:23:04 EDT 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS PDFHandler: 
    DEFINE TEMP-TABLE ttFile NO-UNDO
        FIELD tcFileName AS CHARACTER
        FIELD tclbPayload AS CLOB.
        
        
    DEFINE TEMP-TABLE ttFileName NO-UNDO
        FIELD tiOrder    AS INTEGER
        FIELD tcFileName AS CHARACTER
        FIELD tchref     AS CHARACTER
        INDEX idxOrder IS PRIMARY UNIQUE tiOrder.
        
    DEFINE VARIABLE giOrder AS INTEGER NO-UNDO.
    
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
	METHOD PUBLIC VOID getFile( INPUT pcFileName AS CHARACTER, OUTPUT TABLE ttFile ):
		DEFINE VARIABLE lcFile AS LONGCHAR NO-UNDO.
		DEFINE VARIABLE lpFile    AS MEMPTR    NO-UNDO.
		
		EMPTY TEMP-TABLE ttFile.
		pcFileName = REPLACE(pcFileName,"%20"," ").
		
		COPY-LOB FROM FILE pcFileName TO lpFile.
		lcFile = BASE64-ENCODE(lpFile).
		
		CREATE ttFile.
		ASSIGN ttFile.tcFileName  = pcFileName
		       ttFile.tclbPayload = lcFile.
		       
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
	METHOD PUBLIC VOID getFileList(INPUT pcURI AS CHARACTER,  OUTPUT TABLE ttFileName):
        DEFINE VARIABLE cFile      AS CHARACTER NO-UNDO EXTENT 3.
        DEFINE VARIABLE cExtension AS CHARACTER NO-UNDO.
        
        EMPTY TEMP-TABLE ttFileName.
        
        FILE-INFO:FILE-NAME = ".".
        
        INPUT FROM OS-DIR(FILE-INFO:FULL-PATHNAME).
        REPEAT:
          IMPORT cFile.
          
          IF cFile[3] EQ "D" OR
             NUM-ENTRIES(cFile[1],".") LT 2 THEN NEXT.
             
          cExtension = ENTRY(NUM-ENTRIES(cFile[1],"."),cFile[1],".").
          IF LOOKUP(cExtension,"jpg,jpeg,gif,png,pdf,docx") EQ 0 THEN NEXT.
    
          CREATE ttFileName.       
          ASSIGN giOrder               = giOrder + 1
                 ttFileName.tiOrder    = giOrder
                 ttFileName.tcFileName = cFile[1] 
                 ttFileName.tchref     = pcURI + "/" + REPLACE(cFile[1]," ","%20").
                 
        END.
        
        RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	@openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
	@progress.service.resourceMapping(type="REST", operation="create", URI="", alias="", mediaType="application/json").
	METHOD PUBLIC VOID postFile( INPUT TABLE ttFile ):
        DEFINE VARIABLE lpBuffer AS MEMPTR    NO-UNDO.
        DEFINE VARIABLE lcBuffer AS LONGCHAR  NO-UNDO.
        
        FIND FIRST ttFile.
        
        COPY-LOB FROM ttFile.tclbPayload TO lcBuffer.
        lpBuffer = BASE64-DECODE(lcBuffer).
        COPY-LOB FROM lpBuffer TO FILE ttFile.tcFileName.
        
        SET-SIZE(lpBuffer) = 0.
        
        EMPTY TEMP-TABLE ttFile.
        
	   RETURN.

	END METHOD.

END CLASS.
