 
 /*------------------------------------------------------------------------
    File        : JBoxXmlDoc
    Purpose     : Various functions
    Syntax      : 
    Description : 
    Author(s)   : Brynjar
    Created     : Thy Mar 3 12:43:22 CEST 2016
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

CLASS JBoxXmlDoc:
  DEF VAR cDocName           AS CHAR     NO-UNDO.
  DEF VAR hSax               AS HANDLE   NO-UNDO.
  DEF VAR hText              AS HANDLE   NO-UNDO.
  DEF VAR hFormat            AS HANDLE   NO-UNDO.
  DEF VAR cIndent            AS CHAR     NO-UNDO INIT "  ". /* "~t".  */
  DEF VAR hHelperProc        AS HANDLE   NO-UNDO.
  DEF VAR cCurrParentElement AS CHAR     NO-UNDO.
  DEF VAR cDefaultDocName    AS CHAR     NO-UNDO INIT "JBoxXmlDoc".
  DEF VAR mData              AS MEMPTR   NO-UNDO.
  DEF VAR msiFile            AS LONGCHAR NO-UNDO.
  DEF VAR mb64Data           AS MEMPTR   NO-UNDO.
  DEF VAR bUseMptr           AS LOG      NO-UNDO.
    
  DEF TEMP-TABLE ttNodes
      FIELD hXmlDoc         AS HANDLE
      FIELD hNode           AS HANDLE
      FIELD hParentNode     AS HANDLE
      FIELD iLevel          AS INT
      FIELD cElement        AS CHAR
      FIELD cViewType       AS CHAR /* block / line */
      FIELD cNodeValue      AS CHAR
      FIELD iNodeIdx        AS INT
      FIELD iParentNodeIdx  AS INT
      .
  
  DEF TEMP-TABLE ttDumpNodes
      FIELD iLevel          AS INT
      FIELD cElement        AS CHAR
      FIELD cViewType       AS CHAR /* block / line */
      FIELD cNodeValue      AS CHAR
      FIELD iNodeIdx        AS INT
      FIELD iParentNodeIdx  AS INT
      .

  /* accessible or modified from helper proc: */
  DEF PUBLIC PROPERTY iLevel          AS INT  NO-UNDO GET. SET.
  DEF PUBLIC PROPERTY iPrevLevel      AS INT  NO-UNDO GET. SET.
  DEF PUBLIC PROPERTY cActiveNode     AS CHAR NO-UNDO GET. SET.
  DEF PUBLIC PROPERTY cPrevActiveNode AS CHAR NO-UNDO GET. SET.
  DEF PUBLIC PROPERTY iNodeIdx        AS INT  NO-UNDO GET. SET.
  DEF PUBLIC PROPERTY iParentNodeIdx  AS INT  NO-UNDO GET. SET.
  DEF PUBLIC PROPERTY bEndNode        AS LOG  NO-UNDO GET. SET.

  DEF PUBLIC PROPERTY hbttNodes     AS HANDLE NO-UNDO GET. PRIVATE SET.
  DEF PUBLIC PROPERTY hXmlDoc      AS HANDLE NO-UNDO GET. PRIVATE SET.
  DEF PUBLIC PROPERTY hbttDumpNodes AS HANDLE NO-UNDO 
             GET(): 
               EMPTY TEMP-TABLE ttDumpNodes.  
               FOR EACH ttNodes:
                 CREATE ttDumpNodes.
                 BUFFER-COPY ttNodes TO ttDumpNodes.
               END.
               RETURN hbttDumpNodes.
             END GET.
             PRIVATE SET.
  
  CONSTRUCTOR PUBLIC JBoxXmlDoc (INPUT icDocName AS CHAR):
    THIS-OBJECT:InitializeComponent(icDocName).    
  END CONSTRUCTOR.

  CONSTRUCTOR PUBLIC JBoxXmlDoc ():
    THIS-OBJECT:InitializeComponent(cDefaultDocName).    
  END CONSTRUCTOR.

  METHOD PUBLIC LOGICAL InitializeComponent(INPUT icDocName AS CHAR):
    
    hbttNodes     = BUFFER ttNodes:HANDLE.
    hbttDumpNodes = BUFFER ttDumpNodes:HANDLE.
    cDocName      = icDocName.
        
    CREATE X-DOCUMENT hXmlDoc.
    CREATE X-NODEREF  hText.
    CREATE X-NODEREF  hFormat.

    RUN JBoxXmlDocHelper.p PERSIST SET hHelperProc (THIS-OBJECT).

  END METHOD.

  METHOD PUBLIC LOGICAL AddNode( INPUT icParentElement AS CHAR
                                ,INPUT icViewType      AS CHAR
                                ,INPUT icElement       AS CHAR
                                ,INPUT icValue         AS CHAR).

    DEF VAR iLevel          AS INT    NO-UNDO.
    DEF VAR hParentNode     AS HANDLE NO-UNDO.
    DEF VAR iParentNodeIdx  AS INT    NO-UNDO.
    DEF VAR rbNode          AS ROWID  NO-UNDO.
    
    IF icValue = ? THEN icValue = "".
    
    DEF BUFFER bttNodes FOR ttNodes.
    
    FOR EACH ttNodes
        WHERE ttNodes.cElement = icParentElement
        BY ttNodes.iNodeIdx DESC:
      rbNode = ROWID(ttNodes).
      LEAVE.
    END.
    
    IF rbNode NE ? THEN DO:
      FIND FIRST bttNodes 
           WHERE ROWID(bttNodes) = rbNode.
      ASSIGN iLevel = ttNodes.iLevel + 1
             hParentNode = bttNodes.hNode
             iParentNodeIdx = bttNodes.iNodeIdx
             .
    END.
    ELSE IF CAN-FIND(FIRST ttNodes) OR icViewType NE "block" THEN 
      RETURN NO.
    
    CREATE ttNodes.
    ASSIGN ttNodes.hXmlDoc        = hXmlDoc
           ttNodes.hParentNode    = hParentNode
           ttNodes.iLevel         = iLevel
           ttNodes.cViewType      = IF icViewType NE "" THEN icViewType ELSE "line"
           ttNodes.cNodeValue     = icValue
           ttNodes.cElement       = icElement
           iNodeIdx               = iNodeIdx + 1
           ttNodes.iNodeIdx       = iNodeIdx
           ttNodes.iParentNodeIdx = iParentNodeIdx
           .
    CREATE X-NODEREF ttNodes.hNode.
    
    IF icViewType = "block" THEN DO:
      IF iLevel > 0 THEN DO: /* Create child block node */
        hXmlDoc:CREATE-NODE(ttNodes.hNode,icElement,"ELEMENT").
    
        hXmlDoc:CREATE-NODE(hFormat, ?, "TEXT").
        hFormat:NODE-VALUE = cIndent.
        hParentNode:APPEND-CHILD(hFormat).
    
        hParentNode:APPEND-CHILD(ttNodes.hNode).
        hXmlDoc:CREATE-NODE(hFormat, ?, "TEXT").
        hFormat:NODE-VALUE = "~n" + FILL(cIndent,iLevel).
        ttNodes.hNode:APPEND-CHILD(hFormat).
    
        hXmlDoc:CREATE-NODE(hFormat, ?, "TEXT").
        hFormat:NODE-VALUE = "~n" + FILL(cIndent,iLevel - 1). 
        hParentNode:APPEND-CHILD(hFormat).
      END.
      ELSE DO: /* Create the root block node */        
        hXmlDoc:CREATE-NODE(ttNodes.hNode, icElement, "ELEMENT").
        hXmlDoc:APPEND-CHILD(ttNodes.hNode).
        hXmlDoc:CREATE-NODE(hFormat, ?, "TEXT").
        hFormat:NODE-VALUE = "~n".    /* new line */
        ttNodes.hNode:APPEND-CHILD(hFormat).
        iLevel = 1.
      END.
      cCurrParentElement = icElement.
    END.
    ELSE DO:
      hXmlDoc:CREATE-NODE(hFormat, ?, "TEXT").
      hFormat:NODE-VALUE = cIndent.
      hParentNode:APPEND-CHILD(hFormat).
    
      hXmlDoc:CREATE-NODE(ttNodes.hNode, icElement, "ELEMENT").
      hXmlDoc:CREATE-NODE(hText, ?, "TEXT").
      IF bUseMptr THEN DO:
        hText:MEMPTR-TO-NODE-VALUE(mb64Data).
        bUseMptr = NO.
      END.
      ELSE hText:NODE-VALUE = icValue.
      ttNodes.hNode:APPEND-CHILD(hText).
      hParentNode:APPEND-CHILD(ttNodes.hNode).
      /* since the parent is "block"  type append a new line to close it neatly  */                      
      hXmlDoc:CREATE-NODE(hFormat, ?, "TEXT").
      hFormat:NODE-VALUE = "~n" + FILL(cIndent,iLevel - 1).
      hParentNode:APPEND-CHILD(hFormat).
    END.

    RETURN YES.
  END METHOD.

  METHOD PUBLIC LOGICAL AddNode( INPUT icParentElement AS CHAR
                                ,INPUT icElement       AS CHAR
                                ,INPUT icValue         AS CHAR).
                                
    RETURN THIS-OBJECT:AddNode(icParentElement,"",icElement,icValue).                            
  END.                              
  METHOD PUBLIC LOGICAL AddNode( INPUT icElement       AS CHAR
                                ,INPUT icValue         AS CHAR).
                                
    RETURN THIS-OBJECT:AddNode(cCurrParentElement,"",icElement,icValue).                            
  END.                              

  METHOD PUBLIC LOGICAL AddNode( INPUT icParentElement AS CHAR
                                ,INPUT icElement       AS CHAR
                                ,INPUT imMptr          AS MEMPTR).

    SET-SIZE(mb64data) = 0.
    ASSIGN mb64Data = imMptr
           bUseMptr = YES.
                                
    RETURN THIS-OBJECT:AddNode(icParentElement,"",icElement,"").                            
  END.                              
  METHOD PUBLIC LOGICAL AddNode( INPUT icElement       AS CHAR
                                ,INPUT imMptr          AS MEMPTR).
                                
    SET-SIZE(mb64data) = 0.
    ASSIGN mb64Data = imMptr
           bUseMptr = YES.
           
    RETURN THIS-OBJECT:AddNode(cCurrParentElement,"",icElement,"").                            
  END.                              

  METHOD PUBLIC LOGICAL AddBlockNode( INPUT icParentElement AS CHAR
                                     ,INPUT icElement       AS CHAR).
                                
    RETURN THIS-OBJECT:AddNode(icParentElement,"block",icElement,"").                            
  END.                              
  METHOD PUBLIC LOGICAL AddBlockNode( INPUT icElement       AS CHAR).
                                
    RETURN THIS-OBJECT:AddNode(cCurrParentElement,"block",icElement,"").                            
  END.                   

  METHOD PUBLIC LOGICAL AddFileNode( INPUT icParentElement AS CHAR
                                    ,INPUT icElement       AS CHAR
                                    ,INPUT icFileName      AS CHAR).

    IF SEARCH(icFileName) NE ? THEN DO:
      SET-SIZE(mData) = 0.
      SET-SIZE(mb64data) = 0.
      COPY-LOB FROM FILE icFilename TO mData.
    
      msiFile = BASE64-ENCODE(mData).
      COPY-LOB FROM msiFile TO mb64Data.
    
      bUseMptr = YES.
                                  
      RETURN THIS-OBJECT:AddNode(icParentElement,"",icElement,"").
    END.
    ELSE RETURN NO.                              
  END.                              
  METHOD PUBLIC LOGICAL AddFileNode( INPUT icElement       AS CHAR
                                    ,INPUT icFileName      AS CHAR).
                                           
    RETURN THIS-OBJECT:AddFileNode(cCurrParentElement,icElement,icFileName).                            
  END.                              
                 
  METHOD PUBLIC LOGICAL DumpXmlNodes(INPUT icFileName AS CHAR):
        
    IF SESSION:CLIENT-TYPE = "APPSERVER" THEN DO:
      EMPTY TEMP-TABLE ttDumpNodes.  
      FOR EACH ttNodes:
        CREATE ttDumpNodes.
        BUFFER-COPY ttNodes TO ttDumpNodes.
      END.
      OUTPUT TO VALUE (icFileName).
      FOR EACH ttDumpNodes:
        EXPORT DELIMITER ";" ttDumpNodes.
      END.
      OUTPUT CLOSE.
    END.
    ELSE RUN toExcelViaFile.p (hbttDumpNodes,0).

  END METHOD.

  METHOD PUBLIC LOGICAL DumpXmlNodes():
    THIS-OBJECT:DumpXmlNodes(cDocName).
  END METHOD.

	METHOD PUBLIC VOID LoadXml(INPUT ilcDoc     AS LONGCHAR
	                          ,INPUT ibValidate AS LOG ).

    CREATE SAX-READER hSax NO-ERROR.
        
    hSax:HANDLER = hHelperProc NO-ERROR.
    hSax:SET-INPUT-SOURCE("longchar",ilcDoc) NO-ERROR.
    hSax:SAX-PARSE() NO-ERROR.

    hXmlDoc:LOAD("longchar",ilcDoc,ibValidate).
    FOR EACH ttNodes WHERE ttNodes.cViewType = "":
      ttNodes.cViewType = "block".
    END.
	END METHOD.

  METHOD PUBLIC VOID LoadXml(INPUT ilcDoc     AS LONGCHAR).
    THIS-OBJECT:LoadXml(ilcDoc,NO).
  END METHOD.

  METHOD PUBLIC VOID LoadXmlFile(INPUT icFile AS CHAR).
    DEF VAR lcFile AS LONGCHAR NO-UNDO.
    COPY-LOB FROM FILE icFile TO OBJECT lcFile.
    THIS-OBJECT:LoadXml(lcFile,NO).
  END METHOD.

  METHOD PUBLIC VOID LoadXmlFileFixLF(INPUT icFile AS CHAR).
    DEF VAR lcFile AS LONGCHAR NO-UNDO.
    COPY-LOB FROM FILE icFile TO OBJECT lcFile.
    lcFile = REPLACE(lcFile,">" + CHR(10) + "<!","><!").
    lcFile = REPLACE(lcFile,"]]>" + CHR(10) + "</","]]></").
    lcFile = REPLACE(lcFile,">" + CHR(13) + CHR(10) + "<!","><!").
    lcFile = REPLACE(lcFile,"]]>" + CHR(13) + CHR(10) + "</","]]></").
    THIS-OBJECT:LoadXml(lcFile,NO).
  END METHOD.
                               
  METHOD PUBLIC VOID createNode (charData AS LONGCHAR).
    /* Invoked from helper proc (sax event proc) */
    
    IF NOT bEndNode THEN DO:
      CREATE ttNodes.
      ASSIGN ttNodes.hXmlDoc        = hXmlDoc
             ttNodes.cElement       = cActiveNode
             ttNodes.cNodeValue     = IF LENGTH(charData) < 32000 THEN charData ELSE ""
             iNodeIdx               = iNodeIdx + 1
             ttNodes.iNodeIdx       = iNodeIdx                  
             ttNodes.iLevel         = iLevel
             ttNodes.iParentNodeIdx = iParentNodeIdx
             .
    END.   
    ELSE IF AVAIL ttNodes AND ttNodes.cElement = cActiveNode THEN  
      ttNodes.cViewType = "line".
           
  END METHOD.
  
  METHOD PUBLIC INTEGER getParentNodeForLevel (iiLevel AS INT):
    DEF VAR iParentNode AS INT NO-UNDO.
    FOR EACH ttNodes
        WHERE ttNodes.iLevel = iiLevel - 1
        BY ttNodes.iNodeIdx DESC:
      iParentNode = ttNodes.iNodeIdx.
      LEAVE.
    END.
    RETURN iParentNode.
  END.  

  METHOD PUBLIC CHAR getNodeValue (INPUT icElement AS CHAR).
  
    FIND FIRST ttNodes 
         WHERE ttNodes.cElement = icElement
         NO-ERROR.
   
    RETURN IF AVAIL ttNodes THEN ttNodes.cNodeValue ELSE ?.
  END METHOD.

  METHOD PUBLIC VOID extractNodes (INPUT ihBuffer      AS HANDLE
                                  ,INPUT icCheckFields AS CHAR). /* <- Do nodes exist for these fields? If not assign to ? */
  
    DEF VAR hColumn       AS HANDLE NO-UNDO EXTENT 20.
    DEF VAR ix            AS INT    NO-UNDO.
    DEF VAR iy            AS INT    NO-UNDO.
    DEF VAR cFieldList    AS CHAR   NO-UNDO.
    DEF VAR cFirstElement AS CHAR   NO-UNDO.
    
    IF ihBuffer:TYPE = "TEMP-TABLE" THEN ihBuffer = ihBuffer:DEFAULT-BUFFER-HANDLE.
    
    DO ix = 1 TO ihBuffer:NUM-FIELDS:
      IF THIS-OBJECT:getNodeValue(ihBuffer:BUFFER-FIELD(ix):NAME) NE ? THEN
        ASSIGN iy = iy + 1  
               hColumn[iy] = ihBuffer:BUFFER-FIELD(ix)
               cFieldList  = cFieldList + (IF cFieldList NE "" THEN "," ELSE "") + ihBuffer:BUFFER-FIELD(ix):NAME.
    END.
    FOR EACH ttNodes
        WHERE CAN-DO(cFieldList,ttNodes.cElement)
        BY ttNodes.iNodeIdx:            
      IF cFirstElement = "" THEN cFirstElement = ttNodes.cElement.
      
      IF ttNodes.cElement = cFirstElement THEN DO:
        ihBuffer:BUFFER-CREATE().
        DO ix = 1 TO NUM-ENTRIES(icCheckFields):
          ihBuffer:BUFFER-FIELD(ENTRY(ix,icCheckFields)):BUFFER-VALUE = ?.
        END.  
      END.  
            
      THIS-OBJECT:assignStringValue(ihBuffer:BUFFER-FIELD(ttNodes.cElement),0,ttNodes.cNodeValue).
    END.
    
  END METHOD.

  METHOD PUBLIC LOGICAL setEncoding (INPUT icEncoding AS CHAR).
  
    hXmlDoc:ENCODING = icEncoding NO-ERROR.
   
    RETURN ERROR-STATUS:ERROR.
  END METHOD.
        
  METHOD PUBLIC LONGCHAR getDocument ().
    DEF VAR lcSaveTo AS LONGCHAR NO-UNDO.
    
    hXmlDoc:SAVE("longchar",lcSaveTo).   
    RETURN lcSaveTo.
  END METHOD.

  METHOD PUBLIC VOID saveToFile (INPUT icFileName AS CHAR
                                ,INPUT ibOpen     AS LOG).
    
    IF ibOpen THEN  
      JBoxFunctions:Instance:TextToFile(THIS-OBJECT:getDocument(),icFileName,ibOpen).
    ELSE      
      hXmlDoc:SAVE("file",icFileName).
 
  END METHOD.
          
  METHOD PUBLIC VOID saveToFile (INPUT icFileName AS CHAR).
    hXmlDoc:SAVE("file",icFileName). 
  END METHOD.
        
  METHOD PRIVATE LOGICAL assignStringValue (INPUT ihField        AS HANDLE
                                           ,INPUT iiExtent       AS INT
                                           ,INPUT icUpdateValue  AS CHAR).
                                           
    CASE ihField:DATA-TYPE:
      WHEN "character" THEN
        ihField:BUFFER-VALUE[iiExtent] = TRIM(icUpdateValue) NO-ERROR.
      WHEN "date" THEN 
        ihField:BUFFER-VALUE[iiExtent] = DATE(icUpdateValue) NO-ERROR.
      WHEN "decimal" THEN
        ihField:BUFFER-VALUE[iiExtent] = DEC(icUpdateValue) NO-ERROR.
      WHEN "integer" THEN
        ihField:BUFFER-VALUE[iiExtent] = INT(icUpdateValue) NO-ERROR.
      WHEN "int64" THEN
        ihField:BUFFER-VALUE[iiExtent] = INT64(icUpdateValue) NO-ERROR.
      WHEN "logical" THEN 
        ihField:BUFFER-VALUE[iiExtent] = (IF icUpdateValue = "yes" OR icUpdateValue = "true" THEN TRUE 
                                          ELSE IF icUpdateValue = "?" THEN ?
                                          ELSE FALSE)  NO-ERROR.
      WHEN "datetime" THEN
        IF iiExtent > 0 THEN
          ihField:BUFFER-VALUE[iiExtent] = DATETIME(icUpdateValue) NO-ERROR.
        ELSE 
          ihField:BUFFER-VALUE = DATETIME(icUpdateValue) NO-ERROR.
      WHEN "datetime-tz" THEN
        IF iiExtent > 0 THEN
          ihField:BUFFER-VALUE[iiExtent] = DATETIME-TZ(icUpdateValue) NO-ERROR.
        ELSE
          ihField:BUFFER-VALUE = DATETIME-TZ(icUpdateValue) NO-ERROR.
    END CASE.

    RETURN ERROR-STATUS:ERROR.
  END METHOD.     
        
  DESTRUCTOR PUBLIC JBoxXmlDoc ( ):
    DELETE PROCEDURE hHelperProc NO-ERROR.    
  END DESTRUCTOR.
END CLASS.
  
