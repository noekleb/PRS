 
 /*------------------------------------------------------------------------
    File        : JBoxQuery
    Purpose     : 
    Syntax      : 
    Description : OO Wrapper for library function NewQuery
    Author(s)   : brynjar
    Created     : Tue Nov 05 10:47:14 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
/*USING oo.* FROM PROPATH.*/

CLASS JBoxQuery: 
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	DEF VAR hSourceProc     AS HANDLE NO-UNDO.
	DEF VAR bOk             AS LOG    NO-UNDO.
	DEF VAR bTryFilter      AS LOG    NO-UNDO. /* Clear the filter if no records were found */
	DEF VAR bNoExecFilter   AS LOG    NO-UNDO.
	DEF VAR cLastFilter     AS CHAR   NO-UNDO.
	DEF VAR cInitAttr       AS CHAR   NO-UNDO.
	DEF VAR cSortExpr       AS CHAR   NO-UNDO.
	DEF VAR cQueryJoin      AS CHAR   NO-UNDO.
	DEF VAR hTableHandle    AS HANDLE NO-UNDO.
	DEF VAR oToolbar        AS JBoxToolbar   NO-UNDO.
	DEF VAR cCallFromMethod AS CHAR   NO-UNDO.

  DEF PUBLIC PROPERTY QUERY-HANDLE         AS HANDLE GET. PROTECTED SET.
  DEF PUBLIC PROPERTY BUFFER-HANDLE        AS HANDLE GET. PROTECTED SET. 
  DEF PUBLIC PROPERTY TABLE-HANDLE         AS HANDLE GET. PROTECTED SET.
  DEF PUBLIC PROPERTY PARENT-BROWSE-OBJECT AS JBoxBrowse GET. PROTECTED SET.
  DEF PUBLIC PROPERTY PARENT-QUERY-OBJECT  AS JBoxQuery GET. PROTECTED SET. 
  DEF PUBLIC PROPERTY PARENT-BUFFER-HANDLE AS HANDLE 
      GET():
         DEF VAR hParent AS HANDLE NO-UNDO.
         hParent = DYNAMIC-FUNCTION("getLinkedObject",QUERY-HANDLE,"parent","from").
         IF VALID-HANDLE(hParent) THEN DO:
           IF hParent:TYPE = "browse" THEN RETURN hParent:QUERY:GET-BUFFER-HANDLE(1).
           ELSE RETURN hParent:GET-BUFFER-HANDLE(1).
         END.  
         ELSE RETURN ?.  
      END GET.
      PROTECTED SET.
                      
  {JBoxBrowseQueryProp.i QUERY-HANDLE}
                                            
  DEF PUBLIC PROPERTY rowsToBatch      AS INT   INIT 100 
      GET(): RETURN INT(DYNAMIC-FUNCTION("getAttribute",QUERY-HANDLE,"rowsToBatch")). 
      END GET. 
      SET(iiRowsToBatch AS INT):
        DYNAMIC-FUNCTION("setAttribute",QUERY-HANDLE,"rowsToBatch",STRING(iiRowsToBatch)).
      END SET.
   
  DEF PUBLIC PROPERTY queryJoin AS CHAR
      GET(): RETURN DYNAMIC-FUNCTION("getAttribute",QUERY-HANDLE,"queryJoin"). END GET.
      SET(icQueryJoin AS CHAR):
        DEF VAR ix          AS INT NO-UNDO.
        DEF VAR cBufferJoin AS CHAR NO-UNDO.
        DEF VAR cBuffer     AS CHAR NO-UNDO.
        icQueryJoin = TRIM(icQueryJoin).
        IF NOT icQueryJoin BEGINS "," THEN icQueryJoin = "," + icQueryJoin.
        DO ix = 2 TO NUM-ENTRIES(icQueryJoin):
          cBufferJoin = TRIM(ENTRY(ix,icQueryJoin)).
          IF SUBSTR(cBufferJoin,1,6) = "first " OR
             SUBSTR(cBufferJoin,1,5) = "last " OR
             SUBSTR(cBufferJoin,1,5) = "each " THEN
            cBuffer = ENTRY(1,TRIM(SUBSTR(cBufferJoin,6))," ").
          ELSE
            cBuffer = ENTRY(1,cBufferJoin," ").
          DYNAMIC-FUNCTION("setAttribute",QUERY-HANDLE,"bufferJoin" + cBuffer,"," + cBufferJoin).
        END.
        DYNAMIC-FUNCTION("setAttribute",QUERY-HANDLE,"queryJoin",icQueryJoin).
        IF QUERY-HANDLE:TYPE NE "Query" THEN cQueryJoin = icQueryJoin.
      END SET.
        
  DEF PUBLIC PROPERTY buffersAndFields AS CHAR
      GET(): RETURN DYNAMIC-FUNCTION("getAttribute",QUERY-HANDLE,"buffersAndFields"). END GET.
      SET(icBuffersAndFields AS CHAR):
        DEF VAR ix           AS INT    NO-UNDO.
        DEF VAR cBufferList  AS CHAR   NO-UNDO.
        DEF VAR cTtField     AS CHAR   NO-UNDO.
        DEF VAR cTtFieldList AS CHAR   NO-UNDO.
        DEF VAR hColumn      AS HANDLE NO-UNDO.
        IF NOT (icBuffersAndFields NE "" AND DYNAMIC-FUNCTION("getAttribute",QUERY-HANDLE,"tempTableHandle") NE "") THEN DO:
          DO ix = 2 TO NUM-ENTRIES(icBuffersAndFields):
            cBufferList = cBufferList + "," + ENTRY(1,ENTRY(ix,icBuffersAndFields),";").
          END.  
          IF VALID-HANDLE(QUERY-HANDLE) THEN DYNAMIC-FUNCTION("deleteObject",QUERY-HANDLE).
          DYNAMIC-FUNCTION("setObjectSourceProc",hSourceProc).  
          QUERY-HANDLE = DYNAMIC-FUNCTION("NewQuery",rowsToBatch,"",icBuffersAndFields,"where false" + cBufferList
                                         ,"TEMP-TABLE|" + STRING(hTableHandle)).
          BUFFER-HANDLE = QUERY-HANDLE:GET-BUFFER-HANDLE(1).
          DYNAMIC-FUNCTION("CreateObjectLink",QUERY-HANDLE,hSourceProc).
          DYNAMIC-FUNCTION("setAttribute",QUERY-HANDLE,"useLocalData","").
          cBufferList = DYNAMIC-FUNCTION("getAttribute",QUERY-HANDLE,"bufferList").
        END.
        ELSE DO:
          DO ix = 1 TO NUM-ENTRIES(icBuffersAndFields,";"):
            cTtField = ENTRY(ix,icBuffersAndFields,";").
            IF NUM-ENTRIES(cTtField) > 1 THEN
              cTtField = ENTRY(2,cTtField).
            cTtFieldList = cTtFieldList + (IF cTtFieldList NE "" THEN "," ELSE "") + ENTRY(1,cTtField,"|").
          END.    
        END.
        IF cSortExpr NE "" THEN
          DYNAMIC-FUNCTION("setSortString",QUERY-HANDLE,cSortExpr).     
        IF cQueryJoin NE "" THEN
          queryJoin = cQueryJoin.  
      END SET.
             
 	
  CONSTRUCTOR PUBLIC JBoxQuery():
      SUPER ().

    IF PROVERSION < "11" THEN hSourceProc = DYNAMIC-FUNCTION ("getProcedureHandle",PROGRAM-NAME(2)).
    ELSE hSourceProc = SOURCE-PROCEDURE.     
    
    InitializeComponent("",?).     
      
  END CONSTRUCTOR.
  
  CONSTRUCTOR PUBLIC JBoxQuery (icQueryTableName AS CHAR):
      SUPER ().

    IF PROVERSION < "11" THEN hSourceProc = DYNAMIC-FUNCTION ("getProcedureHandle",PROGRAM-NAME(2)).
    ELSE hSourceProc = SOURCE-PROCEDURE.     
    
    InitializeComponent(icQueryTableName,?).     
      
  END CONSTRUCTOR.

  CONSTRUCTOR PUBLIC JBoxQuery (iTABLE-HANDLE AS HANDLE):
      SUPER ().

    IF PROVERSION < "11" THEN hSourceProc = DYNAMIC-FUNCTION ("getProcedureHandle",PROGRAM-NAME(2)).
    ELSE hSourceProc = SOURCE-PROCEDURE.
                         
    InitializeComponent("",iTABLE-HANDLE).                 
      
  END CONSTRUCTOR.

  METHOD PRIVATE VOID initializeComponent(icQueryTableName   AS CHAR 
                                         ,iTABLE-HANDLE AS HANDLE):
    DEF VAR ix                AS INT  NO-UNDO.
    DEF VAR cBuffsAndFlds     AS CHAR NO-UNDO.
    DEF VAR cDefBuffsAndFlds  AS CHAR NO-UNDO.
    DEF VAR cBufferList       AS CHAR NO-UNDO.
    DEF VAR cCalcFlds         AS CHAR NO-UNDO.
    DEF VAR iy                AS INT  NO-UNDO.
    DEF VAR cDbFields         AS CHAR NO-UNDO.
    DEF VAR cBufferDef        AS CHAR NO-UNDO.
    DEF VAR cFieldDef         AS CHAR NO-UNDO.

    DYNAMIC-FUNCTION("setObjectSourceProc",hSourceProc).
    DYNAMIC-FUNCTION("NewObject",
                     IF VALID-HANDLE(hSourceProc:CURRENT-WINDOW) THEN hSourceProc:CURRENT-WINDOW ELSE ?,
                     hSourceProc,"procedure").

    IF icQueryTableName NE "" AND CAN-DO(hSourceProc:INTERNAL-ENTRIES,"getBuffersAndFieldsQry" + icQueryTableName) THEN DO: 
      cDefBuffsAndFlds = DYNAMIC-FUNCTION("getBuffersAndFieldsQry" + icQueryTableName IN hSourceProc).
      DO ix = 1 TO NUM-ENTRIES(cDefBuffsAndFlds):
        ASSIGN cDbFields = ""
               cCalcFlds = ""
               cBufferDef = ENTRY(ix,cDefBuffsAndFlds).
               
        IF NUM-ENTRIES(cBufferDef,";") > 1 THEN DO iy = 2 TO NUM-ENTRIES(cBufferDef,";"):
          cFieldDef = ENTRY(iy,cBufferDef,";").
          IF cFieldDef BEGINS "+" OR cFieldDef BEGINS "!+" THEN 
            cCalcFlds = cCalcFlds + (IF cCalcFlds NE "" THEN ";" ELSE "") + cFieldDef.
          ELSE
            cDbFields = cDbFields + (IF cDbFields NE "" THEN ";" ELSE "") + cFieldDef.            
        END.  
        cBuffsAndFlds = cBuffsAndFlds + (IF cBuffsAndFlds NE "" THEN "," ELSE "") + ENTRY(1,cBufferDef,";")
                      + ";" + (IF cDbFields = "" THEN DYNAMIC-FUNCTION("getFieldsForTable",ENTRY(1,cBufferDef,";")) ELSE cDbFields) 
                      + (IF cCalcFlds NE "" THEN ";" + cCalcFlds ELSE "").
      END.  
      cQueryJoin    = TRIM(DYNAMIC-FUNCTION("getQueryJoinQry" + icQueryTableName IN hSourceProc),",").
      hTableHandle  = DYNAMIC-FUNCTION("getTableHandleQry" + icQueryTableName IN hSourceProc).
      DYNAMIC-FUNCTION("setAttribute",hTableHandle,"queryJoin","," + cQueryJoin).
    END.
    ELSE IF NOT VALID-HANDLE(iTABLE-HANDLE) THEN cBuffsAndFlds = icQueryTableName.  
    
    IF DYNAMIC-FUNCTION("getAttribute",SESSION,"rowsToBatch") NE "" THEN
      rowsToBatch = INT(DYNAMIC-FUNCTION("getAttribute",SESSION,"rowsToBatch")).  
    
    IF VALID-HANDLE(iTABLE-HANDLE) THEN DO:
      IF iTABLE-HANDLE:TYPE = "buffer" THEN DO:
        DYNAMIC-FUNCTION("setAttribute",iTABLE-HANDLE,"tempTableHandle",STRING(iTABLE-HANDLE:TABLE-HANDLE)).
        hTableHandle = iTABLE-HANDLE:TABLE-HANDLE.
      END.  
      ELSE DO:
        DYNAMIC-FUNCTION("setAttribute",iTABLE-HANDLE,"tempTableHandle",STRING(iTABLE-HANDLE)).
        hTableHandle = iTABLE-HANDLE.
      END.  
      buffersAndFields = "". /* Creates the Query - no need to set buffers and fields from container */ 
    END.  
    ELSE IF cBuffsAndFlds NE "" THEN  
      buffersAndFields = cBuffsAndFlds. /* Creates the query */
                                          
    PUBLISH "setChildQueryObject" FROM hSourceProc (hSourceProc,THIS-OBJECT).                              
    
  END METHOD.                                       

  METHOD PRIVATE CHARACTER getFilterOper(INPUT icOper AS CHAR):
    
    DEFINE VARIABLE ocOper AS CHARACTER NO-UNDO.
  
    CASE icOper:
      WHEN "EQ" THEN ocOper = "=".
      WHEN "NE" THEN ocOper = "<>".
      WHEN "GE" THEN ocOper = ">=".
      WHEN "LE" THEN ocOper = "<=".
      WHEN "GT" THEN ocOper = ">".
      WHEN "LT" THEN ocOper = "<".
      OTHERWISE ocOper = icOper.
    END CASE.  

    RETURN ocOper.

  END METHOD.

  METHOD PUBLIC LOGICAL applyFilter(icExprList AS CHAR):
    DEF VAR cFields    AS CHAR NO-UNDO.
    DEF VAR cOperators AS CHAR NO-UNDO.
    DEF VAR cValues    AS CHAR NO-UNDO.

    DEF VAR hToolbar AS HANDLE NO-UNDO.
    hToolbar = DYNAMIC-FUNCTION("getLinkedObject",QUERY-HANDLE,"toolbar","from").

    bOK = NO.                                  
    IF resolveFilterExpr(IF icExprList = "" THEN cLastFilter ELSE icExprList,OUTPUT cFields,OUTPUT cOperators,OUTPUT cValues) THEN DO:                                  
      IF QUERY-HANDLE:TYPE = "Query" THEN DO:                                  
        bOK = DYNAMIC-FUNCTION("InitDynFilter",QUERY-HANDLE,cFields,cOperators,cValues,
                                (IF hToolbar = ? OR DYNAMIC-FUNCTION("getAttribute",hToolbar,"buttonFilter") = "" THEN "static" ELSE "") +
                                (IF bNoExecFilter THEN "noexec" ELSE IF bTryFilter THEN "try" ELSE "")).
        DYNAMIC-FUNCTION("setCurrentObject",QUERY-HANDLE).
      END.                          
      ELSE
        MESSAGE "Filter cannot be applied before object is initialized, ie buffers and fields are defined (buffersAndFields property)"
        VIEW-AS ALERT-BOX.
    END.
    ELSE
      MESSAGE "Error in filter definition: " + icExprList.
      
    ASSIGN bTryFilter = NO
           bNoExecFilter = NO
           cLastFilter = "".

    RETURN bOk.      
  END METHOD.
  
  METHOD PUBLIC LOGICAL applyFilter(icExprList AS CHAR,ibTry AS LOG):
    bTryfilter = ibTry.
    RETURN applyFilter(IF icExprList = "" THEN cLastFilter ELSE icExprList).
  END METHOD.
  
  METHOD PUBLIC LOGICAL applyFilter(icFieldList AS CHAR
                                   ,icOperList  AS CHAR
                                   ,icValueList AS CHAR):
    DEF VAR hToolbar AS HANDLE NO-UNDO.
    hToolbar = DYNAMIC-FUNCTION("getLinkedObject",QUERY-HANDLE,"toolbar","from").
    
    bOK = NO.                                  
    IF QUERY-HANDLE:TYPE = "Query" THEN DO:                                  
      bOK = DYNAMIC-FUNCTION("InitDynFilter",QUERY-HANDLE,icFieldList,icOperList,icValueList,
                             (IF hToolbar = ? OR DYNAMIC-FUNCTION("getAttribute",hToolbar,"buttonFilter") = "" THEN "static" ELSE "") +
                             (IF bNoExecFilter THEN "noexec" ELSE IF bTryFilter THEN "try" ELSE "")).
      DYNAMIC-FUNCTION("setCurrentObject",QUERY-HANDLE).
    END.                         
    ELSE
      MESSAGE "Filter cannot be applied before object is initialized, ie buffers and fields are defined (buffersAndFields property)"
      VIEW-AS ALERT-BOX.
      
    ASSIGN bTryFilter = NO
           bNoExecFilter = NO
           cLastFilter = "".

    RETURN bOk.      
  END METHOD.

  METHOD PUBLIC VOID setFilter(icExprList AS CHAR):
    ASSIGN bNoExecFilter = YES
           cLastFilter    = icExprList.
    applyFilter(icExprList).
  END METHOD.
    
  METHOD PUBLIC VOID setFilter(icFieldList AS CHAR
                              ,icOperList  AS CHAR
                              ,icValueList AS CHAR):
    DEF VAR hToolbar AS HANDLE NO-UNDO.
    hToolbar = DYNAMIC-FUNCTION("getLinkedObject",QUERY-HANDLE,"toolbar","from").
                                       
    bOK = DYNAMIC-FUNCTION("InitDynFilter",QUERY-HANDLE,icFieldList,icOperList,icValueList,
                           IF hToolbar = ? OR DYNAMIC-FUNCTION("getAttribute",hToolbar,"buttonFilter") = "" THEN "static,noexec" ELSE "noexec").
    
    DYNAMIC-FUNCTION("setCurrentObject",QUERY-HANDLE).
                           
  END METHOD.  

  METHOD PUBLIC VOID setQuerySort(icSortExpr AS CHAR): /* "field1;[desc],field2.." */
    icSortExpr = REPLACE(icSortExpr,"descending","desc").
    IF VALID-HANDLE(QUERY-HANDLE) AND QUERY-HANDLE:TYPE = "Query" THEN
      DYNAMIC-FUNCTION("setSortString",QUERY-HANDLE,icSortExpr).
    ELSE cSortExpr = icSortExpr.
  END METHOD.

  METHOD PUBLIC VOID addCalcField(icSourceBuffer AS CHAR
                                 ,icDataType     AS CHAR
                                 ,icFormat       AS CHAR
                                 ,icLabel        AS CHAR
                                 ,icProcedure    AS CHAR
                                 ,icParameter    AS CHAR):
                                       
  END METHOD.
  
  METHOD PUBLIC VOID setCalcFieldParam(icFieldName AS CHAR
                                      ,icParameter AS CHAR):
    DYNAMIC-FUNCTION("setAttribute",QUERY-HANDLE,"calcParam" + icFieldName,icParameter).                                         
  END METHOD.                                        
           	
  METHOD PUBLIC VOID openQuery():
    RUN InvokeMethod(QUERY-HANDLE,"OpenQuery").
  END METHOD.

  METHOD PUBLIC VOID openQuery(INPUT icQueryString AS CHAR):
    icQueryString = TRIM(icQueryString).
    IF (DYNAMIC-FUNCTION("getAttribute",QUERY-HANDLE,"baseQuery") NE "" OR
        DYNAMIC-FUNCTION("getAttribute",QUERY-HANDLE,"queryFilter") NE "") AND 
       icQueryString BEGINS "WHERE " THEN DO:
      icQueryString = " AND (" + SUBSTR(icQueryString,6) + ")".      
    END.
    DYNAMIC-FUNCTION("setAttribute",QUERY-HANDLE,"queryWhere",icQueryString).
    RUN InvokeMethod(QUERY-HANDLE,"OpenQuery").
  END METHOD.

  /*----------------------------------------------------------------------------
    Purpose: Create a parent relationship for the browse 
    Notes:   If field-names are different in parent object the entry in relationship
             list must be split by ; stating the child field-name first, eg
             custnum;customernum 
    @param ioParentBrowse Parent browse object
    @param icRelationShip Comma-sep. list of fields to define the relationship or "parentlink" to indicate that the query is custom            
  ------------------------------------------------------------------------------*/    
  METHOD PUBLIC VOID setParentBrowseObject(ioParentBrowse AS JBoxBrowse
                                          ,icRelationship AS CHAR):
    IF NOT VALID-OBJECT(ioParentBrowse) THEN DO:
      IF VALID-OBJECT(PARENT-BROWSE-OBJECT) THEN
        DYNAMIC-FUNCTION ("deleteObjectLink",QUERY-HANDLE,PARENT-BROWSE-OBJECT:BROWSE-HANDLE).
    END.
    ELSE DO:
      DYNAMIC-FUNCTION ("deleteObjectLink",QUERY-HANDLE,ioParentBrowse:BROWSE-HANDLE).
      DYNAMIC-FUNCTION("createParentLink",THIS-OBJECT:QUERY-HANDLE,ioParentBrowse:BROWSE-HANDLE,icRelationShip).
    END.  

    PARENT-BROWSE-OBJECT = ioParentBrowse.
                                                                                        
  END METHOD.  

  METHOD PUBLIC VOID setParentQueryObject(ioParentQuery  AS JBoxQuery
                                         ,icRelationship AS CHAR):
    IF NOT VALID-OBJECT(ioParentQuery) THEN DO:
      IF VALID-OBJECT(PARENT-QUERY-OBJECT) THEN
        DYNAMIC-FUNCTION ("deleteObjectLink",QUERY-HANDLE,PARENT-QUERY-OBJECT:QUERY-HANDLE).
    END.
    ELSE DO: 
      DYNAMIC-FUNCTION ("deleteObjectLink",QUERY-HANDLE,ioParentQuery:QUERY-HANDLE).
      DYNAMIC-FUNCTION("createParentLink",THIS-OBJECT:QUERY-HANDLE,ioParentQuery:QUERY-HANDLE,icRelationShip).
    END.  

    PARENT-QUERY-OBJECT = ioParentQuery.
  END METHOD.
    
  METHOD PUBLIC VOID setOneToOneBrowseObject(ioParentBrowse AS JBoxBrowse
                                            ,icRelationship AS CHAR):
    IF NOT VALID-OBJECT(ioParentBrowse) THEN DO:
      IF VALID-OBJECT(PARENT-BROWSE-OBJECT) THEN
        DYNAMIC-FUNCTION ("deleteObjectLink",QUERY-HANDLE,PARENT-BROWSE-OBJECT:BROWSE-HANDLE).
    END.
    ELSE DO:
      DYNAMIC-FUNCTION ("deleteObjectLink",QUERY-HANDLE,ioParentBrowse:BROWSE-HANDLE).
      DYNAMIC-FUNCTION("createOneToOneLink",THIS-OBJECT:QUERY-HANDLE,ioParentBrowse:BROWSE-HANDLE,icRelationShip).
    END.  

    PARENT-BROWSE-OBJECT = ioParentBrowse.
                                                                                        
  END METHOD.  
  
  METHOD PUBLIC VOID setOneToOneQueryObject(ioParentQuery  AS JBoxQuery
                                           ,icRelationship AS CHAR):
    IF NOT VALID-OBJECT(ioParentQuery) THEN DO:
      IF VALID-OBJECT(PARENT-QUERY-OBJECT) THEN
        DYNAMIC-FUNCTION ("deleteObjectLink",QUERY-HANDLE,PARENT-QUERY-OBJECT:QUERY-HANDLE).
    END.
    ELSE DO:
      DYNAMIC-FUNCTION ("deleteObjectLink",QUERY-HANDLE,ioParentQuery:QUERY-HANDLE).
      DYNAMIC-FUNCTION("createOneToOneLink",THIS-OBJECT:QUERY-HANDLE,ioParentQuery:QUERY-HANDLE,icRelationShip).
    END.  

    PARENT-QUERY-OBJECT = ioParentQuery.
  END METHOD.  

  METHOD PUBLIC VOID displayRecord():
    RUN InvokeMethod(QUERY-HANDLE,"DisplayRecord").
  END METHOD.

  METHOD PUBLIC VOID excelRecord():
    RUN InvokeMethod(QUERY-HANDLE,"ExcelRecord").
  END METHOD.
    
  METHOD PUBLIC LOGICAL refreshRow():
    RETURN DYNAMIC-FUNCTION("RefreshRowids",THIS-OBJECT:QUERY-HANDLE,THIS-OBJECT:BUFFER-HANDLE::RowIdent1) > 0. 
  END METHOD.
           
  METHOD PUBLIC VOID setFilterDropDownQuery(icFieldName AS CHAR
                                           ,icBuffersAndFields AS CHAR
                                           ,icQuery AS CHAR):
    DYNAMIC-FUNCTION("setAttribute",QUERY-HANDLE,"filterDropdownFields_" + icFieldName,icBuffersAndFields).
    DYNAMIC-FUNCTION("setAttribute",QUERY-HANDLE,"filterDropdownQuery_" + icFieldName,icQuery).
  END METHOD.  

  METHOD PUBLIC VOID setFilterDropDownList(icFieldName  AS CHAR
                                      ,icValuePairs AS CHAR):
    DYNAMIC-FUNCTION("setAttribute",QUERY-HANDLE,"filterDropdownValuelist_" + icFieldName,icValuePairs).
  END METHOD.  

  METHOD PUBLIC VOID setFilterLookup(icFieldName  AS CHAR
                                    ,icBuffersAndFields AS CHAR
                                    ,icQuery AS CHAR):
    DYNAMIC-FUNCTION("setAttribute",QUERY-HANDLE,"filterLookupFields_" + icFieldName,icBuffersAndFields).
    DYNAMIC-FUNCTION("setAttribute",QUERY-HANDLE,"filterLookupQuery_" + icFieldName,icQuery).
    DYNAMIC-FUNCTION("setAttribute",QUERY-HANDLE,"filterLookupReturnField_" + icFieldName,icFieldName).
  END METHOD.  

  METHOD PUBLIC VOID setFilterLookup(icFieldName  AS CHAR
                                    ,icBuffersAndFields AS CHAR
                                    ,icQuery AS CHAR
                                    ,icReturnField AS CHAR):
    DYNAMIC-FUNCTION("setAttribute",QUERY-HANDLE,"filterLookupFields_" + icFieldName,icBuffersAndFields).
    DYNAMIC-FUNCTION("setAttribute",QUERY-HANDLE,"filterLookupQuery_" + icFieldName,icQuery).
    DYNAMIC-FUNCTION("setAttribute",QUERY-HANDLE,"filterLookupReturnField_" + icFieldName,icReturnField).
  END METHOD.  

  METHOD PUBLIC VOID setJBoxAttribute(icAttribute AS CHAR
                                     ,icValue AS CHAR):
    DYNAMIC-FUNCTION("setAttribute",QUERY-HANDLE,icAttribute,icValue).
  END METHOD.
  METHOD PUBLIC CHARACTER getJBoxAttribute(icAttribute AS CHAR):
    RETURN  DYNAMIC-FUNCTION("getAttribute",QUERY-HANDLE,icAttribute).
  END METHOD.
       
  METHOD PRIVATE LOGICAL resolveFilterExpr(icExprList AS CHAR,OUTPUT ocFields AS CHAR,OUTPUT ocOperators AS CHAR,OUTPUT ocValues AS CHAR):
    DEF VAR ix    AS INT  NO-UNDO.
    DEF VAR cExpr AS CHAR NO-UNDO.
    DEF VAR cOper AS CHAR NO-UNDO.
    
    
    DO ix = 1 TO NUM-ENTRIES(icExprList,"|"):
      
      ASSIGN cExpr       = ENTRY(ix,icExprList,"|")
             ocFields    = ocFields + (IF ocFields NE "" THEN "," ELSE "") + entry(1,cExpr," ")
             cOper       = THIS-OBJECT:getFilterOper(ENTRY(2,cExpr," "))
             ocOperators = ocOperators + (IF ocOperators NE "" THEN "," ELSE "") + cOper
             ocValues    = ocValues + (IF ocValues NE "" THEN "|" ELSE "") + replace(REPLACE(TRIM(substr(cExpr,INDEX(cExpr,cOper) + length(cOper))),"'",""),'"','"')
             .
    END.
    RETURN YES.
  END METHOD.
    
  METHOD PUBLIC VOID setPreScanQuery(icTable  AS CHAR
                                    ,icExpr   AS CHAR):
    DYNAMIC-FUNCTION("setAttribute",THIS-OBJECT:QUERY-HANDLE,"prescanQuery" + icTable,icExpr).                                      
  END METHOD.  

  METHOD PUBLIC VOID setExtraFilterField(icTable      AS CHAR
                                        ,icField      AS CHAR
                                        ,icLabel      AS CHAR
                                        ,icPrescanQry AS CHAR):
    DEF VAR cExtraFilter AS CHAR NO-UNDO.
    cExtraFilter = DYNAMIC-FUNCTION("getAttribute",THIS-OBJECT:QUERY-HANDLE,"extraFilterFields").
    cExtraFilter = cExtraFilter + (IF cExtraFilter NE "" THEN "," ELSE "")
                 + icTable + "." + icField + "|" + icLabel.                                           
    DYNAMIC-FUNCTION("setAttribute",THIS-OBJECT:QUERY-HANDLE,"extraFilterFields",cExtraFilter).
    DYNAMIC-FUNCTION("setAttribute",THIS-OBJECT:QUERY-HANDLE,"prescanQuery" + icTable,icPrescanQry).                                      
  END METHOD.  

  /* Process all rows for query crit: */ 
  METHOD PUBLIC LOGICAL processSet(icProcName AS CHAR
                                  ,icParam AS CHAR):
    DEF VAR bOk AS LOG NO-UNDO.       
    IF cCallFromMethod = "" THEN cCallFromMethod = ENTRY(1,PROGRAM-NAME(2)," ").                                    
    bOk = DYNAMIC-FUNCTION("processQuery",QUERY-HANDLE,icProcName,icParam).
    JBoxServerAPI:Instance:CreateCall(cCallFromMethod,icParam,bOk,?,hSourceProc).
    cCallFromMethod = "".
    RETURN bOk.
  END METHOD.
  
  METHOD PUBLIC LOGICAL processSetNoMessage(icProcName AS CHAR
                                           ,icParam AS CHAR):
    DEF VAR bOk AS LOG NO-UNDO.                    
    cCallFromMethod = ENTRY(1,PROGRAM-NAME(2)," ").                       
    DYNAMIC-FUNCTION("setAttribute",QUERY-HANDLE,"viewErrorsFromProcessRows","no").
    bOk = THIS-OBJECT:processSet(icProcName,icParam).
    DYNAMIC-FUNCTION("setAttribute",QUERY-HANDLE,"viewErrorsFromProcessRows","").
    RETURN bOK.
  END METHOD.

  DESTRUCTOR PUBLIC JBoxQuery ( ):
/*	  DYNAMIC-FUNCTION("DeleteObject",QUERY-HANDLE).*/
  END DESTRUCTOR.

END CLASS.
