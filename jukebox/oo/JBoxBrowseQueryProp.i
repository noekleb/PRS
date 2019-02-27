/* Common properties for browse and query objects */
  DEF PUBLIC PROPERTY TOOLBAR-OBJECT AS JBoxToolbar GET. 
      SET(ioToolbar AS JBoxToolbar):
        IF NOT VALID-OBJECT(ioToolbar) THEN DO:
          IF VALID-OBJECT(oToolbar) THEN
            DYNAMIC-FUNCTION ("DeleteObjectLink",{1},oToolbar:TOOLBAR-HANDLE).
        END.
        ELSE DYNAMIC-FUNCTION("createObjectLink",{1},ioToolbar:TOOLBAR-HANDLE).
        oToolbar = ioToolbar.
      END SET.

  DEF PUBLIC PROPERTY TAB-FOLDER-OBJECT AS JBoxJlwTabs GET. SET. 

  DEF PUBLIC PROPERTY filterExcludeFields AS CHAR /* <field>,<field>,..*/
      GET(): RETURN DYNAMIC-FUNCTION("getAttribute",{1},"filterExcludeFields"). END GET.
      SET(icAttr AS CHAR): 
         DYNAMIC-FUNCTION("setAttribute",{1},"filterExcludeFields",icAttr).
      END SET.
   
  DEF PUBLIC PROPERTY baseQuery AS CHAR /* WHERE ..*/
      GET(): RETURN DYNAMIC-FUNCTION("getAttribute",{1},"baseQuery"). END GET.
      SET(icAttr AS CHAR): 
         DYNAMIC-FUNCTION("setAttribute",{1},"baseQuery",icAttr).
      END SET.
      
  DEF PUBLIC PROPERTY preScanBaseQuery AS CHAR /* To view customers for salesrep names matching *b*a* (Salesrep doesn't need to be included in the query): Salesrep WHERE Repname MATCHES '*b*a*',EACH Customer NO-LOCK OF Salesrep */
      GET(): RETURN DYNAMIC-FUNCTION("getAttribute",{1},"preScanBaseQuery"). END GET.
      SET(icAttr AS CHAR): 
         DYNAMIC-FUNCTION("setAttribute",{1},"preScanBaseQuery",icAttr).
      END SET.

  DEF PUBLIC PROPERTY preScanQueryWhere AS CHAR /* Ff a searchfield is implemented this property is automatically maintained based on the (internal) prescanQuery<table> attribute. Use the method setPrescanQuery */
      GET(): RETURN DYNAMIC-FUNCTION("getAttribute",{1},"preScanQueryWhere"). END GET.
      SET(icAttr AS CHAR): 
         DYNAMIC-FUNCTION("setAttribute",{1},"preScanQueryWhere",icAttr).
         DYNAMIC-FUNCTION("setAttribute",{1},"keepsearchvalue","yes").
      END SET.

  DEF PUBLIC PROPERTY preScanQueryFilter AS CHAR 
      GET(): RETURN DYNAMIC-FUNCTION("getAttribute",{1},"preScanQueryFilter"). END GET.
      SET(icAttr AS CHAR): 
         DYNAMIC-FUNCTION("setAttribute",{1},"preScanQueryFilter",icAttr).
      END SET.
        
  DEF PUBLIC PROPERTY calcFieldProc AS CHAR 
      GET(): RETURN DYNAMIC-FUNCTION("getAttribute",{1},"calcFieldProc"). END GET.
      SET(icAttr AS CHAR): 
         DYNAMIC-FUNCTION("setAttribute",{1},"calcFieldProc",icAttr).
      END SET.  

  DEF PUBLIC PROPERTY queryFilter AS CHAR 
      GET(): RETURN DYNAMIC-FUNCTION("getAttribute",{1},"queryFilter"). END GET.
      SET(icAttr AS CHAR): 
         DYNAMIC-FUNCTION("setAttribute",{1},"queryFilter",icAttr).
      END SET.  

  DEF PUBLIC PROPERTY queryWhere AS CHAR 
      GET(): RETURN DYNAMIC-FUNCTION("getAttribute",{1},"queryWhere"). END GET.
      SET(icAttr AS CHAR): 
         DYNAMIC-FUNCTION("setAttribute",{1},"queryWhere",icAttr).
      END SET.

  DEF PUBLIC PROPERTY altPrimaryBufferList AS CHAR 
      GET(): RETURN DYNAMIC-FUNCTION("getAttribute",{1},"altPrimaryBufferList"). END GET.
      SET(icAttr AS CHAR): 
         DYNAMIC-FUNCTION("setAttribute",{1},"altPrimaryBufferList",icAttr).
      END SET.

  DEF PUBLIC PROPERTY calcFieldFilter AS CHAR /* <fieldname>¤<operator>¤<value>|<fieldname>¤.. */ 
      GET(): RETURN DYNAMIC-FUNCTION("getAttribute",{1},"calcFieldFilter"). END GET.
      SET(icAttr AS CHAR): 
         DYNAMIC-FUNCTION("setAttribute",{1},"calcFieldFilter",icAttr).
      END SET.
        
  DEF PUBLIC PROPERTY calcFieldWhere AS CHAR /* <fieldname>¤<operator>¤<value>|<fieldname>¤.. */ 
      GET(): RETURN DYNAMIC-FUNCTION("getAttribute",{1},"calcFieldWhere"). END GET.
      SET(icAttr AS CHAR): 
         DYNAMIC-FUNCTION("setAttribute",{1},"calcFieldWhere",icAttr).
      END SET.  

  DEF PUBLIC PROPERTY allowCan-DoFilterOperator AS CHAR /* <fieldname>,<fieldname>,.. */ 
      GET(): RETURN DYNAMIC-FUNCTION("getAttribute",{1},"allowCan-DoFilterOperator"). END GET.
      SET(icAttr AS CHAR): 
         DYNAMIC-FUNCTION("setAttribute",{1},"allowCan-DoFilterOperator",icAttr).
      END SET.  
      
  DEF PUBLIC PROPERTY parentLink AS CHAR 
      GET(): RETURN DYNAMIC-FUNCTION("getAttribute",{1},"parentLink"). END GET. 
      SET(icSetting AS CHAR): 
        DYNAMIC-FUNCTION("setAttribute",{1},"parentLink",icSetting).
      END SET.
      
  DEF PUBLIC PROPERTY useLocalData AS LOG 
      GET(): RETURN DYNAMIC-FUNCTION("getAttribute",{1},"useLocalData") = "yes". END GET. 
      SET(ibSetting AS LOG): 
        DYNAMIC-FUNCTION("setAttribute",{1},"useLocalData",STRING(ibSetting)).
      END SET.
      
  DEF PUBLIC PROPERTY IsCurrent  AS LOG 
      GET(): 
        RETURN THIS-OBJECT:{1} = DYNAMIC-FUNCTION ("getCurrentObject"). END GET. 
      PROTECTED SET.
      
  DEF PUBLIC PROPERTY recordAvailable  AS LOG 
      GET(): 
       IF VALID-HANDLE(BUFFER-HANDLE) THEN RETURN BUFFER-HANDLE:AVAIL. ELSE RETURN NO. END GET. 
      PROTECTED SET.

  DEF PUBLIC PROPERTY querySort  AS CHAR 
      GET(): RETURN DYNAMIC-FUNCTION("getAttribute",{1},"querySort"). END GET. 
      PROTECTED SET. /* use setSortString to set */
      
  DEF PUBLIC PROPERTY localSort  AS CHAR 
      GET(): RETURN DYNAMIC-FUNCTION("getAttribute",{1},"localSort"). END GET. 
      PROTECTED SET. /* use setSortString to set */

  DEF PUBLIC PROPERTY getRecordCount AS LOG 
      GET(): RETURN DYNAMIC-FUNCTION("getAttribute",{1},"getRecordCount") NE "NO". END GET.
      SET(ibSet AS LOG): 
        IF ibSet THEN
          DYNAMIC-FUNCTION("setAttribute",{1},"getRecordCount","yes").
        ELSE
          DYNAMIC-FUNCTION("setAttribute",{1},"getRecordCount","no").
      END SET.
        
  DEF PUBLIC PROPERTY currentCount AS INT 
      GET(): RETURN INT(DYNAMIC-FUNCTION("getAttribute",{1},"currentCount")). END GET.
      SET(iiSet AS INT): DYNAMIC-FUNCTION("setAttribute",{1},"currentCount",STRING(iiSet)). END SET.  

  DEF PUBLIC PROPERTY recordCount AS INT 
      GET(): RETURN INT(DYNAMIC-FUNCTION("getAttribute",{1},"recordCount")). END GET.
      SET(iiSet AS INT): DYNAMIC-FUNCTION("setAttribute",{1},"recordCount",STRING(iiSet)). END SET.  

  DEF PUBLIC PROPERTY totalCount AS INT 
      GET(): RETURN INT(DYNAMIC-FUNCTION("getAttribute",{1},"totalCount")). END GET.
      SET(iiSet AS INT): DYNAMIC-FUNCTION("setAttribute",{1},"totalCount",STRING(iiSet)). END SET.  

  DEF PUBLIC PROPERTY viewRecordCount AS LOG 
      GET(): RETURN DYNAMIC-FUNCTION("getAttribute",{1},"viewRecordCount") NE "NO". END GET.
      SET(ibSet AS LOG): 
        IF ibSet THEN
          DYNAMIC-FUNCTION("setAttribute",{1},"viewRecordCount","yes").
        ELSE
          DYNAMIC-FUNCTION("setAttribute",{1},"viewRecordCount","no").
      END SET.

  DEF PUBLIC PROPERTY objectState AS CHAR 
      GET(): RETURN DYNAMIC-FUNCTION("getObjectState",{1}). END GET. 
      SET(icState AS CHAR): 
        DYNAMIC-FUNCTION("setObjectState",{1},icState).  
      END SET. 
      
  DEF PUBLIC PROPERTY filterFields AS CHAR 
      GET(): RETURN DYNAMIC-FUNCTION("getAttribute",{1},"filterFields"). END GET. 
      SET(icSetting AS CHAR): 
        DYNAMIC-FUNCTION("setAttribute",{1},"filterFields",icSetting).
      END SET.
      
  DEF PUBLIC PROPERTY flatViewBuffersAndFields AS CHAR 
      GET(): RETURN DYNAMIC-FUNCTION("getAttribute",{1},"flatViewBuffersAndFields"). END GET. 
      SET(icSetting AS CHAR): 
        DYNAMIC-FUNCTION("setAttribute",{1},"flatViewBuffersAndFields",icSetting).
      END SET.
      
  DEF PUBLIC PROPERTY flatViewQueryJoin AS CHAR 
      GET(): RETURN TRIM(DYNAMIC-FUNCTION("getAttribute",{1},"flatViewQueryJoin"),"-"). END GET. 
      SET(icSetting AS CHAR): 
        DYNAMIC-FUNCTION("setAttribute",{1},"flatViewQueryJoin",IF icSetting = "" THEN "-" ELSE icSetting).
      END SET.

  DEF PUBLIC PROPERTY flatViewTitle AS CHAR 
      GET(): RETURN DYNAMIC-FUNCTION("getAttribute",{1},"flatViewTitle"). END GET. 
      SET(icSetting AS CHAR): 
        DYNAMIC-FUNCTION("setAttribute",{1},"flatViewTitle",icSetting).
      END SET.

  DEF PUBLIC PROPERTY flatViewOuterJoin AS LOG /* Can be set on each query in a chain. If set on the top level it applies to all levels */ 
      GET(): RETURN DYNAMIC-FUNCTION("getAttribute",{1},"flatViewJoinType") = "outer-join". END GET. 
      SET(ibSetting AS LOG): 
        DYNAMIC-FUNCTION("setAttribute",{1},"flatViewJoinType",IF ibSetting THEN "OUTER-JOIN" ELSE "").
      END SET.

  DEF PUBLIC PROPERTY flatViewRecordCount AS LOG /* Must be set on top-level browse/query. Default ON */ 
      GET(): RETURN DYNAMIC-FUNCTION("getAttribute",{1},"flatViewRecordCount") NE "no". END GET. 
      SET(ibSetting AS LOG): 
        DYNAMIC-FUNCTION("setAttribute",{1},"flatViewRecordCount",STRING(ibSetting)).
      END SET.

  DEF PUBLIC PROPERTY excludeFromFlatView AS LOG 
      GET(): RETURN DYNAMIC-FUNCTION("getAttribute",{1},"excludeFromFlatView") = "yes". END GET. 
      SET(ibSetting AS LOG): 
        DYNAMIC-FUNCTION("setAttribute",{1},"excludeFromFlatView",STRING(ibSetting)).
      END SET.

  DEF PUBLIC PROPERTY accumDataTypes AS CHAR 
      GET(): RETURN DYNAMIC-FUNCTION("getAttribute",{1},"accumDataTypes"). END GET. 
      SET(icSetting AS CHAR): 
        DYNAMIC-FUNCTION("setAttribute",{1},"accumDataTypes",icSetting).
      END SET.
      
  DEF PUBLIC PROPERTY availAccumFields AS CHAR 
      GET(): RETURN DYNAMIC-FUNCTION("getAttribute",{1},"availAccumFields"). END GET. 
      SET(icSetting AS CHAR): 
        DYNAMIC-FUNCTION("setAttribute",{1},"availAccumFields",icSetting).
      END SET.
      
  DEF PUBLIC PROPERTY accumFields AS CHAR 
      GET(): RETURN DYNAMIC-FUNCTION("getAttribute",{1},"accumFields"). END GET. 
      SET(icSetting AS CHAR): 
        DYNAMIC-FUNCTION("setAttribute",{1},"accumFields",icSetting).
      END SET.

  DEF PUBLIC PROPERTY distinctDataTypes AS CHAR 
      GET(): RETURN DYNAMIC-FUNCTION("getAttribute",{1},"distinctDataTypes"). END GET. 
      SET(icSetting AS CHAR): 
        DYNAMIC-FUNCTION("setAttribute",{1},"distinctDataTypes",icSetting).
      END SET.
      
  DEF PUBLIC PROPERTY availDistinctColumns AS CHAR 
      GET(): RETURN DYNAMIC-FUNCTION("getAttribute",{1},"availDistinctColumns"). END GET. 
      SET(icSetting AS CHAR): 
        DYNAMIC-FUNCTION("setAttribute",{1},"availDistinctColumns",icSetting).
      END SET.
      
  DEF PUBLIC PROPERTY distinctColumns AS CHAR 
      GET(): RETURN DYNAMIC-FUNCTION("getAttribute",{1},"distinctColumns"). END GET. 
      SET(icSetting AS CHAR): 
        DYNAMIC-FUNCTION("setAttribute",{1},"distinctColumns",icSetting).
      END SET.

  DEF PUBLIC PROPERTY queryStatFields AS CHAR 
      GET(): RETURN DYNAMIC-FUNCTION("getAttribute",{1},"queryStatFields"). END GET. 
      SET(icSetting AS CHAR): 
        DYNAMIC-FUNCTION("setAttribute",{1},"queryStatFields",icSetting).
      END SET.

  DEF PUBLIC PROPERTY skipServerSort AS LOGICAL 
      GET(): RETURN DYNAMIC-FUNCTION("getAttribute",{1},"skipServerSort") = "yes". END GET. 
      SET(ibSetting AS LOGICAL): 
        DYNAMIC-FUNCTION("setAttribute",{1},"skipServerSort",IF ibSetting THEN "yes" ELSE "").
      END SET.

  DEF PUBLIC PROPERTY userSettingContext AS CHAR 
      GET(): RETURN DYNAMIC-FUNCTION("getAttribute",{1},"userSettingContext"). END GET. 
      SET(icSetting AS CHAR): 
        DYNAMIC-FUNCTION("setAttribute",{1},"userSettingContext",icSetting).
      END SET.

  DEF PUBLIC PROPERTY lastRecord AS LOG 
      GET(): RETURN VALID-HANDLE(BUFFER-HANDLE) AND BUFFER-HANDLE:AVAIL AND DYNAMIC-FUNCTION("getAttribute",{1},"lastRowid") = STRING(BUFFER-HANDLE:ROWID). END GET. 
      PROTECTED SET.
      
  DEF PUBLIC PROPERTY firstRecord AS LOG 
      GET(): RETURN VALID-HANDLE(BUFFER-HANDLE) AND BUFFER-HANDLE:AVAIL AND DYNAMIC-FUNCTION("getAttribute",{1},"firstRowid") = STRING(BUFFER-HANDLE:ROWID). END GET. 
      PROTECTED SET.

  DEF PUBLIC PROPERTY activeFilter AS LOG 
      GET(): RETURN DYNAMIC-FUNCTION("getAttribute",{1},"queryFilter") NE "" 
                 OR DYNAMIC-FUNCTION("getAttribute",{1},"prescanQueryFilter") NE ""
                 OR DYNAMIC-FUNCTION("getAttribute",{1},"calcFieldFilter") NE "". 
      END GET. 
      PROTECTED SET.
            
  DEF PUBLIC PROPERTY queryStopAfter AS INT 
      GET(): RETURN INT(DYNAMIC-FUNCTION("getAttribute",{1},"queryStopAfter")). END GET. 
      SET(iiSetting AS INTEGER): 
        DYNAMIC-FUNCTION("setAttribute",{1},"queryStopAfter",STRING(iiSetting)).
      END SET.
            