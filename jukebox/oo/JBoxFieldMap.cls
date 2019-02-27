 
 /*------------------------------------------------------------------------
    File        : JBoxFieldMap
    Purpose     : Enable automatic display of record (buffer) content. Also enable automatic save of updateable fields
    Syntax      : 
    Description : Management of relationships between fields and buffer
    Author(s)   : brynjar
    Created     : Tue Oct 15 13:18:49 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.


CLASS JBoxFieldMap: 
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
  DEF VAR hSourceProc   AS HANDLE NO-UNDO.
  DEF VAR hFrame        AS HANDLE NO-UNDO.
	
  DEF PUBLIC PROPERTY BUFFER-HANDLE AS HANDLE GET. PROTECTED SET.
  DEF PUBLIC PROPERTY QUERY-HANDLE AS HANDLE GET. PROTECTED SET.
  
  DEF PUBLIC PROPERTY BROWSE-OBJECT AS JBoxBrowse GET.
    SET(ioBrowse AS JBoxBrowse):
      IF NOT VALID-OBJECT(ioBrowse) THEN DO:
        IF VALID-OBJECT(BROWSE-OBJECT) THEN
          DYNAMIC-FUNCTION ("deleteObjectLink",BUFFER-HANDLE,BROWSE-OBJECT:BROWSE-HANDLE).
      END.
      ELSE DO:
        ASSIGN BUFFER-HANDLE = ioBrowse:BUFFER-HANDLE
               QUERY-HANDLE  = ioBrowse:QUERY-HANDLE
               .
        swap().  
        DYNAMIC-FUNCTION("createObjectLink",BUFFER-HANDLE,ioBrowse:BROWSE-HANDLE).
      END.
      BROWSE-OBJECT = ioBrowse.
    END SET.
    
  DEF PUBLIC PROPERTY QUERY-OBJECT AS JBoxQuery GET.
    SET(ioQuery AS JBoxQuery):
      IF NOT VALID-OBJECT(ioQuery) THEN DO:
        IF VALID-OBJECT(QUERY-OBJECT) THEN
          DYNAMIC-FUNCTION ("deleteObjectLink",BUFFER-HANDLE,QUERY-OBJECT:QUERY-HANDLE).
      END.
      ELSE DO:
        ASSIGN BUFFER-HANDLE = ioQuery:BUFFER-HANDLE
               QUERY-HANDLE  = ioQuery:QUERY-HANDLE
               .
        swap().  
        DYNAMIC-FUNCTION("createObjectLink",BUFFER-HANDLE,ioQuery:QUERY-HANDLE).
      END.
      QUERY-OBJECT = ioQuery.
    END SET.
    
  DEF PUBLIC PROPERTY TOOLBAR-OBJECT AS JBoxToolbar GET.
    SET(ioToolbar AS JBoxToolbar):
      swap().  
      IF NOT VALID-OBJECT(ioToolbar) THEN DO:
        IF VALID-OBJECT(TOOLBAR-OBJECT) THEN
          DYNAMIC-FUNCTION ("deleteObjectLink",BUFFER-HANDLE,TOOLBAR-OBJECT:TOOLBAR-HANDLE).
      END.
      ELSE DO:
        IF NOT VALID-HANDLE(BUFFER-HANDLE) THEN
          BUFFER-HANDLE = DYNAMIC-FUNCTION ("getLinkedObject",ioToolbar:TOOLBAR-HANDLE,"query","from").
        IF NOT VALID-HANDLE(BUFFER-HANDLE) THEN
          BUFFER-HANDLE = DYNAMIC-FUNCTION ("getLinkedObject",ioToolbar:TOOLBAR-HANDLE,"browse","from").
        IF VALID-HANDLE(BUFFER-HANDLE) THEN  
          DYNAMIC-FUNCTION("createObjectLink",BUFFER-HANDLE,ioToolbar:TOOLBAR-HANDLE).
        ELSE 
          MESSAGE "Cannot associate a Toolbar with a FieldMap that doesn't have an association" SKIP
                  "to a buffer thru a browse or query" SKIP
                  "(and when FieldMap iself also misses this association)"
          VIEW-AS ALERT-BOX.
      END.  
      TOOLBAR-OBJECT = ioToolbar.
    END SET.
      
  DEF PUBLIC PROPERTY updateFields AS CHAR 
      GET(): RETURN DYNAMIC-FUNCTION("getAttribute",BUFFER-HANDLE,"screenUpdateFields"). END GET. 
      SET(icFlds AS CHAR): 
        DEF VAR cScreenFlds AS CHAR NO-UNDO.
        DEF VAR cDbFlds     AS CHAR NO-UNDO.
        DEF VAR ix  AS INT NO-UNDO.
        IF NUM-ENTRIES(icFlds,";") > 1 THEN 
          DO ix = 1 TO NUM-ENTRIES(icFlds):
            ASSIGN cDbFlds = cDbFlds + (IF cDbFlds NE "" THEN "," ELSE "") + ENTRY(1,ENTRY(ix,icFlds),";")
                   cScreenFlds = cScreenFlds + (IF cScreenFlds NE "" THEN "," ELSE "") 
                               + (IF NUM-ENTRIES(ENTRY(ix,icFlds),";") > 1 THEN ENTRY(2,ENTRY(ix,icFlds),";") ELSE ENTRY(ix,icFlds))
                               . 
          END.  
        ELSE cDbFlds = icFlds.  
        DYNAMIC-FUNCTION("setObjectSourceProc",hSourceProc).  
        DYNAMIC-FUNCTION("appendFieldMap"
               ,BUFFER-HANDLE
               ,hFrame
               ,cDbFlds,cScreenFlds
               ,"",""
               ,"").
      END SET.
      
  DEF PUBLIC PROPERTY updateWidgets AS CHAR 
      GET(): RETURN DYNAMIC-FUNCTION("getAttribute",BUFFER-HANDLE,"screenUpdateWidgets"). END GET. 
      PROTECTED SET.
       
  DEF PUBLIC PROPERTY displayFields AS CHAR 
      GET(): RETURN DYNAMIC-FUNCTION("getAttribute",BUFFER-HANDLE,"screenDisplayFields"). END GET. 
      SET(icFlds AS CHAR): 
        DYNAMIC-FUNCTION("setObjectSourceProc",hSourceProc).  
        DYNAMIC-FUNCTION("appendFieldMap"
               ,BUFFER-HANDLE
               ,hFrame
               ,"",""
               ,icFlds,""
               ,"").
      END SET. 

  DEF PUBLIC PROPERTY primaryKeyFields AS CHAR 
      GET(): RETURN DYNAMIC-FUNCTION("getAttribute",BUFFER-HANDLE,"primaryKeyFields"). END GET. 
      SET(icFlds AS CHAR): 
        DYNAMIC-FUNCTION("setObjectSourceProc",hSourceProc).  
        DYNAMIC-FUNCTION("appendFieldMap"
               ,BUFFER-HANDLE
               ,hFrame
               ,"",""
               ,"",""
               ,";" + icFlds).
      END SET. 

  DEF PUBLIC PROPERTY timeInputFields AS CHAR 
      GET(): RETURN DYNAMIC-FUNCTION("getAttribute",BUFFER-HANDLE,"timeInputFields"). END GET. 
      SET(icSetting AS CHAR): 
        DYNAMIC-FUNCTION("setAttribute",hFrame,"timeInputFields",icSetting).  
        swap().  
      END SET.

  DEF PUBLIC PROPERTY recordAvailObjects AS CHAR 
      GET(): RETURN DYNAMIC-FUNCTION("getAttribute",BUFFER-HANDLE,"extraUpdateFields"). END GET. 
      SET(icFlds AS CHAR): 
        DYNAMIC-FUNCTION("setObjectSourceProc",hSourceProc).  
        BUFFER-HANDLE = DYNAMIC-FUNCTION("appendFieldMap"
               ,BUFFER-HANDLE
               ,hFrame
               ,"",""
               ,"",""
               ,icFlds).
      END SET.
       
  DEF PUBLIC PROPERTY objectState AS CHAR 
      GET(): RETURN DYNAMIC-FUNCTION("getObjectState",BUFFER-HANDLE). END GET. 
      SET(icState AS CHAR): 
        IF VALID-HANDLE(THIS-OBJECT:BUFFER-HANDLE) THEN DO:
          DYNAMIC-FUNCTION("setFieldMapState",THIS-OBJECT:BUFFER-HANDLE,icState).
          DYNAMIC-FUNCTION("setObjectState",THIS-OBJECT:BUFFER-HANDLE,icState). 
        END.   
        ELSE DO:
          DYNAMIC-FUNCTION("setObjectState",hFrame,icState).  
          swap().  
        END.  
      END SET. 

  DEF PUBLIC PROPERTY customCreateProc AS CHAR 
      GET(): RETURN DYNAMIC-FUNCTION("getAttribute",BUFFER-HANDLE,"customCreateProc"). END GET. 
      SET(icSetting AS CHAR): 
        DYNAMIC-FUNCTION("setAttribute",hFrame,"customCreateProc",icSetting).  
        swap().  
      END SET.

  DEF PUBLIC PROPERTY customUpdateValProc AS CHAR 
      GET(): RETURN DYNAMIC-FUNCTION("getAttribute",BUFFER-HANDLE,"customUdateValProc"). END GET. 
      SET(icSetting AS CHAR): 
        DYNAMIC-FUNCTION("setAttribute",hFrame,"customUpdateValProc",  
                         (IF icSetting = "ignore" OR icSetting BEGINS "+" THEN icSetting ELSE "=" + TRIM(icSetting,"="))).  
        swap().  
      END SET.
      
  DEF PUBLIC PROPERTY customDeleteValProc AS CHAR 
      GET(): RETURN DYNAMIC-FUNCTION("getAttribute",BUFFER-HANDLE,"customDeleteValProc"). END GET. 
      SET(icSetting AS CHAR): 
        DYNAMIC-FUNCTION("setAttribute",hFrame,"customDeleteValProc",
                         (IF icSetting = "ignore" OR icSetting BEGINS "+" THEN icSetting ELSE "=" + TRIM(icSetting,"="))).  
        swap().  
      END SET.
       
  DEF PUBLIC PROPERTY postUpdateProc AS CHAR 
      GET(): RETURN DYNAMIC-FUNCTION("getAttribute",BUFFER-HANDLE,"postUdateProc"). END GET. 
      SET(icSetting AS CHAR): 
        DYNAMIC-FUNCTION("setAttribute",hFrame,"postUpdateProc",icSetting).
        swap().  
      END SET.
       
  DEF PUBLIC PROPERTY bufferExtraFields AS CHAR 
      GET(): RETURN DYNAMIC-FUNCTION("getAttribute",BUFFER-HANDLE,"bufferExtraFields"). END GET. 
      SET(icSetting AS CHAR): 
        DYNAMIC-FUNCTION("setAttribute",hFrame,"bufferExtraFields",icSetting).
        swap().  
      END SET.
       
  DEF PUBLIC PROPERTY bufferExtraValues AS CHAR 
      GET(): RETURN DYNAMIC-FUNCTION("getAttribute",BUFFER-HANDLE,"bufferExtraValues"). END GET. 
      SET(icSetting AS CHAR): 
        DYNAMIC-FUNCTION("setAttribute",hFrame,"bufferExtraValues",icSetting).
        swap().  
      END SET.

  DEF PUBLIC PROPERTY serverTransInputParam AS CHAR 
      GET(): RETURN DYNAMIC-FUNCTION("getAttribute",BUFFER-HANDLE,"serverTransInputParam"). END GET. 
      SET(icSetting AS CHAR): 
        DYNAMIC-FUNCTION("setAttribute",hFrame,"serverTransInputParam",icSetting).
        swap().  
      END SET.
      
  DEF PUBLIC PROPERTY serverTransReturnParam AS CHAR 
      GET(): RETURN DYNAMIC-FUNCTION("getAttribute",BUFFER-HANDLE,"serverTransReturnParam"). END GET. 
      PROTECTED SET.
      
  DEF PUBLIC PROPERTY serverTransReturnMessage AS CHAR 
      GET(): RETURN DYNAMIC-FUNCTION("getAttribute",BUFFER-HANDLE,"serverTransReturnMessage"). END GET. 
      PROTECTED SET.
       
  DEF PUBLIC PROPERTY checkModified AS LOG 
      GET(): RETURN NOT CAN-DO("no,never",DYNAMIC-FUNCTION("getAttribute",BUFFER-HANDLE,"checkModified")). END GET. 
      SET(ibSetting AS LOG): 
        DYNAMIC-FUNCTION("setAttribute",hFrame,"checkModified",STRING(ibSetting)).
        swap().  
      END SET.
       
  DEF PUBLIC PROPERTY neverCheckModified AS LOG 
      GET(): RETURN DYNAMIC-FUNCTION("getAttribute",BUFFER-HANDLE,"checkModified") NE "never". END GET. 
      SET(ibSetting AS LOG): 
        DYNAMIC-FUNCTION("setAttribute",hFrame,"checkModified",IF ibSetting THEN "never" ELSE "").
        swap().  
      END SET. 

  DEF PUBLIC PROPERTY editBgColor AS INT 
      GET(): RETURN INT(DYNAMIC-FUNCTION("getAttribute",BUFFER-HANDLE,"editBgColor")). END GET. 
      SET(iiSetting AS INT): 
        DYNAMIC-FUNCTION("setAttribute",hFrame,"editBgColor",IF iiSetting = ? THEN "?" ELSE STRING(iiSetting)).
        swap().  
      END SET. 

  DEF PUBLIC PROPERTY dynamicValidation AS LOG 
      GET(): RETURN DYNAMIC-FUNCTION("getAttribute",BUFFER-HANDLE,"customUpdateValProc") NE "ignore". END GET. 
      SET(ibSetting AS LOG): 
        DYNAMIC-FUNCTION("setAttribute",hFrame,"customUpdateValProc",IF ibSetting THEN "" ELSE "ignore").
        swap().  
      END SET. 

  DEF PUBLIC PROPERTY currentField AS CHAR 
      GET():
        DEF VAR hCurrWidget AS HANDLE NO-UNDO.
        hCurrWidget = DYNAMIC-FUNCTION("getCurrentWidget").
        IF VALID-HANDLE(hCurrWidget) THEN
          RETURN hCurrWidget:NAME.
        ELSE RETURN ?. 
      END GET. 
      PROTECTED SET.
       
  DEF PUBLIC PROPERTY currentFieldModified AS LOG 
      GET():
        DEF VAR hCurrWidget AS HANDLE NO-UNDO.
        hCurrWidget = DYNAMIC-FUNCTION("getCurrentWidget").
        IF VALID-HANDLE(hCurrWidget) THEN
          RETURN hCurrWidget:MODIFIED.
        ELSE RETURN ?.  
      END GET. 
      PROTECTED SET. 

  DEF PUBLIC PROPERTY currentFieldHandle AS CHAR 
      GET():
        RETURN DYNAMIC-FUNCTION("getCurrentWidget").
      END GET. 
      PROTECTED SET. 

  DEF PUBLIC PROPERTY isCopy AS LOG 
      GET():
        IF VALID-OBJECT(TOOLBAR-OBJECT) THEN
          RETURN DYNAMIC-FUNCTION("getAttribute",TOOLBAR-OBJECT:TOOLBAR-HANDLE,"copyRecord") = "yes".
        ELSE RETURN ?.            
      END GET. 
      PROTECTED SET.
       
  DEF PUBLIC PROPERTY IsCurrent  AS LOG 
      GET(): 
        RETURN THIS-OBJECT:BUFFER-HANDLE = DYNAMIC-FUNCTION ("getCurrentObject"). END GET. 
      PROTECTED SET.

  DEF PUBLIC PROPERTY queryOnly AS LOG
      GET():
        RETURN DYNAMIC-FUNCTION("getAttribute",THIS-OBJECT:BUFFER-HANDLE,"queryOnly") = "yes".                    
      END GET. 
      SET(INPUT ibQueryOnly AS LOG):
        DYNAMIC-FUNCTION("setAttribute",THIS-OBJECT:BUFFER-HANDLE,"queryOnly",IF ibQueryOnly THEN "yes" ELSE "").
      END SET. 

  DEF PUBLIC PROPERTY nextFocusedField AS CHAR
      PROTECTED GET. 
      SET(INPUT icFieldName AS CHAR):
        DEF VAR hFld AS HANDLE NO-UNDO.
        DEF VAR cFldNames   AS CHAR NO-UNDO.
        DEF VAR cFldHandles AS CHAR NO-UNDO.
        ASSIGN cFldNames = DYNAMIC-FUNCTION("getAttribute",BUFFER-HANDLE,"ScreenUpdateFields")
                         + "," + DYNAMIC-FUNCTION("getAttribute",BUFFER-HANDLE,"ScreenDisplayFields")
                         + "," + DYNAMIC-FUNCTION("getAttribute",BUFFER-HANDLE,"ExtraUpdateFields")
               cFldHandles = DYNAMIC-FUNCTION("getAttribute",BUFFER-HANDLE,"ScreenUpdateWidgets")
                         + "," + DYNAMIC-FUNCTION("getAttribute",BUFFER-HANDLE,"ScreenDisplayWidgets")
                         + "," + DYNAMIC-FUNCTION("getAttribute",BUFFER-HANDLE,"ExtraUpdateWidgets")
               .           
        hFld = WIDGET-HANDLE(ENTRY(LOOKUP(icFieldName,cFldNames),cFldHandles)) NO-ERROR.
        IF NOT ERROR-STATUS:ERROR THEN
          DYNAMIC-FUNCTION("setWidgetEnter",hFld).
      END SET. 
    
		
  CONSTRUCTOR PUBLIC JBoxFieldMap (ihFrame AS HANDLE):
	SUPER ().
		
    IF PROVERSION < "11" THEN hSourceProc = DYNAMIC-FUNCTION ("getProcedureHandle",PROGRAM-NAME(2)).
    ELSE hSourceProc = SOURCE-PROCEDURE.
		
	  hFrame = ihFrame.		
		       
    DYNAMIC-FUNCTION("setObjectSourceProc",hSourceProc).  
    BUFFER-HANDLE = DYNAMIC-FUNCTION("NewFieldMap",QUERY-HANDLE,hFrame,"","","","","").
		       
	END CONSTRUCTOR.

  METHOD PUBLIC VOID editRecord():
    RUN InvokeMethod(BUFFER-HANDLE,"editRecord"). 
  END METHOD.
	
	METHOD PUBLIC LOGICAL saveRecord():
	  RUN InvokeMethod(BUFFER-HANDLE,"SaveRecord").	
	  RETURN DYNAMIC-FUNCTION("getTransactionMessage") = "".
	END METHOD.
	  
  METHOD PUBLIC LOGICAL saveRecord(INPUT icFields AS CHAR,
                                   INPUT icValues AS CHAR):
    DYNAMIC-FUNCTION("setAttribute",BUFFER-HANDLE,"bufferExtraFields",icFields).
    DYNAMIC-FUNCTION("setAttribute",BUFFER-HANDLE,"bufferExtraValues",icValues).
                                        
    RUN InvokeMethod(BUFFER-HANDLE,"SaveRecord"). 
    RETURN DYNAMIC-FUNCTION("getTransactionMessage") = "".
  END METHOD.  

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
    
  METHOD PUBLIC VOID setJBoxAttribute(icAttribute AS CHAR
                                     ,icValue AS CHAR):
    DYNAMIC-FUNCTION("setAttribute",BUFFER-HANDLE,icAttribute,icValue).
  END METHOD.
  METHOD PUBLIC CHARACTER getJBoxAttribute(icAttribute AS CHAR):
    RETURN  DYNAMIC-FUNCTION("getAttribute",BUFFER-HANDLE,icAttribute).
  END METHOD.
  
  METHOD PRIVATE VOID swap():
    IF VALID-HANDLE(BUFFER-HANDLE) THEN 
      DYNAMIC-FUNCTION ("swapObjectHandle",hFrame,BUFFER-HANDLE).
  END METHOD.

	DESTRUCTOR PUBLIC JBoxFieldMap ( ):
/*    DYNAMIC-FUNCTION("DeleteObject",BUFFER-HANDLE).*/
    DYNAMIC-FUNCTION("DeleteObject",hFrame) NO-ERROR. /* Placeholder - should not exist */
	END DESTRUCTOR.

END CLASS.