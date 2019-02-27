 
 /*------------------------------------------------------------------------
    File        : JBoxBuffer
    Purpose     : Client table buffer
    Syntax      : 
    Description : Represents a db table buffer at the client side. 
                  Must not be mixed with the fieldMap object that in addition to
                  representing a buffer also manages UI input/output
    Author(s)   : Brynjar
    Created     : Fri Apr 24 12:43:22 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

CLASS JBoxBuffer:
  DEF VAR cDbName     AS CHAR NO-UNDO.
  DEF VAR httTable AS HANDLE NO-UNDO.
  
  DEFINE PUBLIC PROPERTY DB-NAME AS CHAR  
         GET(): cDbName. END GET.
         PROTECTED SET.

  DEFINE PUBLIC PROPERTY BUFFER-HANDLE AS HANDLE  
         GET. PRIVATE SET.

  DEFINE PUBLIC PROPERTY BUFFER-NAME AS CHAR  
         GET. PRIVATE SET.

  DEFINE PUBLIC PROPERTY BUFFER-FIELDS AS CHAR  
         GET. PRIVATE SET.
         
  DEFINE PUBLIC PROPERTY QUERY-WHERE AS CHAR INIT "WHERE FALSE"  
         GET. SET.

  DEFINE PUBLIC PROPERTY customCreateProc AS CHAR  
         GET. SET.
         
  DEFINE PUBLIC PROPERTY customUpdateValProc AS CHAR  
         GET. SET.
         
  DEFINE PUBLIC PROPERTY customDeleteValProc AS CHAR  
         GET. SET.
         
  DEFINE PUBLIC PROPERTY postUpdateProc AS CHAR  
         GET. SET.

  DEFINE PUBLIC PROPERTY transactionMessage AS CHAR  
         GET. PRIVATE SET.
                   
  CONSTRUCTOR PUBLIC JBoxBuffer (INPUT icDbBufferName AS CHAR):
    DEF VAR ix       AS INT    NO-UNDO.

    IF NUM-ENTRIES(icDbBufferName,".") = 2 THEN 
      ASSIGN cDbName     = ENTRY(1,icDbBufferName,".") + "."
             BUFFER-NAME = ENTRY(2,icDbBufferName,".").
    ELSE BUFFER-NAME = icDbBufferName.
        
    httTable = DYNAMIC-FUNCTION("getTempTableJoin",0,0,"",BUFFER-NAME,QUERY-WHERE).
  
    BUFFER-HANDLE = httTable:DEFAULT-BUFFER-HANDLE.
    DO ix = 1 TO BUFFER-HANDLE:NUM-FIELDS:  
      BUFFER-FIELDS = BUFFER-FIELDS + (IF BUFFER-FIELDS NE "" THEN "," ELSE "") + BUFFER-HANDLE:BUFFER-FIELD(ix):NAME.
    END.  
    
  END CONSTRUCTOR.
                             
  METHOD PUBLIC LOGICAL Find (icWhere AS CHAR).
    DEF VAR httTable AS HANDLE NO-UNDO.
    DEF VAR hBuffer  AS HANDLE NO-UNDO.
    DEF VAR hQuery   AS HANDLE NO-UNDO.
    DEF VAR ix       AS INT    NO-UNDO.

    QUERY-WHERE = icWhere.
    
    httTable = DYNAMIC-FUNCTION("getTempTableJoin",0,0,"",BUFFER-NAME,QUERY-WHERE).
    IF BUFFER-HANDLE:AVAIL THEN BUFFER-HANDLE:BUFFER-DELETE().
    hBuffer = httTable:DEFAULT-BUFFER-HANDLE.
    CREATE QUERY hQuery.
    hQuery:SET-BUFFERS(hBuffer).
    hQuery:QUERY-PREPARE("FOR EACH " + hBuffer:NAME + " WHERE true").
    hQuery:QUERY-OPEN().
    hQuery:GET-FIRST().
    REPEAT WHILE NOT hQuery:QUERY-OFF-END OR ix < 2:
      ix = ix + 1.
      IF ix = 1 THEN
        BUFFER-HANDLE:BUFFER-COPY(hBuffer).  
      hQuery:GET-NEXT().
    END.
    DELETE OBJECT hQuery.      
    DELETE OBJECT httTable.
    
    IF ix = 0 THEN RETURN NO.
    ELSE IF ix = 1 THEN RETURN YES.
    ELSE RETURN ?.
    
  END METHOD.
  
  METHOD PUBLIC LOGICAL Save():
    DEF VAR cFieldList AS CHAR NO-UNDO.  
    DEF VAR cValueList AS CHAR NO-UNDO.
    DEF VAR ix         AS INT  NO-UNDO.
    DEF VAR bOk        AS LOG  NO-UNDO.
            
    IF NOT BUFFER-HANDLE:AVAIL THEN RETURN NO.
            
    DO ix = 1 TO BUFFER-HANDLE:NUM-FIELDS:
      ASSIGN cFieldList = cFieldList + (IF cFieldList NE "" THEN "," ELSE "") + BUFFER-HANDLE:BUFFER-FIELD(ix):NAME         
             cValueList = cValueList + (IF BUFFER-HANDLE:BUFFER-FIELD(ix):BUFFER-VALUE NE ? THEN 
                                          (IF BUFFER-HANDLE:BUFFER-FIELD(ix):DATA-TYPE NE "CHARACTER" THEN
                                            STRING(BUFFER-HANDLE:BUFFER-FIELD(ix):BUFFER-VALUE)
                                           ELSE BUFFER-HANDLE:BUFFER-FIELD(ix):BUFFER-VALUE)
                                        ELSE "?") + CHR(1).
    END.
    cValueList = SUBSTR(cValueList,1,LENGTH(cValueList) - 1).                                    

    DYNAMIC-FUNCTION("setPostUpdProc",postUpdateProc).
            
    IF BUFFER-HANDLE::RowIdent1 = "" THEN         
      bOk = DYNAMIC-FUNCTION("DoCreate",cDbName + BUFFER-NAME,
                  IF customUpdateValProc NE "" THEN
                    LEFT-TRIM(RIGHT-TRIM(customUpdateValProc + ",=" +
                                         customCreateProc,",="),",")
                  ELSE IF customCreateProc NE "" THEN
                    ",=" + customCreateProc
                  ELSE "",
                  cFieldList,
                  cValueList,
                  YES).
    ELSE
      bOk = DYNAMIC-FUNCTION("DoUpdate",cDbName + BUFFER-NAME,
                  customUpdateValProc,
                  "",
                  BUFFER-HANDLE::RowIdent1,
                  cFieldList,
                  cValueList,
                  YES).
                  
    transactionMessage = DYNAMIC-FUNCTION("getTransactionMessage").
                  
    IF bOk THEN 
      RETURN DYNAMIC-FUNCTION("DoRefetchTrans",BUFFER-HANDLE,"FIRST","").
    ELSE 
      RETURN NO.       
  END METHOD.

  METHOD PUBLIC LOGICAL Save(icParam AS CHAR):
    DYNAMIC-FUNCTION("setServerTransInputParam",icParam).
    THIS-OBJECT:Save(). 
  END METHOD.    
  
  METHOD PUBLIC LOGICAL Delete():
    DEF VAR bOk AS LOG NO-UNDO.         
    
    IF NOT BUFFER-HANDLE:AVAIL THEN RETURN NO.
            
    DYNAMIC-FUNCTION("setPostUpdProc",postUpdateProc).
            
    bOk = DYNAMIC-FUNCTION("DoUpdate",cDbName + BUFFER-NAME,
                  customDeleteValProc,
                  "",
                  BUFFER-HANDLE::RowIdent1,
                  YES).
    transactionMessage = DYNAMIC-FUNCTION("getTransactionMessage").
    
    RETURN bOk.
    
  END METHOD.

  METHOD PUBLIC LOGICAL Delete(icParam AS CHAR):
    DYNAMIC-FUNCTION("setServerTransInputParam",icParam).
    THIS-OBJECT:Delete(). 
  END METHOD.    
  
  METHOD PUBLIC LOGICAL AddField(icFieldName AS CHAR
                                ,icDataType  AS CHAR):

    DEF VAR httNew AS HANDLE NO-UNDO.
    DEF VAR ix     AS INT    NO-UNDO.
    
    CREATE TEMP-TABLE httNew.
    httNew:CREATE-LIKE (BUFFER-HANDLE).
    httNew:ADD-NEW-FIELD(icFieldName,icDataType).
    httNew:TEMP-TABLE-PREPARE (BUFFER-NAME).
    httNew:DEFAULT-BUFFER-HANDLE:BUFFER-COPY (BUFFER-HANDLE).

    BUFFER-HANDLE = httNew:DEFAULT-BUFFER-HANDLE.
                                    
  END METHOD.                                  
  
                               
  DESTRUCTOR PUBLIC JBoxBuffer ( ):
    DELETE OBJECT THIS-OBJECT:BUFFER-HANDLE NO-ERROR.
    DELETE OBJECT httTable NO-ERROR.
  END DESTRUCTOR.
END CLASS.
  