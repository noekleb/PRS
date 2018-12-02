/* ----------------------------------------- (c) 2014 CHSO -------------- */
/* ---- Program Name : jBoxPropertyControl.cls                        ---- */
/* ---- Description  : Startup of OSKPlu buttons                     ---- */
/* ---- Author       : Curt H. Oldenborg                             ---- */
/* ---- Date Started : 2014-05-01                                    ---- */
/* ---------------------------------------------------------------------- */
                                                                     
ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS uc.JBoxUCPropertyControl FINAL: 
    
   DEFINE PUBLIC VARIABLE PropertyTableHandle AS HANDLE NO-UNDO. 

   DEFINE TEMP-TABLE PROPERTY NO-UNDO
       FIELD PropertyParentHandle AS HANDLE LABEL "Parent Handle"
       FIELD PropertyParentType   AS CHAR   LABEL "Parent Type"
       FIELD PropertyParentName   AS CHAR   LABEL "Parent Name"
       FIELD PropertyParentNumber AS INT    LABEL "Parent Number"
       FIELD PropertyName         AS CHAR   LABEL "Property Name"
       FIELD PropertyType         AS CHAR   LABEL "Property Type"
       FIELD PropertyValue        AS CHAR   LABEL "Property Value". 

   CONSTRUCTOR PUBLIC JBoxUCPropertyControl ():
               
        PropertyTableHandle = BUFFER PROPERTY:HANDLE. 
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    
   END CONSTRUCTOR.



   METHOD PUBLIC VOID addPropertyRecord 
       (INPUT ipHandle AS HANDLE, 
        INPUT ipcpropertyType AS CHAR, 
        INPUT ipcPropertyName AS CHAR,
        INPUT ipcPropertyValue AS CHAR ):

       DEFINE VARIABLE cType AS CHAR NO-UNDO.     

       IF ipHandle:TYPE = "BUFFER-FIELD" THEN
          cType = "COLUMN".
        
       ELSE IF ipHandle:TYPE = "BUFFER" THEN
            cType = "TABLE".
      
       CREATE PROPERTY.
       ASSIGN 
           PROPERTY.PropertyParentHandle = ipHandle
           PROPERTY.PropertyParentType   = cType
           PROPERTY.PropertyParentName   = ipHandle:NAME
           PROPERTY.PropertyParentNumber = ipHandle:POSITION NO-ERROR.
       ASSIGN 
           PROPERTY.propertyValue = STRING(ipcPropertyValue)
           PROPERTY.propertyType  = ipcpropertyType
           PROPERTY.PropertyName  = ipcPropertyName NO-ERROR.

   END. 

   DEF VAR ghDset AS HANDLE. 

   METHOD PUBLIC VOID BuildDataSetInit ():
       IF NOT VALID-HANDLE(ghDset) THEN
          CREATE DATASET ghDset .
      ghDset:NAME = "DataSource". 
      ghDset:ADD-BUFFER(BUFFER PROPERTY:HANDLE).
   END.


   METHOD PUBLIC HANDLE cpDBToTempTable__ (hBuffer AS HANDLE):
       DEFINE VARIABLE tth AS HANDLE NO-UNDO. 
       DEFINE VARIABLE hDataSource AS HANDLE NO-UNDO. 

       CREATE DATA-SOURCE hDataSource. 
       IF hBuffer:TABLE-HANDLE = ? THEN
       DO:
           CREATE TEMP-TABLE tth.
           tth:CREATE-LIKE(hBuffer).
           tth:TEMP-TABLE-PREPARE(hBuffer:TABLE).
           tth:ATTACH-DATA-SOURCE(hDataSource). 
       END.                                                                  
       RETURN tth:DEFAULT-BUFFER-HANDLE. 
   END. 



   METHOD PUBLIC HANDLE cpDBToTempTable (hBuffer AS HANDLE):
       DEFINE VARIABLE tth AS HANDLE NO-UNDO. 
       DEFINE VARIABLE hDataSource AS HANDLE NO-UNDO. 
       DEFINE VARIABLE hDset AS HANDLE NO-UNDO. 
       DEFINE VARIABLE hBuffer1 AS HANDLE NO-UNDO. 
       DEFINE VARIABLE hDummy AS HANDLE NO-UNDO.

       IF hBuffer:TABLE-HANDLE = ? THEN
       DO:
           CREATE DATA-SOURCE hDataSource. 
           CREATE TEMP-TABLE tth.
           tth:CREATE-LIKE(hBuffer).
           tth:TEMP-TABLE-PREPARE(hBuffer:TABLE).
           hBuffer1 = tth:DEFAULT-BUFFER-HANDLE.
                        
           /* Code for moving data from database to temp-table workaround - with filling
              dataset */ 
           CREATE DATASET hDset. 
           hDset:ADD-BUFFER(hBuffer1).
           hDataSource:ADD-SOURCE-BUFFER(hBuffer,""). 
           hBuffer1 = tth:DEFAULT-BUFFER-HANDLE.
           hBuffer1:ATTACH-DATA-SOURCE(hDataSource). 
           hBuffer1:FILL().       
           hDset:SET-BUFFERS(hDummy) NO-ERROR. 
           
           DELETE OBJECT hDataSource. 
           DELETE OBJECT hDset. 

           RETURN tth:DEFAULT-BUFFER-HANDLE. 
       END.
       RETURN hBuffer. 
   END. 
   


   METHOD PUBLIC HANDLE ExportXml (INPUT hDataSet AS HANDLE ):   
   
   hDataSet:WRITE-XML ("file", "C:\tmp\ds.xml", TRUE, ?,  ?, FALSE,FALSE).
        
   /*
        hDataSet:READ-XML ("file", "C:\tmp\OnlineReport.xml", "empty", ?,  ?, ?,?).
        */
   END.

   /* - methods for adding temp-tables to dataset, need to keep it in this cls for changes in the future  */ 
   METHOD PUBLIC HANDLE BuildDataSet (INPUT-OUTPUT hBuffer1 AS HANDLE):
       BuildDataSetInit().   
       hBuffer1 = cpDBToTempTable(hBuffer1). 
       ghDset:ADD-BUFFER(hBuffer1). RETURN ghDset.
   END.
   METHOD PUBLIC HANDLE BuildDataSet (hBuffer1 AS HANDLE,hBuffer2 AS HANDLE):
           BuildDataSetInit().   
           ghDset:ADD-BUFFER(hBuffer1). ghDset:ADD-BUFFER(hBuffer2). RETURN ghDset. 
   END.
   METHOD PUBLIC VOID BuildDataSet (hBuffer1 AS HANDLE,hBuffer2 AS HANDLE,hBuffer3 AS HANDLE):
         BuildDataSetInit().   
         ghDset:ADD-BUFFER(hBuffer1). ghDset:ADD-BUFFER(hBuffer2). ghDset:ADD-BUFFER(hBuffer3).
   END.
   METHOD PUBLIC VOID BuildDataSet (hBuffer1 AS HANDLE,hBuffer2 AS HANDLE,hBuffer3 AS HANDLE,hBuffer4 AS HANDLE):
       BuildDataSetInit().   
       ghDset:ADD-BUFFER(hBuffer1). ghDset:ADD-BUFFER(hBuffer2). ghDset:ADD-BUFFER(hBuffer3). ghDset:ADD-BUFFER(hBuffer4).
   END.
   METHOD PUBLIC VOID BuildDataSet (hBuffer1 AS HANDLE,hBuffer2 AS HANDLE,hBuffer3 AS HANDLE,hBuffer4 AS HANDLE,hBuffer5 AS HANDLE):
       BuildDataSetInit().   
       ghDset:ADD-BUFFER(hBuffer1). ghDset:ADD-BUFFER(hBuffer2). ghDset:ADD-BUFFER(hBuffer3). ghDset:ADD-BUFFER(hBuffer4). ghDset:ADD-BUFFER(hBuffer5).
   END.
   METHOD PUBLIC VOID BuildDataSet (hBuffer1 AS HANDLE,hBuffer2 AS HANDLE,hBuffer3 AS HANDLE,hBuffer4 AS HANDLE,hBuffer5 AS HANDLE,hBuffer6 AS HANDLE):
       BuildDataSetInit().   
       ghDset:ADD-BUFFER(hBuffer1). ghDset:ADD-BUFFER(hBuffer2). ghDset:ADD-BUFFER(hBuffer3). ghDset:ADD-BUFFER(hBuffer4). ghDset:ADD-BUFFER(hBuffer5). ghDset:ADD-BUFFER(hBuffer6).
   END.



   METHOD PUBLIC VOID setProperty ( ipHandle AS HANDLE,ipcPropertyName AS CHAR,iplValue AS LOGICAL ):
      addPropertyRecord(ipHandle,"LOGICAL",ipcPropertyName,STRING(iplValue)).
   END. 

   METHOD PUBLIC VOID setProperty ( ipHandle AS HANDLE,ipcPropertyName AS CHAR,iplValue AS CHAR ):
      addPropertyRecord(ipHandle,"CHARACTER",ipcPropertyName,STRING(iplValue)).
   END. 

   METHOD PUBLIC VOID setProperty ( ipHandle AS HANDLE,ipcPropertyName AS CHAR,iplValue AS DECIMAL ):
      addPropertyRecord(ipHandle,"DECIMAL",ipcPropertyName,STRING(iplValue)).
   END. 

   METHOD PUBLIC VOID setProperty (ipHandle AS HANDLE,ipcPropertyName AS CHAR,iplValue AS INTEGER ):
      addPropertyRecord(ipHandle,"INTEGER",ipcPropertyName,STRING(iplValue)).
   END. 


   METHOD PUBLIC HANDLE PropertyTableHandle ():
      RETURN BUFFER PROPERTY:HANDLE. 
   END. 

   DESTRUCTOR PUBLIC JBoxUCPropertyControl ( ):
   END DESTRUCTOR.

END CLASS.

