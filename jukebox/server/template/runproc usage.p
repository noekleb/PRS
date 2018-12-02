/* jukebox/server/template/runproc usage.p
   Template for server procedure invoked by the "runproc" function:
   DYNAMIC-FUNCTION("runproc","myproc.p","<parameter string>",temp-table handle).
   
   If a temp-table is passed from the client the corresponding default buffer is input param here.
   Cleanup for buffer and TT is taken care of by the parent of this procedure which is jbserv_runproc.p
   
   Similar procedure can be used for the ProcessQuery and processSelectedRows methods.

   To invoke existing server logic in a custom server procedure you can start an api super procedure that exposes a set of standard functions:

   DYNAMIC-FUNCTION("startASlib" IN SOURCE-PROCEDURE).

   If you f.ex are creating customers in the custom .p procedure and you want to re-use the logic for assigning customer numbers that exists 
   in the hook customCreateProc you can now f.ex make this call after creating the Customer record:

   hBufCustomer = BUFFER Customer:HANDLE.
   CREATE Customer.
   RUN InvokeCreateProc ("customer_create.p",hBufCustomer,OUTPUT ocReturn).

   Or you can re-use an existing procedure (customDeleteValProc hook) to check if a record can be deleted:

   hBufFormDetail = BUFFER FormDetail:HANDLE.
   RUN InvokeDeleteValProc ("form_detail_delete.p",hBufFormDetail,
                                                OUTPUT ocReturn).
   IF ocReturn = "" THEN
     DELETE FormDetail.
     
   For a complete list of available functions see jbserv_api_for_server.p
   Probably the most used functions are:  
      DYNAMIC-FUNCTION("getASuserId")               ABhack: gau 
      DYNAMIC-FUNCTION("getCompany")                Character value of company id. ABhack: gcom  
                        getCompanyId                returns integer value
      DYNAMIC-FUNCTION("getFieldList","buffer;field;field..,buffer2..","WHERE ").       ABhack: gfl      
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DYNAMIC-FUNCTION("startASlib" IN SOURCE-PROCEDURE).

DEF VAR hQuery AS HANDLE NO-UNDO.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " NO-LOCK").
hQuery:QUERY-OPEN().


DO TRANSACTION ON ERROR UNDO,LEAVE:
  hQuery:GET-FIRST().
  REPEAT WHILE NOT hQuery:QUERY-OFF-END:

    hQuery:GET-NEXT().
  END.
END.

DELETE OBJECT hQuery.

obOk = ocReturn = "".

