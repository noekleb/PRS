/* Create Customer
   Parameters:  New buffer
                Fields (comma-separated)
                Values (pipe-separated)
   
   Created: 06.10.05 by Brynjar Hasle                  

   To invoke: DYNAMIC-FUNCTION("setAttribute",hFieldMap | hBrowse,"customCreateProc","<procname>").
   
   In this procedure use the getValue function to get to values passed from the client rather
   than looking up icValues parameter to avoid planned change in use of delimiter

-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM hBuffer     AS HANDLE NO-UNDO.
DEF INPUT  PARAM icFields    AS CHAR NO-UNDO.
DEF INPUT  PARAM icValues    AS CHAR NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO. 

DEF VAR iCustNum AS INT NO-UNDO INIT 1.

DO ON ERROR UNDO, RETURN:
  FIND LAST Customer NO-LOCK NO-ERROR.
  IF AVAIL Customer THEN
    iCustNum = Customer.CustNum + 1.

  hBuffer:BUFFER-FIELD("CustNum"):BUFFER-VALUE = iCustNum.
END.

