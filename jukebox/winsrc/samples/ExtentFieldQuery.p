/* StartOrderDrill.p
   Initial query for order drill-down. 
   Template for testing from AppBuilder or for invoking this type of query from any menu system.
   Corresponding example for invoking from JBoxDynMenu.w: winsrc/samples/StartOrderDrill.p
-------------------------------------------------------------------------*/
DEF VAR hDataBrwWin AS HANDLE NO-UNDO. /* Change to input parameter when invoked from JBoxDynMenu.w */
DEF VAR hBrowse     AS HANDLE NO-UNDO.

/* Skip when invoked from JBoxDynMenu.w */

RUN JBoxLoadLib.p ("JBoxUIlib.p,JBoxObjLib.p,JBoxFUlib.p,JBoxASlib.p,ResizeLib.p").
DYNAMIC-FUNCTION("setLanguageCode","EN"). /* Standard objects support english and norwegian (NO) */

/* Make the server routine log queries: */
DYNAMIC-FUNCTION("setASlibBehaviour","QueryLogFile|ServerQuery.log").   

RUN JBoxDataBrw.w PERSIST SET hDataBrwWin.

/* End Skip when invoked from JBoxDynMenu.w */

DYNAMIC-FUNCTION("setRowsToBatch" IN hDataBrwWin,100).

hDataBrwWin:CURRENT-WINDOW:TITLE = "Array field query".

/* Set the fields available for calculation of totals by rigth-click (default is all DECIMAL fields): */
DYNAMIC-FUNCTION("setTotalFields" IN hDataBrwWin,"Qty,LineTotal").

RUN InitializeObject IN hDataBrwWin (
    "SalesRep"
      + ";SalesRep"
      + ";RepName"
      + ";MonthQuota[1]"
      + ";MonthQuota[6]"
    ,"WHERE true"
     ,""
     ,TRUE). /* <- do record count */
   
hBrowse = DYNAMIC-FUNCTION("getBrowseHandle" IN hDataBrwWin).
DYNAMIC-FUNCTION("setAttribute",hBrowse,"accumdatatypes","integer,decimal").

/* Delete the following when running from JBoxDynMenu.w: */
SUBSCRIBE TO "InvalidateHandle" IN hDataBrwWin. 

WAIT-FOR "close" OF THIS-PROCEDURE.

PROCEDURE InvalidateHandle:
  DEF INPUT PARAM ihProc AS HANDLE NO-UNDO.
  IF NOT THIS-PROCEDURE:FILE-NAME MATCHES "*.cmp" THEN
    DELETE PROCEDURE THIS-PROCEDURE.
  ELSE QUIT.
END.
