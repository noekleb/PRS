/* Purpose:  Get query criteria from the client, run them through standard
             jbserv_gettemptablejoin.p and pass the result set to a server routine.
             As an option also return the (modified) resultset to the client
   Created:  31.jan.07 By brynjar@chemistry.no             
-----------------------------------------------------------------------------*/
DEF INPUT  PARAM icSessionId            AS CHAR NO-UNDO. 
DEF INPUT  PARAM icDirection            AS CHAR NO-UNDO.
DEF INPUT  PARAM icBuffersAndFields     AS CHAR NO-UNDO.
DEF INPUT  PARAM icQueryCriteria        AS CHAR NO-UNDO.
DEF INPUT  PARAM icQueryStatFields      AS CHAR NO-UNDO.
DEF INPUT  PARAM icParamString          AS CHAR NO-UNDO.
DEF INPUT  PARAM icRunProc              AS CHAR NO-UNDO.
DEF INPUT  PARAM icParamList            AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocQueryStatFieldValues AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn               AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOk                   AS LOG NO-UNDO.

DEF VAR hTempTable AS HANDLE NO-UNDO.
DEF VAR hBuffer    AS HANDLE NO-UNDO.
DEF VAR hJbAPI     AS HANDLE NO-UNDO.

{incl/validatesession.i}

FUNCTION getASuserId     RETURNS CHARACTER FORWARD.
FUNCTION getCompanyId    RETURNS INTEGER   FORWARD.
FUNCTION getCodeMaster   RETURNS INTEGER   FORWARD.
FUNCTION getLanguageCode RETURNS CHARACTER FORWARD.
FUNCTION Scandinavian    RETURNS LOGICAL   FORWARD.
FUNCTION startASlib      RETURNS HANDLE    FORWARD.
FUNCTION getSysParam     RETURNS CHARACTER
       ( INPUT icParamName AS CHAR,
         INPUT icValueType AS CHAR ) FORWARD.
FUNCTION getGenCode RETURNS CHARACTER
  ( INPUT icCodeType  AS CHAR,
    INPUT icValueDesc AS CHAR ) FORWARD.

RUN jbserv_gettemptablejoin.p
   (icSessionId,
    100000000,
    0,
    icDirection,
    icBuffersAndFields,
    icQueryCriteria,
    icQueryStatFields,
    icParamString,
    OUTPUT TABLE-HANDLE hTempTable,
    OUTPUT ocQueryStatFieldValues,
    OUTPUT ocReturn)
    NO-ERROR.

IF ocReturn NE "" THEN DO:
  obOk = NO.
  RETURN.
END.

IF SEARCH(SUBSTR(icRunProc,1,R-INDEX(icRunProc,".")) + "p") NE ? OR 
   SEARCH(SUBSTR(icRunProc,1,R-INDEX(icRunProc,".")) + "r") NE ? OR 
   SEARCH(SUBSTR(icRunProc,1,R-INDEX(icRunProc,".")) + "w") NE ? THEN
  RUN VALUE(icRunProc) (icParamList,
                        IF VALID-HANDLE(hTempTable) THEN 
                          hTempTable:DEFAULT-BUFFER-HANDLE
                        ELSE ?,
                        icSessionId,
                        OUTPUT ocReturn, 
                        OUTPUT obOK).

ELSE ocReturn = "Couldn't find " + icRunProc + " on server".

IF VALID-HANDLE(hTempTable) THEN DELETE OBJECT hTempTable.

FUNCTION getASuserId RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN cCurrUserId.

END FUNCTION.

FUNCTION getCompanyId RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN iCurrCompanyId. 

END FUNCTION.

FUNCTION getCodeMaster RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN iCodeMaster. 

END FUNCTION.

FUNCTION getLanguageCode RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN cCurrLanguage.

END FUNCTION.

FUNCTION Scandinavian RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  IF CAN-DO("da,dk,no,sv,se,nn,",cCurrLanguage) THEN
    RETURN TRUE.
  ELSE
    RETURN FALSE.  
END FUNCTION.

FUNCTION getSysParam RETURNS CHARACTER
  ( INPUT icParamName AS CHAR,
    INPUT icValueType AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cParamValue       AS CHAR   NO-UNDO.
DEF VAR hBuffJboxSysParam AS HANDLE NO-UNDO.

CREATE BUFFER hBuffJboxSysParam FOR TABLE "JBoxSysParam" NO-ERROR.
IF ERROR-STATUS:ERROR THEN
  RETURN ?.
DELETE OBJECT hBuffJboxSysParam.

IF SEARCH("jbadmin_getsysparam.p") NE ? OR SEARCH("jbadmin_getsysparam.r") NE ? THEN
  RUN jbadmin_getsysparam.p (icParamName,icValueType,iCurrCompanyId,iCodeMaster,OUTPUT cParamValue).
ELSE RETURN ?.

RETURN cParamValue.

END FUNCTION.

FUNCTION getGenCode RETURNS CHARACTER
  ( INPUT icCodeType  AS CHAR,
    INPUT icValueDesc AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cCodeValue        AS CHAR   NO-UNDO.
DEF VAR hBuffJboxGenCode  AS HANDLE NO-UNDO.

CREATE BUFFER hBuffJboxGenCode FOR TABLE "JBoxGenCode" NO-ERROR.
IF ERROR-STATUS:ERROR THEN
  RETURN ?.
DELETE OBJECT hBuffJboxGenCode.

IF SEARCH("jbadmin_getgeneralcode.p") NE ? OR SEARCH("jbadmin_getgeneralcode.r") NE ? THEN
  RUN jbadmin_getgeneralcode.p (icCodeType,icValueDesc,iCurrCompanyId,iCodeMaster,OUTPUT cCodeValue).
ELSE RETURN ?.

RETURN cCodeValue.

END FUNCTION.

FUNCTION startASlib RETURNS HANDLE ():
  RUN jbserv_api_for_server.p PERSIST SET hJbAPI (icSessionId).
  SOURCE-PROCEDURE:ADD-SUPER-PROCEDURE(hJbAPI).
  RETURN hJbAPI.
END.
