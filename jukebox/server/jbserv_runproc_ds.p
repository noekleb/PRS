/* HUB for excecuting (most) BL procedures */

DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO. 
DEF INPUT  PARAM icRunProc    AS CHAR NO-UNDO.
DEF INPUT  PARAM icParamList  AS CHAR NO-UNDO.
DEF INPUT-OUTPUT PARAM DATASET-HANDLE iohDs.
DEF OUTPUT PARAM ocReturn     AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOk         AS LOG NO-UNDO.

DEF VAR bAccess AS LOG    NO-UNDO INIT YES.
DEF VAR hJbAPI  AS HANDLE NO-UNDO.

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

IF SEARCH(SUBSTR(icRunProc,1,R-INDEX(icRunProc,".")) + "p") NE ? OR 
   SEARCH(SUBSTR(icRunProc,1,R-INDEX(icRunProc,".")) + "r") NE ? OR 
   SEARCH(SUBSTR(icRunProc,1,R-INDEX(icRunProc,".")) + "w") NE ? THEN DO:

  IF SEARCH("jbserv_check_runproc_access.p") NE ? OR SEARCH("jbserv_check_runproc_access.r") NE ? THEN
    RUN jbserv_check_runproc_access.p (icSessionId,icRunProc,iCurrCompanyId,OUTPUT bAccess) NO-ERROR.
  IF bAccess THEN DO:
    RUN VALUE(icRunProc) (icParamList,
/*                           INPUT-OUTPUT DATASET-HANDLE iohDs, */
                          IF VALID-HANDLE(iohDs) THEN
                            iohDs
                          ELSE ?,
                          icSessionId,
                          OUTPUT ocReturn, 
                          OUTPUT obOK).
    IF VALID-HANDLE(hJbAPI) THEN DO:
      DYNAMIC-FUNCTION("setPreTransBuffer" IN hJbAPI,?).  
      DELETE PROCEDURE hJbAPI.  
    END. 
  END.
  ELSE ocReturn = "You don't have access to run this procedure".
END.

ELSE ocReturn = "Couldn't find " + icRunProc + " on server".

IF VALID-HANDLE(iohDs) THEN DELETE OBJECT iohDs.

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
