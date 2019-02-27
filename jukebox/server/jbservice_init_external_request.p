/* jbservice_init_external_request.p 
   Parameters partner name, request type, request parameter, pass.phrase (opt)
   Returns requestid for completion.
   
   Created  19.11.12 by brynjar@chemistry.no
-------------------------------------------------------------------------*/   
DEF INPUT  PARAM icPartnerName       AS CHAR NO-UNDO.
DEF INPUT  PARAM icRequestType       AS CHAR NO-UNDO.
DEF INPUT  PARAM icRequestParam      AS CHAR NO-UNDO.
DEF INPUT  PARAM icRequestPassPhrase AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOk                AS LOG  NO-UNDO INIT YES.
DEF OUTPUT PARAM ocReturn            AS CHAR NO-UNDO.
DEF OUTPUT PARAM oiRequestId         AS INT  NO-UNDO.

DEF VAR bReqTypeChecked AS LOG NO-UNDO.

FIND FIRST JBoxExternalPartner NO-LOCK
     WHERE JBoxExternalPartner.cExternalPartnerName = icPartnerName
     NO-ERROR.

FIND FIRST JBoxExternalRequestType NO-LOCK
     WHERE JBoxExternalRequestType.cProgram = icRequestType
     NO-ERROR.

IF AVAIL JBoxExternalRequestType 
   AND AVAIL JBoxExternalPartner THEN DO:
       
  FIND FIRST JBoxExternalPartnerRequestType NO-LOCK
       WHERE JBoxExternalPartnerRequestType.iJBoxExternalPartnerId     = JBoxExternalPartner.iJBoxExternalPartnerId
         AND JBoxExternalPartnerRequestType.iJBoxExternalRequestTypeId = JBoxExternalRequestType.iJBoxExternalRequestTypeId
       NO-ERROR.
  IF NOT AVAIL JBoxExternalPartnerRequestType THEN
    ASSIGN obOk             = NO 
           bReqTypeChecked  = YES
           ocReturn         = "Invalid request " + JBoxExternalRequestType.cExternalRequestType.
  ELSE IF SEARCH(JBoxExternalRequestType.cRequestTypePassPhrase) NE ? THEN DO:
    RUN VALUE(JBoxExternalRequestType.cRequestTypePassPhrase) (
        JBoxExternalPartner.cExternalPartnerName,icRequestPassPhrase,OUTPUT obOk,OUTPUT ocReturn).
    bReqTypeChecked = YES.
  END.
END.

IF NOT bReqTypeChecked THEN DO:
  IF AVAIL JBoxExternalPartner THEN DO:
    IF SEARCH(JBoxExternalPartner.cExtPartnerPassPhrase) NE ? THEN
      RUN VALUE(JBoxExternalPartner.cExtPartnerPassPhrase) (
          JBoxExternalPartner.cExternalPartnerName,icRequestPassPhrase,OUTPUT obOk,OUTPUT ocReturn).
    ELSE IF JBoxExternalPartner.cExtPartnerPassPhrase NE "none" THEN
      RUN jbservice_validate_external_request.p (
          JBoxExternalPartner.cExternalPartnerName,icRequestPassPhrase,OUTPUT obOk,OUTPUT ocReturn).
  END.
  ELSE
    ASSIGN obOk       = NO 
           ocReturn   = "Invalid partnerId".
END.

CREATE JBoxExternalRequest.
ASSIGN JBoxExternalRequest.cRequestInputParam   = icRequestParam
       JBoxExternalRequest.cPassPhrase          = icRequestPassPhrase
       JBoxExternalRequest.dtRequest            = NOW
       JBoxExternalRequest.bRequestOk           = obOk
       .

oiRequestId = JBoxExternalRequest.iJBoxExternalRequestId.

IF AVAIL JBoxExternalRequestType THEN
  JBoxExternalRequest.iJBoxExternalRequestTypeId = JBoxExternalRequestType.iJBoxExternalRequestTypeId.

IF AVAIL JBoxExternalPartner THEN
  JBoxExternalRequest.iJBoxExternalPartnerId = JBoxExternalPartner.iJBoxExternalPartnerId.

