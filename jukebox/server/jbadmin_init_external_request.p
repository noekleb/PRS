/* jbadmin_init_external_request.p 
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

FIND FIRST JBoxExternalPartner NO-LOCK
     WHERE JBoxExternalPartner.cExternalPartnerName = icPartnerName
     NO-ERROR.

FIND FIRST JBoxExternalRequestType NO-LOCK
     WHERE JBoxExternalRequestType.cExternalRequestType = icRequestType
     NO-ERROR.

RUN jbadmin_validate_external_request.p (icPartnerName,icRequestPassPhrase,OUTPUT obOk).

CREATE JBoxExternalRequest.
ASSIGN JBoxExternalRequest.cRequestInputParam   = icRequestParam
       JBoxExternalRequest.dtRequest            = NOW
       JBoxExternalRequest.bRequestOk           = obOk
       .

oiRequestId = JBoxExternalRequest.iJBoxExternalRequestId.

IF AVAIL JBoxExternalRequestType THEN
  JBoxExternalRequest.iJBoxExternalRequestTypeId = JBoxExternalRequestType.iJBoxExternalRequestTypeId.

IF AVAIL JBoxExternalPartner THEN
  JBoxExternalRequest.iJBoxExternalPartnerId = JBoxExternalPartner.iJBoxExternalPartnerId.

