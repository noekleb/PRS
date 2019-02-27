/* Update link from ExternalParter to RequestTypes
   Parameters: <partner id>;<list of request types>
         
   Created:  26.11.12 by Brynjar Hasle  
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR ix            AS INT   NO-UNDO.
DEF VAR iPartnerId    AS INT   NO-UNDO.
DEF VAR cRequestTypes AS CHAR  NO-UNDO.
DEF VAR cUserId       AS CHAR  NO-UNDO.

ASSIGN iPartnerId    = INT(ENTRY(1,icParam,";"))
       cRequestTypes = ENTRY(2,icParam,";")
       cUserId       = DYNAMIC-FUNCTION("getAsUserId" IN SOURCE-PROCEDURE)
       .

DO TRANSACTION:
  FOR EACH JBoxExternalPartnerRequestType EXCLUSIVE-LOCK
      WHERE JBoxExternalPartnerRequestType.iJBoxExternalPartnerId = iPartnerId:
    IF LOOKUP(STRING(JBoxExternalPartnerRequestType.iJBoxExternalRequestTypeId),cRequestTypes,"|") = 0 THEN
      DELETE JBoxExternalPartnerRequestType.
  END.
  DO ix = 1 TO NUM-ENTRIES(cRequestTypes,"|"):
    FIND FIRST JBoxExternalRequestType 
         WHERE JBoxExternalRequestType.iJBoxExternalRequestTypeId = INT(ENTRY(ix,cRequestTypes,"|"))
         NO-LOCK NO-ERROR.
    IF AVAIL JBoxExternalRequestType AND 
       NOT CAN-FIND(FIRST JBoxExternalPartnerRequestType
                    WHERE JBoxExternalPartnerRequestType.iJBoxExternalPartnerId     = iPartnerId
                      AND JBoxExternalPartnerRequestType.iJBoxExternalRequestTypeId = JBoxExternalRequestType.iJBoxExternalRequestTypeId) 
       THEN DO:
      CREATE JBoxExternalPartnerRequestType.
      ASSIGN JBoxExternalPartnerRequestType.iJBoxExternalPartnerId = iPartnerId
             JBoxExternalPartnerRequestType.iJBoxExternalRequestTypeId = JBoxExternalRequestType.iJBoxExternalRequestTypeId
             JBoxExternalPartnerRequestType.dCreated        = TODAY
             JBoxExternalPartnerRequestType.cCreatedBy      = cUserId 
             .
    END.
  END.
END.

obOk = ocReturn = "".

