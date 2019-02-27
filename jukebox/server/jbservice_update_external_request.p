/* jbadmin_init_external_request.p 
   Parameters partner name, request type, request parameter, pass.phrase (opt)
   Returns requestid for completion.
   
   Created  19.11.12 by brynjar@chemistry.no
-------------------------------------------------------------------------*/   
DEF INPUT  PARAM iiRequestId         AS INT     NO-UNDO.
DEF INPUT  PARAM ibResultOk          AS LOG     NO-UNDO.
DEF INPUT  PARAM icRequestReturn     AS CHAR    NO-UNDO.
DEF INPUT  PARAM ihDataSetXSD        AS HANDLE  NO-UNDO.
DEF INPUT  PARAM ihDataSetXML        AS HANDLE  NO-UNDO.


FIND FIRST JBoxExternalRequest
     WHERE JBoxExternalRequest.iJBoxExternalRequestId = iiRequestId
     EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
IF AVAIL JBoxExternalRequest THEN
  ASSIGN JBoxExternalRequest.bResultOk       = ibResultOk
         JBoxExternalRequest.cRequestResult  = icRequestReturn
         .
ELSE MESSAGE "Finner ikke request " iiRequestId.

