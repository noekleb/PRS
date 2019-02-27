 
 /*------------------------------------------------------------------------
    File        : JBoxEmail
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : goo
    Created     : Wed Aug 20 08:32:54 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS JBoxSendMail: 
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

  DEF TEMP-TABLE ttSendMail no-undo
    FIELD iSendMailId     AS int
    FIELD cSendTo         AS CHARACTER
    FIELD cSendCC         AS CHARACTER
    FIELD cSendBCC        AS CHARACTER
    FIELD cSendSubject    AS CHARACTER
    field cSendBody       as char 
    index iSendMailId is primary unique iSendMailId
    .

  def temp-table ttEvent no-undo
    FIELD iSendMailId     AS int
    FIELD iEventId        AS int
    field RowIdent1       as char
    FIELD cEventType      AS CHARACTER
    FIELD cEventText      AS CHARACTER
    FIELD cSourceTable    AS CHARACTER
    FIELD cKeyFieldList   AS CHARACTER
    index iSendMailId is primary unique iSendMailId iEventId
    .

  def temp-table ttAttachment no-undo
    FIELD iSendMailId     AS int
    field iAttachmentId   as int 
    FIELD cAttachmentFile  AS CHARACTER
    index iSendMailId is primary unique iSendMailId iAttachmentId
    index cAttachmentFile is unique cAttachmentFile
    .
      
  define public property SMTPServer        as char get. set.
  define public property AccountUser       as char get. set.
  define public property AccountPwd        as char get. set.
  define public property MailReplyTo       as char get. set.
  define public property MailSender        as char get. set.
  define public property MailArchive       as char get. set.
  define public property AutoSend          as char get. set.
  define public property MailAttachment    as char get. set.
  define public property MailViewLog       as logical get. set.
  define public property MailEventId       as char get. set.
  define public property MailEventStatus   as logical get. set.
  
  
  
  define public property ttSendMailHdl     as handle  
    get():
      return temp-table ttSendMail:handle.
    end get. private set.
  define public property ttEventHdl as handle  
    get():
      return temp-table ttEvent:handle.
    end get. private set.

  def var cTmpValue as char no-undo. /*Used for testing of returnvalues */    
		
	CONSTRUCTOR PUBLIC JBoxSendMail ():
		SUPER ().
    getSMTPserver().
    getAccountUser().
    getMailReplyTo().
    getMailSender().
        	  
	END CONSTRUCTOR.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC VOID addAttachment(input icFileName as char):
	  def var ii as int no-undo.
	  
		find last ttSendMail no-lock no-error.
		if not avail ttSendMail then 
		MESSAGE 
		  'Ingen mail tilgjengelig, må først legge inn en mail før vedlegg kan legges inn.'		
    VIEW-AS ALERT-BOX.
    
    find first ttAttachment where ttAttachment.cAttachmentFile = icFileName no-lock no-error.
    if avail ttAttachment then return.
     
    if search(icFileName) = ? then MESSAGE 'Finner ikke filen ' + icFileName
                                   VIEW-AS ALERT-BOX.
    find last ttAttachment no-lock no-error.
    if not avail ttAttachment then ii = 1. else ii = ttAttachment.iAttachmentId + 1.
                                      
    create ttAttachment.
    assign 
      ttAttachment.iSendMailId     = ttSendMail.iSendMailId 
      ttAttachment.iAttachmentId   = ii
      ttAttachment.cAttachmentFile = icFileName
    .
    
		RETURN.

	END METHOD.


	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC VOID addEvent( input icEventType as char, input icEventText as char, input icSourceTable as char, input icRowIdent1 as char, input  icKeyFieldList as char ):
		
    def var ii as int no-undo.
    
    find last ttSendMail no-lock no-error.
    if not avail ttSendMail then 
    MESSAGE 
      'Ingen mail tilgjengelig, må først legge inn en mail før vedlegg kan legges inn.'   
    VIEW-AS ALERT-BOX.
    
    find last ttEvent no-lock no-error.
    if not avail ttEvent then ii = 1. else ii = ttEvent.iEventId + 1.
                                      
    create ttEvent.
    assign 
      ttEvent.iSendMailId   = ttSendMail.iSendMailId 
      ttEvent.iEventId      = ii
      ttEvent.cEventType    = icEventType
      ttEvent.cEventText    = icEventText
      ttEvent.cSourceTable  = icSourceTable
      ttEvent.RowIdent1     = icRowIdent1
      ttEvent.cKeyFieldList = icKeyFieldList
      
    .
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose: 
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC VOID addSendMail(input icTo as char, input icCC as char, input icBCC as char, input icSubject as char, input icBody as char ):
		def var ii as int no-undo.
		
		find last ttSendMail no-lock no-error.
		if not avail ttSendMail then ii = 1. else ii = ttSendMail.iSendMailId + 1.
		create ttSendMail.
		assign 
		  ttSendMail.iSendMailId  = ii
		  ttSendMail.cSendTo      = icTo
		  ttSendMail.cSendCC      = icCC
		  ttSendMail.cSendBCC     = icBCC
		  ttSendMail.cSendSubject = icSubject
		  ttSendMail.cSendBody    = icBody
		.		
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PRIVATE VOID getAccountPwd(  ):
		cTmpValue = DYNAMIC-FUNCTION("getFieldValues","JBoxSysParam",
                                 "WHERE bActive AND cSysParamName = 'CompanyMailUser' AND iJBoxCompanyId = " + DYNAMIC-FUNCTION("getCompany"),
                                 "cSysParamCharValue").
    if cTmpValue = ? or cTmpValue = '' then 
    do: 
      cTmpValue = DYNAMIC-FUNCTION("getFieldValues","JBoxSysParam","WHERE bActive AND cSysParamName = 'CompanyMailPwd' AND iJBoxCompanyId = " + DYNAMIC-FUNCTION("getCompany"),"cSysParamCharValue").
      AccountPwd = if cTmpValue = ? then '' else cTmpValue.
    end.   
                              
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD private VOID getAccountUser(  ):
    cTmpValue = DYNAMIC-FUNCTION("getFieldValues","JBoxGenCode","WHERE iJBoxCompanyId = " + DYNAMIC-FUNCTION("getCompany")
                                                                  + " AND cCodeType = 'EmailUserName'"
                                                                  + " AND cDescription = '" + DYNAMIC-FUNCTION("getASuserId") + "'","cCodeValue").
    if cTmpValue = ? or cTmpValue = '' then
    do:
      cTmpValue = DYNAMIC-FUNCTION("getFieldValues","JBoxSysParam","WHERE bActive AND cSysParamName = 'CompanyMailUser' AND iJBoxCompanyId = " + DYNAMIC-FUNCTION("getCompany"),"cSysParamCharValue").
    end.
    AccountUser = if cTmpValue = ? then '' else cTmpValue.                                                                  
	
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC VOID getAttachment(  ):
		for each ttAttachment:
		  MailAttachment = if MailAttachment ne '' then ',' + ttAttachment.cAttachmentFile else ttAttachment.cAttachmentFile. 
		end.
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC VOID getContext(  ):
		
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose: Har ikke tatt forbehold om flere en en event i forhold til status og id
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC VOID getEvent(  ):
	  for each ttEvent:
  		IF NOT DYNAMIC-FUNCTION("runProc","jbadmin_make_eventlog.p",ttEvent.cEventType + "|" + "|||||" + ttEvent.cKeyFieldList + "|" + ttEvent.cEventText,buffer ttEvent:handle) THEN 
  		DO:
        MailEventStatus = DYNAMIC-FUNCTION("getTransactionMessage").
  		end.
  		MailEventId = DYNAMIC-FUNCTION("getTransactionMessage").
    end.
    RETURN.
 
	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PRIVATE VOID getMailReplyTo(  ):
    cTmpValue   = DYNAMIC-FUNCTION("getFieldValues","JBoxSysParam","WHERE bActive AND cSysParamName = 'CompanyReplyToEmail' AND iJBoxCompanyId = " + DYNAMIC-FUNCTION("getCompany"),"cSysParamCharValue").		
    MailReplyTo = if cTmpValue = ? then '' else cTmpValue.     
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PRIVATE VOID getMailSender(  ):
    cTmpValue = DYNAMIC-FUNCTION("getFieldValues","JBoxSysParam","WHERE bActive AND cSysParamName = 'CompanySendFromEmail' AND iJBoxCompanyId = " + DYNAMIC-FUNCTION("getCompany"),"cSysParamCharValue").
    MailSender = if cTmpValue = ? then '' else cTmpValue. 		
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PRIVATE VOID getSMTPserver(  ):
	  def var cInfo as char no-undo.
	  
    SMTPserver = DYNAMIC-FUNCTION("getFieldValues","JBoxSysParam","WHERE bActive AND cSysParamName = 'CompanyMailServer' AND iJBoxCompanyId = " + DYNAMIC-FUNCTION("getCompany"),"cSysParamCharValue").		
    if SMTPserver = ? or SMTPserver = "" then cInfo = DYNAMIC-FUNCTION("getFieldValues","JBoxEmailAccount","WHERE bActive AND cAccountName = 'SMTP mail'(Codemaster)","cMailServer,cUserName,cPassword").
    if cInfo = '' or cInfo = ? then writeLOG('Mail','SMTPserver is either not set, or blank. Check jboxSysParam:CompanyMailServer or jboxEmailAccount:SMTP Mail (Codemaster). Sjekk for firma:' + string(DYNAMIC-FUNCTION("getCompany")) ).
    else
      do:
        assign 
          SMTPserver = entry(1,cInfo,'|')
        .
        if num-entries(cInfo,'|') ge 3 then 
          assign 
            AccountUser = entry(2,cInfo,'|')
            AccountPwd  = entry(3,cInfo,'|')
          .
      end.
    
		RETURN.
  CATCH myError AS Progress.Lang.Error :
  		MESSAGE 'Feil:' myError:GetMessage(1)
      VIEW-AS ALERT-BOX.
  END CATCH.
	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC VOID writeLOG(input icLogType as char, input icLogMessage as char  ):
	  MESSAGE icLogType skip icLogMessage
    VIEW-AS ALERT-BOX.
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC VOID SendMail(  ):
		
		RETURN.

	END METHOD.

	DESTRUCTOR PUBLIC JBoxSendMail ( ):

	END DESTRUCTOR.

END CLASS.