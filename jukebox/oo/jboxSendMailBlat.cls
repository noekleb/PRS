 
 /*------------------------------------------------------------------------
    File        : jboxMailBlat
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : goo
    Created     : Wed Aug 20 21:24:38 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS jboxSendMailBlat inherits jboxSendMail: 
  define stream stFile.
  
  define public property BlatToFile         as char get. set.
  define public property BlatCCFile         as char get. set.
  define public property BlatBCCFile        as char get. set.
  define public property BlatSubjectFile    as char get. set.
  define public property BlatBodyFile       as char get. set.
  define public property BlatLogFile        as char get. set.
  define public property BlatBATFile        as char get. set.
  define public property BlatStatus         as char get. set.
  define public property BlatOSerror        as int  get. set.
  define public property BlatExeFile        as char 
  get():
     FILE-INFO:FILE-NAME = SEARCH("blat.exe").
     return file-info:full-pathname. 
  end get.
  private set.
  
  def var  qh as handle no-undo.
  def var  bh as handle no-undo.
  def var  iiAttempt as int    no-undo.
    
  DEF TEMP-TABLE ttBlatLog 
    FIELD iLine    AS INT
    FIELD cLogText AS CHAR
    .
    
  constructor public jboxSendMailBlat():
    super ().    
    assign 
      BlatToFile      = SESSION:TEMP-DIR + "mailTO.txt"
      BlatCCFile      = SESSION:TEMP-DIR + "mailCC.txt"
      BlatBCCFile     = SESSION:TEMP-DIR + "mailBCC.txt"
      BlatSubjectFile = SESSION:TEMP-DIR + "mailSubject.txt"
      BlatBodyFile    = SESSION:TEMP-DIR + "mailBody.txt"
      BlatLogFile     = SESSION:TEMP-DIR + "mailLOG.txt"
      BlatBATFile     = SESSION:TEMP-DIR + "blatmail.bat"
    .
    bh = ttSendMailHdl:DEFAULT-BUFFER-HANDLE.
    create query qh.
    qh:add-buffer(bh).
    
  end constructor.
  

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC VOID processBLATLOG(  ):

    DEF VAR cLogToday    AS CHAR NO-UNDO.
    DEF VAR cLogLine     AS CHAR NO-UNDO.
    DEF VAR ix           AS INT  NO-UNDO.
    DEF VAR bStartLogImp AS LOG  NO-UNDO.
    DEF VAR bStartDelLog AS LOG  NO-UNDO.
    DEF VAR bBlatError   AS LOG  NO-UNDO.
    DEF VAR bTimeOut     AS LOG  NO-UNDO.
    DEF VAR cReturn      AS CHAR NO-UNDO.
    
    IF SEARCH(BlatLogFile) = ? THEN 
    do:
      BlatStatus = 'Could not find logfile: ' + BlatLogFile.
      RETURN.
    end.
    
    ASSIGN cLogToday = STRING(YEAR(TODAY)) + "." + STRING(MONTH(TODAY),"99") + "." + STRING(DAY(TODAY),"99")
           ix        = 0.
    INPUT FROM VALUE(BlatLogFile).
    REPEAT:
      IMPORT UNFORMATTED cLogLine.
      IF cLogLine BEGINS cLogToday THEN bStartLogImp = YES.
      IF bStartLogImp THEN DO:
        CREATE ttBlatLog.
        ASSIGN ix                 = ix + 1
               ttBlatLog.cLogText = cLogLine
               ttBlatLog.iLine    = ix.
      END.
    END.
    INPUT CLOSE.
    
    FOR EACH ttBlatLog BY ttBlatLog.iLine DESC:
      IF bStartDelLog OR ttBlatLog.cLogText = "" THEN DELETE ttBlatLog.
      ELSE IF ttBlatLog.cLogText MATCHES "*Start of Session*" THEN bStartDelLog = YES.
    END.
    
    FOR EACH ttBlatLog BY ttBlatLog.iLine:
      IF (NOT ttBlatLog.cLogText BEGINS cLogToday 
         AND NOT ttBlatLog.cLogText BEGINS "Blat" 
         AND NOT ttBlatLog.cLogText BEGINS "32-bit" 
         AND NOT ttBlatLog.cLogText BEGINS "64-bit") OR
         ttBlatLog.cLogText MATCHES "*): Error:*" 
         THEN
        bBlatError = YES.
      IF ttBlatLog.cLogText MATCHES "*timed out*" THEN
        bTimeOut = YES.
      DISP ttBlatLog.iLine
           ttBlatLog.cLogText FORMAT "x(150)"
           WITH WIDTH 200.
    END.
    
    IF bBlatError THEN
      FOR EACH ttBlatLog BY ttBlatLog.iLine:
        cReturn = cReturn + ttBlatLog.cLogText + CHR(10).
      END.
      
    IF bTimeOut AND iiAttempt = 1 THEN 
    do:
      BlatStatus = "retry".
      return.
    end.
    BlatStatus = cReturn.
    
		RETURN.

	END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:Read the produced mail sending logfile
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PRIVATE VOID readMailLOG(  ):
    
    if MailViewLog then 
    IF SEARCH(blatLogFile) NE ? THEN
      OS-COMMAND NO-WAIT notepad VALUE(blatLogFile).
      
    RETURN.

  END METHOD.


	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD override PUBLIC VOID SendMail(  ):
	  qh:query-prepare('FOR EACH ' + bh:name).
	  qh:query-open().
    qh:GET-FIRST ().
	  do while bh:avail:
      writeReceivers().
      writeBody().
      writeSubject().
      getAttachment().
      writeBAT().
      qh:get-next().
    end.
		
		SESSION:SET-WAIT-STATE("General").
    OS-COMMAND SILENT VALUE(blatBATFile).
    BlatOSerror = OS-ERROR.  
    SESSION:SET-WAIT-STATE("").
		
		readMailLOG().
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC VOID writeBAT(  ):
		
  OUTPUT STREAM stFile TO VALUE(blatBATfile).
    PUT STREAM stFile UNFORMATTED
        blatExeFile + " " + quoter(blatBodyFile) + " " 
      + "-server " + SMTPserver + " "      
      + "-f " + (IF AccountUser NE "" THEN AccountUser ELSE MailSender) + " "
      + (IF AccountPwd  NE "" THEN "-u " + AccountUser + " -pw " + AccountPwd + " " ELSE "")
      + (IF MailSender  NE "" THEN "-from " + MailSender + " " ELSE "")
      + (IF MailReplyTo NE "" THEN "-replyto " + MailReplyTo + " " ELSE "")      
      + "-log " + quoter(blatLogFile) + " -timestamp "
       
      + (IF bh:buffer-field('cSendTo'):buffer-value ne  "" then "-tf " + blatToFile  + " " ELSE "")
      
      + (IF bh:buffer-field('cSendCC'):buffer-value ne "" then "-cf " + blatCCFile  + " " ELSE "")
      + (IF bh:buffer-field('cSendBCC'):buffer-value ne "" then "-bf " + blatBCCFile + " " ELSE "")
      + (IF bh:buffer-field('cSendCC'):buffer-value = "" AND bh:buffer-field('cSendTo'):buffer-value = "" THEN "-ur " ELSE "")
      + (IF bh:buffer-field('cSendSubject'):buffer-value NE "" THEN "-sf " + blatSubjectFile + " " ELSE "")
      + (IF MailAttachment NE "" THEN "-attach " + MailAttachment ELSE "")
      .
    OUTPUT STREAM stFile CLOSE.
      
    RETURN.

    CATCH anyError AS Progress.Lang.Error :
        MESSAGE anyError:GetMessage(1)
        VIEW-AS ALERT-BOX information.
        OUTPUT STREAM stFile CLOSE.
        
    END CATCH.

	END METHOD.

  /*------------------------------------------------------------------------------
   Purpose: Produce the file for mailBody
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PRIVATE VOID writeBody(  ):
    output stream stFile to value(blatBodyFile).
    PUT STREAM stFile  UNFORMATTED bh:BUFFER-FIELD("cSendBody"):BUFFER-VALUE.
    output stream stFile close.
    RETURN.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose: Produce the files for TO, CC and BCC 
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PRIVATE VOID writeReceivers(  ):
    output stream stFile to value(blatToFile).
    PUT STREAM stFile  UNFORMATTED bh:BUFFER-FIELD("cSendTo"):BUFFER-VALUE.
    output stream stFile close.

    output stream stFile to value(blatCCFile).
    PUT STREAM stFile  UNFORMATTED bh:BUFFER-FIELD("cSendCC"):BUFFER-VALUE.
    output stream stFile close.
    
    output stream stFile to value(blatBCCFile).
    PUT STREAM stFile  UNFORMATTED bh:BUFFER-FIELD("cSendBCC"):BUFFER-VALUE.
    output stream stFile close.
    
    RETURN.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose: Produce the file for mailSubject
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PRIVATE VOID writeSubject(  ):
    output stream stFile to value(blatSubjectFile).
    PUT STREAM stFile  UNFORMATTED bh:BUFFER-FIELD("cSendSubject"):BUFFER-VALUE.
    output stream stFile close.
    
    RETURN.

  END METHOD.
  
END CLASS.