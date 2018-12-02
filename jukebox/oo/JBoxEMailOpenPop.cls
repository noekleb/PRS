/*------------------------------------------------------------------------
    File        : JBoxEMailOpenPop
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : brynjar
    Created     : Sun Oct 30 20:30:59 CET 2011
    Notes       : 
--------------------------------------------------------------------------*/
USING System.*.
USING System.Collections.Generic.*.
USING System.IO.*.
USING Progress.Lang.*.
USING OpenPop.*.

CLASS JBoxEMailOpenPop: 
	
  DEFINE VARIABLE pop3Client1     AS CLASS OpenPop.Pop3.Pop3Client  NO-UNDO.
  DEFINE VARIABLE pop3Message     AS CLASS OpenPop.Mime.Message     NO-UNDO.
  DEFINE VARIABLE pop3MessagePart AS CLASS OpenPop.Mime.MessagePart NO-UNDO.
  DEFINE VARIABLE pop3MsgPartList AS CLASS "List<OpenPop.Mime.MessagePart>" NO-UNDO.
  DEFINE VARIABLE pop3MessageHdr  AS CLASS OpenPop.Mime.Header.MessageHeader NO-UNDO.
  DEFINE VARIABLE pop3AttachList  AS CLASS "List<OpenPop.Mime.MessagePart>" NO-UNDO.
  DEFINE VARIABLE pop3AddressList AS CLASS "List<OpenPop.Mime.Header.RfcMailAddress>" NO-UNDO.
  DEFINE VARIABLE pop3Address     AS CLASS OpenPop.Mime.Header.RfcMailAddress NO-UNDO.
  DEFINE VARIABLE FileInfo        AS CLASS System.IO.FileInfo NO-UNDO.
    
  DEFINE VARIABLE cAttachDir      AS CHAR NO-UNDO.
		
  DEFINE TEMP-TABLE ttMessages NO-UNDO
         FIELD cFrom         AS CHAR
         FIELD cDisplayFrom  AS CHAR
         FIELD cTo           AS CHAR
         FIELD cDisplayTo    AS CHAR
         FIELD cCc           AS CHAR
         FIELD cDisplayCc    AS CHAR
         FIELD cBcc          AS CHAR
         FIELD cDisplayBcc   AS CHAR
         FIELD cSubject      AS CHAR
         FIELD dtDate        AS DATETIME
         FIELD cPlainText    AS CHAR
         FIELD blBody        AS BLOB
         FIELD iAttachCount  AS INT
         FIELD cBodyFileName AS CHAR   
         FIELD cAttachments  AS CHAR
         FIELD cAttachFiles  AS CHAR
         FIELD iMsgType      AS INTEGER /* 1: Text, 2: HTML */
         FIELD iMsgId        AS INTEGER
         .

  DEF VAR hBuffTTMessages AS HANDLE NO-UNDO.

	CONSTRUCTOR PUBLIC JBoxEMailOpenPop ():
		
    SUPER().
    InitializeComponent().
                
    CATCH e AS Progress.Lang.Error:
        UNDO, THROW e.
    END CATCH.

	END CONSTRUCTOR.

	METHOD PRIVATE VOID InitializeComponent(  ):
    pop3Client1 = NEW OpenPop.Pop3.Pop3Client().

    CATCH e AS Progress.Lang.Error:
        UNDO, THROW e.
    END CATCH.
	END METHOD.

  METHOD PUBLIC LOGICAL setEmailServerProperties
        (icHost      AS CHAR,
         icUserName  AS CHAR,
         icPassword  AS CHAR,
         iiPort      AS INT,
         icAttachDir AS CHAR,
         icProxyHost AS CHAR,
         icProxyUser AS CHAR,
         icProxyPwd  AS CHAR,
         iiProxyPort AS INT):

    IF icAttachDir = "" THEN icAttachDir = SESSION:TEMP-DIRECTORY.
    IF icProxyUser = "" AND SESSION:PROXY-USERID NE ?   THEN icProxyUser = SESSION:PROXY-USERID.
    IF icProxyPwd  = "" AND SESSION:PROXY-PASSWORD NE ? THEN icProxyPwd  = SESSION:PROXY-PASSWORD.

    pop3Client1:Connect(icHost,iiPort,NO) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO.

    pop3Client1:Authenticate(icUserName,icPassword) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO.

    cAttachDir = RIGHT-TRIM(icAttachDir,"\") + "\".

    RETURN pop3Client1:Connected.
  END METHOD.

  METHOD PUBLIC INTEGER getEmailMessageCount():
    DEF VAR iMsgCnt AS INT NO-UNDO.
    iMsgCnt = Pop3Client1:GetMessageCount() NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN
      RETURN iMsgCnt.
    ELSE
      RETURN 0.
/*     RETURN INTEGER(Pop3Client1:GetMessageCount()). */
  END METHOD.

  METHOD PUBLIC HANDLE getEmailMessages():

    DEFINE VARIABLE ix           AS INT    NO-UNDO.
    DEFINE VARIABLE iCount       AS INT    NO-UNDO.
    DEFINE VARIABLE iMsgCount    AS INT    NO-UNDO.
    DEFINE VARIABLE iMsgType     AS INT    NO-UNDO.
    DEFINE VARIABLE cFileName    AS CHAR   NO-UNDO.
    DEFINE VARIABLE cBody        AS CHAR   NO-UNDO.
    DEFINE VARIABLE cPopFileName AS CHAR   NO-UNDO.
  
    hBuffTTMessages = BUFFER ttMessages:HANDLE.
  
    EMPTY TEMP-TABLE ttMessages.

    iMsgCount = Pop3Client1:GetMessageCount() NO-ERROR.

    IF NOT ERROR-STATUS:ERROR THEN
      DO iCount = 1 TO iMsgCount:
        pop3Message = pop3Client1:GetMessage (iCount) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN NEXT.

        pop3MessageHdr = Pop3Client1:GetMessageHeaders(iCount). 
        pop3MessagePart = pop3Message:FindFirstHtmlVersion().
        IF pop3MessagePart = ? THEN DO:
          iMsgType = 1.
          pop3MessagePart = pop3Message:FindFirstPlainTextVersion().
        END.
        ELSE iMsgType = 2.
  
        cBody = pop3MessagePart:GetBodyAsText() NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
          iMsgType = 1.
          pop3MessagePart = pop3Message:FindFirstPlainTextVersion().
        END.
  
        pop3AttachList = pop3Message:FindAllAttachments().
  
        CREATE ttMessages.
        ASSIGN ttMessages.cSubject      = pop3MessageHdr:Subject
               ttMessages.cFrom         = pop3MessageHdr:From:Address
               ttMessages.cDisplayFrom  = pop3MessageHdr:From:DisplayName
               ttMessages.dtDate        = pop3MessageHdr:DateSent
               ttMessages.iMsgType      = iMsgType
               ttMessages.cPlainText    = (IF pop3MessagePart NE ? THEN pop3MessagePart:GetBodyAsText() ELSE "")
               ttMessages.iAttachCount  = pop3AttachList:Count + 1 
               ttMessages.iMsgId        = iCount   
               ttMessages.cBodyFileName = cAttachDir + ttMessages.cFrom + "_" + STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") + "_" + STRING(TIME) + (IF iMsgType = 1 THEN ".txt" ELSE ".htm")
               .
        
        FileInfo = NEW System.IO.FileInfo(ttMessages.cBodyFileName).
        IF pop3MessagePart NE ? THEN
          pop3MessagePart:Save(FileInfo).                                                                                                                                                                                                                            
  
        COPY-LOB FROM FILE ttMessages.cBodyFileName TO ttMessages.blBody NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
          ttMessages.iMsgType = 1.
    
        pop3AddressList = pop3MessageHdr:To.
        DO ix = 0 TO pop3AddressList:Count - 1:
          pop3Address = pop3AddressList:Item[ix].
          ASSIGN ttMessages.cTo = ttMessages.cTo + (IF ttMessages.cTo NE "" THEN ";" ELSE "") + pop3Address:Address
                 ttMessages.cDisplayTo = ttMessages.cDisplayTo + (IF ttMessages.cDisplayTo NE "" THEN ";" ELSE "") + pop3Address:DisplayName
                 .
        END.       
  
        pop3AddressList = pop3MessageHdr:Cc.
        DO ix = 0 TO pop3AddressList:Count - 1:
          pop3Address = pop3AddressList:Item[ix].
          ASSIGN ttMessages.cCc = ttMessages.cCc + (IF ttMessages.cCc NE "" THEN ";" ELSE "") + pop3Address:Address
                 ttMessages.cDisplayCc = ttMessages.cDisplayCc + (IF ttMessages.cDisplayCc NE "" THEN ";" ELSE "") + pop3Address:DisplayName
                 .
        END.       
  
        DO ix = 0 TO pop3AttachList:Count - 1:
          cPopFileName = pop3AttachList[ix]:FileName.
          cPopFileName = REPLACE(cPopFileName,"?","").
          cPopFileName = REPLACE(cPopFileName,":","").
          cPopFileName = REPLACE(cPopFileName,"/","").
          cPopFileName = REPLACE(cPopFileName,"\","").
          cPopFileName = REPLACE(cPopFileName,"*","").
          cPopFileName = REPLACE(cPopFileName,"<","").
          cPopFileName = REPLACE(cPopFileName,">","").
          cPopFileName = REPLACE(cPopFileName,"|","").
          IF NUM-ENTRIES(cPopFileName,".") < 2 THEN cPopFileName = STRING(ix) +  ".jpg".
          ASSIGN cFileName = cAttachDir + ttMessages.cFrom + "_" + STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") + "_" + STRING(TIME) + "_" + REPLACE(cPopFileName," ","_")
                 ttMessages.cAttachFiles = ttMessages.cAttachFiles + (IF ttMessages.cAttachFiles NE "" THEN ";" ELSE "") + cFileName 
                 ttMessages.cAttachments = ttMessages.cAttachments + (IF ttMessages.cAttachments NE "" THEN ";" ELSE "") + cPopFileName
                 .
          FileInfo = NEW System.IO.FileInfo(cFileName).
          pop3AttachList:Item[ix]:Save(FileInfo).
        END.
      END.
  
    RETURN hBuffTTMessages.
  END METHOD.
      
  METHOD PUBLIC LOGICAL DeleteMsg (INPUT iiMsgId AS INT):
    pop3Client1:DeleteMessage(iiMsgId).
    RETURN YES.
  END METHOD.

  METHOD PUBLIC LOGICAL SignOff ():
    DEF VAR bOK AS LOG NO-UNDO.
    bOK = pop3Client1:Connected NO-ERROR.
    IF bOk THEN 
/*     IF pop3Client1:Connected THEN */
      pop3Client1:Disconnect() NO-ERROR.      
    RETURN YES.
  END METHOD.

	DESTRUCTOR PUBLIC JBoxEMailOpenPop ( ):

/*     IF VALID-OBJECT(components) THEN DO:              */
/*       CAST(components, System.IDisposable):Dispose(). */
/*     END.                                              */

	END DESTRUCTOR.

END CLASS.
