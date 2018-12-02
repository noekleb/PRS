/*------------------------------------------------------------------------
    File        : JBoxEMailDotNet
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : brynjar
    Created     : Sun Oct 30 20:30:59 CET 2011
    Notes       : 
--------------------------------------------------------------------------*/

USING Progress.Lang.*.
/* USING Progress.Windows.UserControl. */

USING Email.NET.Common.Configurations.*.
USING Email.NET.POP3.*.
USING Email.NET.Common.*.
USING Email.NET.Common.Collections.*.


CLASS JBoxEMailDotNet: 
	
  DEFINE VARIABLE pop3Client1  AS Email.Net.Pop3.Pop3Client NO-UNDO.
  DEFINE VARIABLE pop3Message  AS Email.Net.Common.Rfc822Message NO-UNDO.
  DEFINE VARIABLE pop3MsgStat  AS Email.Net.Pop3.Pop3MessageStatistics NO-UNDO.
  DEFINE VARIABLE pop3Uid      AS Email.Net.Pop3.Pop3MessageUIDInfo NO-UNDO.
  DEFINE VARIABLE pop3Response AS Email.Net.Pop3.Pop3Response NO-UNDO.
    
		
  DEFINE TEMP-TABLE ttMessages NO-UNDO
         FIELD cFrom        AS CHAR
         FIELD cDisplayFrom AS CHAR
         FIELD cTo          AS CHAR
         FIELD cDisplayTo   AS CHAR
         FIELD cCc          AS CHAR
         FIELD cDisplayCc   AS CHAR
         FIELD cBcc         AS CHAR
         FIELD cDisplayBcc  AS CHAR
         FIELD cSubject     AS CHAR
         FIELD dtDate       AS DATETIME
         FIELD cPlainText   AS CHAR
         FIELD blBody       AS BLOB
         FIELD iAttachCount AS INT
         FIELD cAttachments AS CHAR
         FIELD cAttachFiles AS CHAR
         FIELD iMsgType     AS INTEGER /* 1: Text, 2: HTML */
         FIELD iMsgId       AS INTEGER
         .

  DEF VAR hBuffTTMessages AS HANDLE NO-UNDO.

	CONSTRUCTOR PUBLIC JBoxEMailDotNet (  ):
		
    SUPER().
    InitializeComponent().
                
    CATCH e AS Progress.Lang.Error:
        UNDO, THROW e.
    END CATCH.

	END CONSTRUCTOR.

	METHOD PRIVATE VOID InitializeComponent(  ):
/*     THIS-OBJECT:components  = NEW System.ComponentModel.Container(). */
/*     THIS-OBJECT:pop3Client1 = NEW Email.Net.Pop3.Pop3Client(THIS-OBJECT:components). */
    THIS-OBJECT:pop3Client1 = NEW Email.Net.Pop3.Pop3Client().
/*     THIS-OBJECT:SuspendLayout(). */


/*     THIS-OBJECT:pop3Client1:AuthenticationType = Email.Net.Common.Configurations.EAuthenticationType:Auto. */
/*     THIS-OBJECT:pop3Client1:Host = "mail.appfarm.no".                                                      */
/*     THIS-OBJECT:pop3Client1:Password = "gaautfra".                                                         */
/*     THIS-OBJECT:pop3Client1:Port = System.Convert:ToUInt16(110).                                           */
/*     THIS-OBJECT:pop3Client1:ProxyHost = "".                                                                */
/*     THIS-OBJECT:pop3Client1:ProxyPassword = "".                                                            */
/*     THIS-OBJECT:pop3Client1:ProxyPort = System.Convert:ToUInt16(0).                                        */
/*     THIS-OBJECT:pop3Client1:ProxyType = Email.Net.Common.Configurations.EProxyType:No.                     */
/*     THIS-OBJECT:pop3Client1:ProxyUser = "".                                                                */
/*     THIS-OBJECT:pop3Client1:ReceiveTimeout = 10000.                                                        */
/*     THIS-OBJECT:pop3Client1:SendTimeout = 10000.                                                           */
/*     THIS-OBJECT:pop3Client1:SSLInteractionType = Email.Net.Common.Configurations.EInteractionType:Plain.   */
/*     THIS-OBJECT:pop3Client1:Username = "utgaaende@appfarm.no".                                             */
/*                                                                                                            */
/*     pop3Client1:AttachmentDirectory = icAttachDir. */

/*         THIS-OBJECT:ClientSize = NEW System.Drawing.Size(292, 266).  */
/*     THIS-OBJECT:Name = "JBoxEMailDotNet". */
/*     THIS-OBJECT:Text = "JBoxEMailDotNet". */
/*     THIS-OBJECT:ResumeLayout(FALSE). */
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

    THIS-OBJECT:pop3Client1:AuthenticationType = Email.Net.Common.Configurations.EAuthenticationType:Auto.
    THIS-OBJECT:pop3Client1:Host = icHost.
    THIS-OBJECT:pop3Client1:Password = icPassword.
    THIS-OBJECT:pop3Client1:Port = System.Convert:ToUInt16(iiPort).
    THIS-OBJECT:pop3Client1:ProxyHost = icProxyHost.
    THIS-OBJECT:pop3Client1:ProxyPassword = "".
    THIS-OBJECT:pop3Client1:ProxyPort = System.Convert:ToUInt16(iiProxyPort).
    THIS-OBJECT:pop3Client1:ProxyType = Email.Net.Common.Configurations.EProxyType:No.
    THIS-OBJECT:pop3Client1:ProxyUser = "".
    THIS-OBJECT:pop3Client1:ReceiveTimeout = 10000.
    THIS-OBJECT:pop3Client1:SendTimeout = 10000.
    THIS-OBJECT:pop3Client1:SSLInteractionType = Email.Net.Common.Configurations.EInteractionType:Plain.
    THIS-OBJECT:pop3Client1:Username = icUserName.

    pop3Client1:AttachmentDirectory = icAttachDir.

    RETURN STRING(pop3Client1:Login()) BEGINS "+".
  END METHOD.

  METHOD PUBLIC LOGICAL EmailServerLogin():
    RETURN STRING(pop3Client1:Login()) BEGINS "+".
  END METHOD.

  METHOD PUBLIC INTEGER getEmailMessageCount():
      pop3MsgStat = pop3Client1:GetStatistic().

      RETURN INTEGER(pop3MsgStat:CountMessages).
  END METHOD.

  METHOD PUBLIC HANDLE getEmailMessages():

    DEFINE VARIABLE ix     AS INT      NO-UNDO.
    DEFINE VARIABLE iCount AS INT      NO-UNDO.
    DEFINE VARIABLE blBody AS LONGCHAR NO-UNDO.
  
    hBuffTTMessages = BUFFER ttMessages:HANDLE.

    pop3MsgStat = pop3Client1:GetStatistic().
  
    EMPTY TEMP-TABLE ttMessages.

    DO iCount = 1 TO pop3MsgStat:CountMessages:
      pop3Message = pop3Client1:GetMessage (iCount). 
      pop3Uid     = pop3Client1:GetUIDMessage(iCount).

/*       MESSAGE "pop3Message:Subject" pop3Message:Subject */
/*           VIEW-AS ALERT-BOX INFO BUTTONS OK.            */
      CREATE ttMessages.
      ASSIGN ttMessages.cSubject     = pop3Message:Subject
             ttMessages.cFrom        = pop3Message:From:Address
             ttMessages.cDisplayFrom = pop3Message:From:DisplayName
/*              ttMessages.cCc          = STRING(pop3Message:CarbonCopies)        */
/*              ttMessages.cBcc         = STRING(pop3Message:BlindedCarbonCopies) */
             ttMessages.dtDate       = pop3Message:Date
             ttMessages.iMsgType     = IF pop3Message:PlainText NE ? THEN 2 ELSE 1
             ttMessages.cPlainText   = IF pop3Message:PlainText NE ? THEN pop3Message:PlainText ELSE pop3Message:Text
             ttMessages.iAttachCount = pop3Message:Attachments:Count 
             blBody                  = pop3Message:Text
             ttMessages.iMsgId       = pop3Uid:SerialNumber   
             .
      
      COPY-LOB blBody TO ttMessages.blBody NO-ERROR.
      IF ERROR-STATUS:ERROR THEN
        ttMessages.iMsgType = 1.
  
      DO ix = 0 TO pop3Message:To:Count - 1:
        ASSIGN ttMessages.cTo = ttMessages.cTo + (IF ttMessages.cTo NE "" THEN ";" ELSE "") + pop3Message:To:Item[ix]:Address
               ttMessages.cDisplayTo = ttMessages.cDisplayTo + (IF ttMessages.cDisplayTo NE "" THEN ";" ELSE "") + pop3Message:To:Item[ix]:DisplayName
               .
      END.
      DO ix = 0 TO pop3Message:CarbonCopies:Count - 1:
        ASSIGN ttMessages.cCc = ttMessages.cCc + (IF ttMessages.cCc NE "" THEN ";" ELSE "") + pop3Message:CarbonCopies:Item[ix]:Address
               ttMessages.cDisplayCc = ttMessages.cDisplayCc + (IF ttMessages.cDisplayCc NE "" THEN ";" ELSE "") + pop3Message:CarbonCopies:Item[ix]:DisplayName
               .
      END.
      DO ix = 0 TO pop3Message:BlindedCarbonCopies:Count - 1:
        ASSIGN ttMessages.cBcc = ttMessages.cBcc + (IF ttMessages.cBcc NE "" THEN ";" ELSE "") + pop3Message:BlindedCarbonCopies:Item[ix]:Address
               ttMessages.cDisplayBcc = ttMessages.cDisplayBcc + (IF ttMessages.cDisplayBcc NE "" THEN ";" ELSE "") + pop3Message:BlindedCarbonCopies:Item[ix]:DisplayName
               .
      END.

      DO ix = 0 TO pop3Message:Attachments:Count - 1:
        ASSIGN ttMessages.cAttachFiles = ttMessages.cAttachFiles + (IF ttMessages.cAttachFiles NE "" THEN ";" ELSE "") + pop3Message:Attachments:Item[ix]:FullFilename 
               ttMessages.cAttachments = ttMessages.cAttachments + (IF ttMessages.cAttachments NE "" THEN ";" ELSE "") + pop3Message:Attachments:Item[ix]:TransferFilename 
               .
      END.
    END.
  
    RETURN hBuffTTMessages.
  END METHOD.
      
  METHOD PUBLIC LOGICAL DeleteMsg (INPUT iiMsgId AS INT):
    pop3Response = pop3Client1:DeleteMessage(iiMsgId).
    RETURN STRING(pop3Response:Type) = "OK".
  END METHOD.

  METHOD PUBLIC LOGICAL SignOff ():
    pop3Client1:Logout().      
    RETURN YES.
  END METHOD.

	DESTRUCTOR PUBLIC JBoxEMailDotNet ( ):

/*     IF VALID-OBJECT(components) THEN DO:              */
/*       CAST(components, System.IDisposable):Dispose(). */
/*     END.                                              */

	END DESTRUCTOR.

END CLASS.
