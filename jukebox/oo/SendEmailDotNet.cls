/*------------------------------------------------------------------------
    File        : SendEmailDotNet
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Brynjar
    Created     : Mon May 08 15:24:59 CEST 2017
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING System.Exception.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS SendEmailDotNet: 
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEF VAR smtp      AS System.Net.Mail.SmtpClient  NO-UNDO.
    DEF VAR cAttList  AS CHAR NO-UNDO.    
    DEF VAR ix        AS INT  NO-UNDO.
    
    DEF STREAM strLog.
        
        
        
    DEFINE PUBLIC PROPERTY Subject          AS CHARACTER NO-UNDO GET. SET. 
    DEFINE PUBLIC PROPERTY Body             AS CHARACTER NO-UNDO GET. SET. 
    DEFINE PUBLIC PROPERTY ReplyTo          AS CHARACTER NO-UNDO GET. SET. 
    DEFINE PUBLIC PROPERTY From             AS CHARACTER NO-UNDO GET. SET. 
    DEFINE PUBLIC PROPERTY FromName         AS CHARACTER NO-UNDO GET. SET. 
    DEFINE PUBLIC PROPERTY Sender           AS CHARACTER NO-UNDO GET. SET. 
    DEFINE PUBLIC PROPERTY SenderName       AS CHARACTER NO-UNDO GET. SET. 
    DEFINE PUBLIC PROPERTY TO               AS CHARACTER NO-UNDO GET. SET.
    DEFINE PUBLIC PROPERTY CC               AS CHARACTER NO-UNDO GET. SET. 
    DEFINE PUBLIC PROPERTY BCC              AS CHARACTER NO-UNDO GET. SET.
    DEFINE PUBLIC PROPERTY LogFile          AS CHARACTER NO-UNDO GET. SET.
    DEFINE PUBLIC PROPERTY SuppressErrorMsg AS LOGICAL   NO-UNDO GET. SET.
    DEFINE PUBLIC PROPERTY LogErrorsOnly    AS LOGICAL   NO-UNDO INIT YES GET. SET.
    DEFINE PUBLIC PROPERTY LogMessage       AS CHARACTER NO-UNDO GET. PRIVATE SET.
         

    CONSTRUCTOR PUBLIC SendEmailDotNet (icMailServer AS CHAR
                                       ,icUser       AS CHAR
                                       ,icPwd        AS CHAR
                                        ):
        SUPER ().
        
      InitializeComponent(icMailServer,icUser,icPwd,25,no).
    END CONSTRUCTOR.

    CONSTRUCTOR PUBLIC SendEmailDotNet (icMailServer AS CHAR
                                       ,icUser       AS CHAR
                                       ,icPwd        AS CHAR
                                       ,iiPort       AS INT
                                       ,ibSSL        AS LOG
                                        ):
      SUPER ().
        
      InitializeComponent(icMailServer,icUser,icPwd,iiPort,ibSSL).
    END CONSTRUCTOR.


    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LOGICAL LogToFile( INPUT icMessage AS CHAR ):
      
      LogMessage = icMessage.
      
      IF LogFile = "" THEN DO:
        IF NOT SuppressErrorMsg THEN   
          MESSAGE icMessage
          VIEW-AS ALERT-BOX ERROR.
        RETURN NO.
      END.  
        
      OUTPUT STREAM strLog TO VALUE(LogFile) APPEND.
      PUT STREAM strLog UNFORMATTED STRING(NOW,"99/99/9999 HH:MM:SS") " -> " icMessage SKIP.
      OUTPUT STREAM strLog CLOSE.
        
      RETURN YES.
        
    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL SetAttachmentFile_s( INPUT icAttachmentFile AS CHAR):
            
      icAttachmentFile = REPLACE(icAttachmentFile,";",",").
      
      DO ix = 1 TO NUM-ENTRIES(icAttachmentFile):
        IF NOT CAN-DO(cAttList,ENTRY(ix,icAttachmentFile)) THEN DO:
          IF SEARCH(ENTRY(ix,icAttachmentFile)) = ? THEN DO:
            LogToFile("Invalid attachment file-name: " + ENTRY(ix,icAttachmentFile)).
            RETURN NO.
          END.    
          cAttList = cAttList + (IF cAttList NE "" THEN "," ELSE "") + ENTRY(ix,icAttachmentFile).
                                        
        END.      
      END.    
        
      RETURN YES.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID InitializeComponent( icMailServer AS CHAR
                                           ,icUser       AS CHAR
                                           ,icPwd        AS CHAR
                                           ,iiPort       AS INT
                                           ,ibSSL        AS LOG ):
       
      smtp = NEW System.Net.Mail.SmtpClient(icMailServer).
      smtp:EnableSsl = ibSSL.
      smtp:Port = iiPort.
      smtp:Credentials = NEW System.Net.NetworkCredential(icUser,icPwd).
        
      RETURN.

    END METHOD.    

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    
    METHOD PUBLIC LOGICAL Send():
      
      DEF VAR emes      AS System.Net.Mail.MailMessage NO-UNDO.
      DEF VAR cErrorMsg AS CHAR NO-UNDO.
      
      IF THIS-OBJECT:From = "" THEN 
        cErrorMsg = "Cannot send without a 'from' address".
      ELSE IF THIS-OBJECT:To = "" AND THIS-OBJECT:CC = "" AND THIS-OBJECT:BCC = "" THEN 
        cErrorMsg = "Missing recipient(s) ('TO','CC' and 'BCC' addresses)".

      IF cErrorMsg NE "" THEN DO:
        LogToFile(cErrorMsg).
        RETURN NO.
      END.    
      
      emes = NEW System.Net.Mail.MailMessage().
             
      DO ix = 1 TO NUM-ENTRIES(cAttList):
        FILE-INFO:FILE-NAME = SEARCH(ENTRY(ix,cAttList)).
        emes:Attachments:Add(NEW System.Net.Mail.Attachment(FILE-INFO:FULL-PATHNAME)).
      END.  
      
      IF THIS-OBJECT:Sender NE "" THEN DO:
        IF THIS-OBJECT:SenderName NE "" THEN  
          emes:Sender = NEW System.Net.Mail.MailAddress(THIS-OBJECT:Sender,THIS-OBJECT:SenderName) NO-ERROR.
        ELSE  
          emes:Sender = NEW System.Net.Mail.MailAddress(THIS-OBJECT:Sender) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
          THIS-OBJECT:LogToFile(ERROR-STATUS:GET-MESSAGE(1) + " Error in SENDER-address: " + THIS-OBJECT:Sender).
          RETURN NO. 
        END.      
      END.
      
      IF THIS-OBJECT:FromName NE "" THEN
        emes:From = NEW System.Net.Mail.MailAddress(THIS-OBJECT:From,THIS-OBJECT:FromName) NO-ERROR.
      ELSE  
        emes:From = NEW System.Net.Mail.MailAddress(THIS-OBJECT:From) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN DO:
        THIS-OBJECT:LogToFile(ERROR-STATUS:GET-MESSAGE(1) + " Error in FROM-address: " + THIS-OBJECT:From).
        RETURN NO. 
      END.      
        
      emes:Subject = THIS-OBJECT:Subject.
      emes:Body = THIS-OBJECT:Body.
      
      IF SUBSTR(TRIM(THIS-OBJECT:Body),1,1) = "<" THEN
        emes:IsBodyHtml = YES.  
        
      IF THIS-OBJECT:To NE "" THEN DO:
        THIS-OBJECT:To = REPLACE(THIS-OBJECT:To,";",",").   
        DO ix = 1 TO NUM-ENTRIES(THIS-OBJECT:To):  
          emes:To:Add(NEW System.Net.Mail.MailAddress(ENTRY(ix,THIS-OBJECT:To))) NO-ERROR.
          IF ERROR-STATUS:ERROR THEN DO:
            THIS-OBJECT:LogToFile(ERROR-STATUS:GET-MESSAGE(1) + " Error in TO-address: " + ENTRY(ix,THIS-OBJECT:To)).
            RETURN NO. 
          END.    
        END.
      END.  
      IF THIS-OBJECT:CC NE "" THEN DO:  
        THIS-OBJECT:CC = REPLACE(THIS-OBJECT:CC,";",",").   
        DO ix = 1 TO NUM-ENTRIES(THIS-OBJECT:CC):  
          emes:CC:Add(NEW System.Net.Mail.MailAddress(ENTRY(ix,THIS-OBJECT:CC))) NO-ERROR.
          IF ERROR-STATUS:ERROR THEN DO:
            THIS-OBJECT:LogToFile(ERROR-STATUS:GET-MESSAGE(1) + " Error in CC-address: " + ENTRY(ix,THIS-OBJECT:CC)).
            RETURN NO. 
          END.    
        END.
      END.  
      IF THIS-OBJECT:BCC NE "" THEN DO:  
        THIS-OBJECT:BCC = REPLACE(THIS-OBJECT:BCC,";",",").   
        DO ix = 1 TO NUM-ENTRIES(THIS-OBJECT:BCC):  
          emes:BCC:Add(NEW System.Net.Mail.MailAddress(ENTRY(ix,THIS-OBJECT:BCC))) NO-ERROR.
          IF ERROR-STATUS:ERROR THEN DO:
            THIS-OBJECT:LogToFile(ERROR-STATUS:GET-MESSAGE(1) + " Error in BCC-address: " + ENTRY(ix,THIS-OBJECT:BCC)).
            RETURN NO. 
          END.    
        END.
      END.  
      IF THIS-OBJECT:ReplyTo NE "" THEN DO:  
        emes:ReplyTo = NEW System.Net.Mail.MailAddress(THIS-OBJECT:ReplyTo) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
          THIS-OBJECT:LogToFile(ERROR-STATUS:GET-MESSAGE(1) + " Error in ReplyTo-address: " + THIS-OBJECT:ReplyTo).
          RETURN NO. 
        END.    
      END.  

      smtp:Send(emes) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN DO:
        THIS-OBJECT:LogToFile(ERROR-STATUS:GET-MESSAGE(1) 
                           + " Error executing Send for address(es): " 
                           + (IF THIS-OBJECT:To  NE "" THEN " TO: "  + THIS-OBJECT:To ELSE "")  
                           + (IF THIS-OBJECT:CC  NE "" THEN " CC: "  + THIS-OBJECT:CC ELSE "")  
                           + (IF THIS-OBJECT:BCC NE "" THEN " BCC: " + THIS-OBJECT:BCC ELSE "")
                           ).
        RETURN NO.                     
      END.    
      IF NOT LogErrorsOnly THEN
         THIS-OBJECT:LogToFile("Email sent from " + THIS-OBJECT:From + " to address(es): " 
                             + (IF THIS-OBJECT:To  NE "" THEN " TO: "  + THIS-OBJECT:To ELSE "")  
                             + (IF THIS-OBJECT:CC  NE "" THEN " CC: "  + THIS-OBJECT:CC ELSE "")  
                             + (IF THIS-OBJECT:BCC NE "" THEN " BCC: " + THIS-OBJECT:BCC ELSE "")
                             + (IF THIS-OBJECT:ReplyTo NE "" THEN " ReplyTo: " + THIS-OBJECT:ReplyTo ELSE "")
                             + (IF THIS-OBJECT:cAttList NE "" THEN " Attachments: " + THIS-OBJECT:cAttList ELSE "")
                             + (IF THIS-OBJECT:Subject NE "" THEN " Subject: " + THIS-OBJECT:Subject ELSE "")
                              ).
                                          
      RETURN YES.
    END METHOD.    
    
    DESTRUCTOR PUBLIC SendEmailDotNet ( ):

    END DESTRUCTOR.

/*CATCH Progress.Lang.SysError AS eSys:*/
/*/*IF bThrow THEN UNDO, THROW eSys.*/ */
/*END CATCH.                           */

END CLASS.
