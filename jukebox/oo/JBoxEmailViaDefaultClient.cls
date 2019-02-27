 
/*------------------------------------------------------------------------
   File        : JBoxEmailViaDefaultClient
   Purpose     : 
   Syntax      : 
   Description : 
   Author(s)   : Brynjar
   Created     : Wed Sep 20 08:32:54 CEST 2015
   Notes       : 
 ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING System.Net.Mail.MailMessage FROM ASSEMBLY.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS JBoxEmailViaDefaultClient: 
		
  DEFINE        VARIABLE smtpClient     AS System.Net.Mail.SmtpClient         NO-UNDO.
  DEFINE        VARIABLE mailMessage    AS System.Net.Mail.MailMessage        NO-UNDO.
  DEFINE        VARIABLE mailFrom       AS System.Net.Mail.MailAddress        NO-UNDO.
  DEFINE        VARIABLE mailTo         AS System.Net.Mail.MailAddress        NO-UNDO.
  DEFINE        VARIABLE mailCc         AS System.Net.Mail.MailAddress        NO-UNDO.
  DEFINE        VARIABLE mailBcc        AS System.Net.Mail.MailAddress        NO-UNDO.
  DEFINE        VARIABLE mailAttachment AS System.Net.Mail.Attachment         NO-UNDO.
  DEFINE        VARIABLE mailDir        AS System.Net.Mail.SmtpDeliveryMethod NO-UNDO.
  
  DEFINE PUBLIC VARIABLE Sender         AS CHARACTER                          NO-UNDO.
  DEFINE PUBLIC VARIABLE Receiver       AS CHARACTER                          NO-UNDO.
  DEFINE PUBLIC VARIABLE Cc             AS CHARACTER                          NO-UNDO.
  DEFINE PUBLIC VARIABLE Bcc            AS CHARACTER                          NO-UNDO.
  DEFINE PUBLIC VARIABLE Subject        AS CHARACTER                          NO-UNDO.
  DEFINE PUBLIC VARIABLE Body           AS CHARACTER                          NO-UNDO.
  DEFINE PUBLIC VARIABLE AttachmentList AS CHARACTER                          NO-UNDO.
  DEFINE PUBLIC VARIABLE MailDirectory  AS CHARACTER                          NO-UNDO.

  DEFINE TEMP-TABLE ttFile
    FIELD cFileName    AS CHARACTER
    FIELD cModFileName AS CHARACTER
    FIELD dFileDate    AS DATE
    FIELD iFileTime    AS INTEGER.

  CONSTRUCTOR PUBLIC JBoxEmailViaDefaultClient (INPUT icSubject        AS CHARACTER
    ,INPUT icBody           AS CHARACTER
    ,INPUT icAttachmentList AS CHARACTER):
    SUPER ().
        	  
    InitializeComponent("","","",icSubject,icBody,icAttachmentList).

  END CONSTRUCTOR.

  CONSTRUCTOR PUBLIC JBoxEmailViaDefaultClient (INPUT icSubject        AS CHARACTER
    ,INPUT icBody           AS CHARACTER):
    SUPER ().

    InitializeComponent("","","",icSubject,icBody,"").

  END CONSTRUCTOR.

  CONSTRUCTOR PUBLIC JBoxEmailViaDefaultClient ():
    SUPER().
  END CONSTRUCTOR.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC VOID SendMail(  ):
    InitializeComponent(Receiver,cc,Bcc,Subject,Body,AttachmentList).
    RETURN.

  END METHOD.

  METHOD PRIVATE VOID InitializeComponent(
     INPUT icToList         AS CHARACTER 
    ,INPUT icCcList         AS CHARACTER
    ,INPUT icBccList        AS CHARACTER
    ,INPUT icSubject        AS CHARACTER
    ,INPUT icBody           AS CHARACTER
    ,INPUT icAttachmentList AS CHARACTER):

    DEFINE VARIABLE ix       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cEmlFile AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cInput   AS CHARACTER NO-UNDO.
    def var bNoReceiver as logical no-undo. /*Used for getting one receiver in the EML/MSG file*/
    
    smtpClient = NEW System.Net.Mail.SmtpClient().
    smtpClient:SendCompleted:SUBSCRIBE (THIS-OBJECT:handler_SendCompleted).

    mailMessage = NEW System.Net.Mail.MailMessage().

    if MailDirectory =  '' then MailDirectory   = session:temp-dir.
    IF icToList      <> '' and icToList  <> ?  THEN Receiver  = REPLACE(icToList,';',',').
    IF icCcList      <> '' and icCcList  <> ?  THEN Cc        = REPLACE(icCcList,';',',').
    IF icBccList     <> '' and icBccList <> ?  THEN Bcc       = REPLACE(icBccList,';',','). 
    if Sender        =  '' then Sender          = 'john.doe@foo.com'.
    if Receiver = '' then 
      assign 
        bNoReceiver = true
        Receiver = 'john.doe@foo.com'.

    mailFrom = NEW System.Net.Mail.MailAddress(Sender).
    mailMessage:From = mailFrom. 
    
    if Receiver  ne '' then mailMessage:To:Add(Receiver).
    if Cc        ne '' then mailMessage:Cc:Add(Cc).
    if Bcc       ne '' then mailMessage:Bcc:Add(Bcc).
    

    mailDir = NEW System.Net.Mail.SmtpDeliveryMethod().
    mailMessage:Subject = icSubject.

    
/*    IF mailMessage:To:ToString() = '' THEN                                       */
/*      mailMessage:To:Add(IF icToList = '' THEN 'john.doe@foo.com' ELSE icToList).*/
    
    icBody = TRIM(icBody).
    mailMessage:IsBodyHtml = YES. 
  
    mailMessage:Body = icBody.
  
    DO ix = 1 TO NUM-ENTRIES(icAttachmentList,";"):
      mailAttachment = NEW System.Net.Mail.Attachment(ENTRY(ix,icAttachmentList,";")).
      mailMessage:Attachments:Add(mailAttachment).
    END.
  
    smtpClient:PickupDirectoryLocation = SESSION:TEMP-DIR.
  
    smtpClient:DeliveryMethod = System.Net.Mail.SmtpDeliveryMethod:SpecifiedPickupDirectory. 
  
    smtpClient:Send(mailMessage).

    INPUT FROM OS-DIR (SESSION:TEMP-DIR).
    REPEAT:
      IMPORT cEmlFile.
      IF cEmlFile MATCHES "*.eml" THEN 
      DO:
        FILE-INFO:FILE-NAME = SESSION:TEMP-DIR + cEmlFile.
        IF FILE-INFO:FILE-CREATE-TIME > TIME - 3 THEN 
        DO:
          CREATE ttFile.
          ASSIGN 
            ttFile.cFileName    = FILE-INFO:FULL-PATHNAME
            ttFile.cModFileName = SESSION:TEMP-DIR + "mod_" + cEmlFile
            ttFile.dFileDate    = FILE-INFO:FILE-CREATE-DATE
            ttFile.iFileTime    = FILE-INFO:FILE-CREATE-TIME
            .
        END.
      END.
    END.
    INPUT CLOSE.
    
    FOR EACH ttFile 
      BY ttFile.dFileDate DESCENDING
      BY ttFile.iFileTime DESCENDING:

      INPUT FROM VALUE(ttFile.cFileName).
      SET ^.
      SET ^.
      SET ^.
      SET ^.
      SET ^.

      OUTPUT TO VALUE(ttFile.cModFileName).
      PUT UNFORMATTED 
        "X-Unsent: 1" SKIP
        if not bNoReceiver then 'To: ' + mailMessage:To:ToString() else '' skip
        "MIME-Version: 1.0" SKIP
/*        if not bNoReceiver then 'To: ' + mailMessage:To:ToString() else '' skip.*/
         .
      REPEAT:
        IMPORT UNFORMATTED cInput.
        PUT UNFORMATTED cInput SKIP.
        IF cInput BEGINS "Content-Transfer-Encoding:" /* " quoted-printable" */ OR
          cInput BEGINS " boundary=--" OR
          cInput BEGINS "Content-Disposition: attachment"
          THEN PUT UNFORMATTED SKIP(1).
/*              IF (cInput BEGINS "Content-Transfer-Encoding" AND icAttachmentList = "") OR cInput BEGINS " boundary=--" THEN PUT UNFORMATTED SKIP(1).*/
      END.
      IF icAttachmentList = "" AND icBody = "" THEN PUT UNFORMATTED SKIP(4).
      INPUT CLOSE.
      OUTPUT CLOSE.

      DYNAMIC-FUNCTION("setWebDoc","",ttFile.cModFileName) NO-ERROR.
      LEAVE.
    END.
    
    FINALLY:
      DELETE OBJECT mailFrom       NO-ERROR.
      DELETE OBJECT mailAttachment NO-ERROR.
      DELETE OBJECT mailDir        NO-ERROR.
      DELETE OBJECT mailMessage    NO-ERROR.
      DELETE OBJECT smtpClient     NO-ERROR. 
             		
    END FINALLY.
  END METHOD.

  METHOD PRIVATE VOID handler_SendCompleted (sender AS CLASS System.Object, e AS CLASS System.ComponentModel.AsyncCompletedEventArgs):
    MESSAGE STRING(e)
      VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
  END METHOD. 


  DESTRUCTOR PUBLIC JBoxEmailViaDefaultClient ( ):
    DELETE OBJECT mailFrom       NO-ERROR.
    DELETE OBJECT mailAttachment NO-ERROR.
    DELETE OBJECT mailDir        NO-ERROR.
    DELETE OBJECT mailMessage    NO-ERROR.
    DELETE OBJECT smtpClient     NO-ERROR.
  END DESTRUCTOR.

END CLASS.
