
/*------------------------------------------------------------------------
    File        : excelrapport3_run.p 
    Purpose     : Batch prog. for start av temp rapport 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/


/* ***************************  Main Block  *************************** */
DEFINE VARIABLE obOk      AS LOG       NO-UNDO.
DEFINE VARIABLE ocMelding AS CHAR      NO-UNDO.
DEFINE VARIABLE cRappNr   AS CHARACTER NO-UNDO.
DEFINE VARIABLE iCount    AS INTEGER   NO-UNDO.
DEFINE VARIABLE ttbh      AS HANDLE NO-UNDO. 
DEFINE VARIABLE tth       AS HANDLE NO-UNDO. 


DEFINE VARIABLE cSMTPserver       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cMailSender       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cMailAuthorize    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cMailAuthType     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cMailUser         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cMailPwd          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cMailProgram      AS CHARACTER INIT 'prssmtpmailv5_7a.p' NO-UNDO.
DEFINE VARIABLE cMailContentType  AS CHARACTER INIT 'CharSet=iso8859-1'  NO-UNDO.


    {syspara.i 50 50 1 cSMTPserver }
    {syspara.i 50 50 2 cMailAuthorize  }
    {syspara.i 50 50 3 cMailAuthType }
    {syspara.i 50 50 4 cMailUser }
    {syspara.i 50 50 5 cMailPwd }

DEFINE TEMP-TABLE ttAttachments NO-UNDO
      FIELD iNum      AS INTEGER
      FIELD cFileName AS CHARACTER
      FIELD cExtent   AS CHARACTER
      FIELD cFullPath AS CHARACTER
      FIELD bBinary   AS LOGICAL
      INDEX iNum IS PRIMARY UNIQUE iNum.


DEFINE TEMP-TABLE ttsyspara LIKE syspara. 
ttbh = BUFFER ttsyspara:HANDLE.                       
ttbh:TABLE-HANDLE:PRIVATE-DATA = "batch". 
tth = ttbh:TABLE-HANDLE. 

/* Initiering av JukeBox */
RUN initjukebox.p.
DYNAMIC-FUNCTION('setEnableColor',NO).
DYNAMIC-FUNCTION("setAttribute",SESSION,"SE_PRINTER",SESSION:PRINTER-NAME).

/* Henter oppkoblingsinfo fra oppstartsicon. */
IF SESSION:PARAMETER <> "" THEN 
DO iCount = 1 TO NUM-ENTRIES(SESSION:PARAMETER):
    IF ENTRY(iCount,SESSION:PARAMETER) BEGINS "RAPPNR" AND 
        NUM-ENTRIES(ENTRY(iCount,SESSION:PARAMETER),"=") = 2 THEN 
    DO:
        ASSIGN 
            cRappNr = ENTRY(2,ENTRY(iCount,SESSION:PARAMETER),"=").
    END.
END.
   


IF cRappNr = '' THEN RETURN.
ELSE RUN excelrapport3.p (INPUT '228|' + cRappNr,INPUT ttbh,INPUT ?, OUTPUT ocMelding,OUTPUT obOk) NO-ERROR.


RUN SendMail. 


/* -------------------------------------------------------- */
FUNCTION BuildAttachments RETURNS CHARACTER
    (INPUT ipcFileList    AS CHARACTER) :
    
  DEFINE VARIABLE cReturn       AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE ix            AS INTEGER     NO-UNDO.

  TEMP-TABLE ttAttachments:DEFAULT-BUFFER-HANDLE:EMPTY-TEMP-TABLE().
  DO ix = 1 TO NUM-ENTRIES(ipcFileList):
    FILE-INFO:FILE-NAME = ENTRY(ix,ipcFileList).
    IF FILE-INFO:FILE-NAME NE ?  THEN
    DO:
      CREATE ttAttachments.
      ASSIGN 
        ttAttachments.iNum = ix
        ttAttachments.cFileName = FILE-INFO:FILE-NAME
        ttAttachments.cFileName = REPLACE(ttAttachments.cFileName,"\","/")
        ttAttachments.cFullPath = ttAttachments.cFileName
        ttAttachments.cFileName = SUBSTRING(ttAttachments.cFileName,R-INDEX(ttAttachments.cFileName,'/') + 1)
        ttAttachments.cExtent   = SUBSTRING(ttAttachments.cFileName,R-INDEX(ttAttachments.cFileName,'.') + 1)
        ttAttachments.cFileName = SUBSTRING(ttAttachments.cFileName,1,R-INDEX(ttAttachments.cFileName,'.') - 1)
        ttAttachments.bBinary   = ttAttachments.cExtent = 'XLS'
      .        
    END.
  END.

END FUNCTION.


FUNCTION GetSubLists RETURNS CHAR 
  (INPUT ipcType AS CHARACTER) :
  
  DEFINE VARIABLE cReturn AS CHARACTER   NO-UNDO.

  FOR EACH ttAttachments:    
    IF ipcType = 'Files' THEN 
    DO:
      cReturn = cReturn + ',' + ttAttachments.cFullPath.
    END.
    ELSE
    DO: /*Attachments*/
      cReturn = cReturn + ',' + ttAttachments.cFileName + '.' + ttAttachments.cExtent.
      IF ttAttachments.bBinary THEN cReturn = cReturn + ':filetype=BINARY'. 
    END.
  END.
  cReturn = TRIM(cReturn,',').
  RETURN cReturn.

END FUNCTION.



PROCEDURE SendMail : 

  DEFINE VARIABLE qh AS HANDLE NO-UNDO.
  DEFINE VARIABLE bh AS HANDLE NO-UNDO.

  DEFINE VARIABLE cMailReceiver    AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cMailCC          AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cMailBCC         AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cMailSubject     AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cMailBody        AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cMailAttachments AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cMailFiles       AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE iMailImportance  AS INTEGER     NO-UNDO.
  DEFINE VARIABLE cReturn          AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE bOk              AS LOGICAL     NO-UNDO.

  CREATE QUERY qh.
  bh = tth:DEFAULT-BUFFER-HANDLE.
  qh:SET-BUFFERS(bh).
  qh:QUERY-PREPARE('for each ' + tth:DEFAULT-BUFFER-HANDLE:NAME + ' WHERE hjelpetekst1="SendMail"').
  qh:QUERY-OPEN().
  qh:GET-NEXT().
  DO WHILE bh:AVAIL: 
    ASSIGN 
      cMailReceiver   = bh:BUFFER-FIELD('parameter1'):BUFFER-VALUE
      cMailSubject    = ENTRY(1,bh:BUFFER-FIELD('parameter2'):BUFFER-VALUE,'¤')
      cMailBody       = ENTRY(2,bh:BUFFER-FIELD('parameter2'):BUFFER-VALUE,'¤')
      cMailSender     = cMailUser
      cMailFiles      = ENTRY(3,bh:BUFFER-FIELD('parameter2'):BUFFER-VALUE,'¤')
    .


  buildAttachments(cMailFiles).
  cMailAttachments = getSubLists('Attachments').
  cMailFiles       = getSubLists('Files').


      RUN prssmtpmailv5_7a.p (
        /*mailhub    */   cSMTPserver,
        /*EmailTo    */   cMailReceiver,
        /*EmailFrom  */   cMailSender,
        /*EmailCC    */   cMailCC,
        /*Attachments*/   cMailAttachments,
        /*LocalFiles */   cMailFiles,
        /*Subject    */   cMailSubject,
        /*Body       */   cMailBody,
        /*MIMEHeader */   cMailContentType,
        /*BodyType   */   "",
        /*Importance */   iMailImportance,
        /*L_DoAUTH   */   IF cMailAuthorize = '1' THEN 'yes' ELSE 'no',
        /*C_AuthType */   cMailAuthType,
        /*C_User     */   cMailUser,
        /*C_Password */   cMailPwd,
        /*oSuccessful*/  OUTPUT bOk,
        /*vMessage   */  OUTPUT cReturn) NO-ERROR.


    bh:BUFFER-DELETE().
    qh:GET-NEXT().
  END.
  qh:QUERY-CLOSE().
  DELETE OBJECT qh.
END. 


QUIT.

