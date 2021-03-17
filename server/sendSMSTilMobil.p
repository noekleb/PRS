/* Kodrehode_sendMail.p
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEFINE VARIABLE cMailServer      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cAuthorize       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cAuthType        AS CHARACTER NO-UNDO.   
DEFINE VARIABLE cMailUser        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMailPwd         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMailProgram     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSmsDomain       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSMSReplyTo      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSMSProvider     AS CHARACTER NO-UNDO.

DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEmne AS CHARACTER NO-UNDO.
DEFINE VARIABLE cBodyTekst AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMobilNr AS CHARACTER NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.
DEFINE VARIABLE rSendEMail AS cls.SendEMail.SendEMail NO-UNDO.

ASSIGN 
  cLogg = 'sendSMSTilMobil' + REPLACE(STRING(TODAY),'/','')
  .
  
rStandardFunksjoner = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.
rSendeMail          = NEW cls.SendeMail.SendeMail( cLogg ) NO-ERROR.

SYSBLOKK:
FOR EACH SysPara NO-LOCK WHERE 
  SysPara.SysHId = 50 AND 
  SysPara.SysGr  = 50 AND 
  SysPara.ParaNr <= 9:
    CASE SysPara.Beskrivelse:
      WHEN "Mailhub"     THEN cMailServer  = Syspara.Parameter1.
      WHEN "DoAuth"      THEN cAuthorize   = Syspara.Parameter1.
      WHEN "AuthType"    THEN cAuthType    = Syspara.Parameter1.
      WHEN "User"        THEN cMailUser    = Syspara.Parameter1.
      WHEN "Password"    THEN cMailPwd     = Syspara.Parameter1.
      WHEN "Mailprogram" THEN cMailProgram = Syspara.Parameter1.
      WHEN "SMSDomain"   THEN cSmsDomain   = "@" + Syspara.Parameter1.
      WHEN "SMSProvider" THEN cSmsProvider = "@" + Syspara.Parameter1.
      WHEN "SMSReplyTo"  THEN cSmsReplyTo  = Syspara.Parameter1.
    END CASE.
END. /* SYSBLOKK */



rStandardFunksjoner:SkrivTilLogg(cLogg,
            '  Start sendSMSTilMobil:' 
            ).   
rStandardFunksjoner:SkrivTilLogg(cLogg,
            '    icParam: ' + icParam 
            ).   

ASSIGN 
  cEmne      = ENTRY(1,icParam,'|')
  cBodyTekst = ENTRY(2,icParam,'|')
  cMobilNr  = ENTRY(3,icParam,'|')
  ocReturn   = ''
  NO-ERROR.
IF ERROR-STATUS:ERROR THEN 
DO:
  rStandardFunksjoner:SkrivTilLogg(cLogg,
              '  Feil i parameterstreng. Avbryter.' 
              ).   
  RETURN.
END.  

rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner() NO-ERROR.

rStandardFunksjoner:SkrivTilLogg(cLogg,
            '  Parametre:' 
            ).   
rStandardFunksjoner:SkrivTilLogg(cLogg,
            '    cEmne: ' + cEmne 
            ).   
rStandardFunksjoner:SkrivTilLogg(cLogg,
            '    cBodytekst: ' + cBodytekst 
            ).   
rStandardFunksjoner:SkrivTilLogg(cLogg,
            '    cMobilNr: ' + cMobilNr 
            ).   
rStandardFunksjoner:SkrivTilLogg(cLogg,
            '    cMailServer: ' + cMailServer 
            ).   
rStandardFunksjoner:SkrivTilLogg(cLogg,
            '    cAuthorize: ' + cAuthorize 
            ).   
rStandardFunksjoner:SkrivTilLogg(cLogg,
            '    cAuthType: ' + cAuthType 
            ).   
rStandardFunksjoner:SkrivTilLogg(cLogg,
            '    cMailUser: ' + cMailUser 
            ).   
rStandardFunksjoner:SkrivTilLogg(cLogg,
            '    cMailPwd: ' + cMailPwd 
            ).   
rStandardFunksjoner:SkrivTilLogg(cLogg,
            '    cMailProgram: ' + cMailProgram 
            ).   
rStandardFunksjoner:SkrivTilLogg(cLogg,
            '    cSmsDomain: ' + cSmsDomain 
            ).   
rStandardFunksjoner:SkrivTilLogg(cLogg,
            '    cSmsProvider: ' + cSmsProvider 
            ).   
rStandardFunksjoner:SkrivTilLogg(cLogg,
            '    cSmsReplyTo: ' + cSmsReplyTo 
            ).   
 
RUN SendeMail.

obOk = ocReturn = "".

rStandardFunksjoner:SkrivTilLogg(cLogg,
            '  Resultat: ' + STRING(obOk) + ' ' + ocReturn  
            ).   
rStandardFunksjoner:SkrivTilLogg(cLogg,
            '  Slutt sendSMSTilMobil:' 
          ).   


/* **********************  Internal Procedures  *********************** */

PROCEDURE SendeMail:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE pcMessageFile AS CHARACTER NO-UNDO.
    DEFINE VARIABLE pcparLOGFILE AS CHARACTER NO-UNDO.
    
    pcparLOGFILE = rStandardFunksjoner:getTempFileName ().
    OUTPUT TO VALUE(pcparLOGFILE).
    OUTPUT CLOSE.
    FILE-INFO:FILE-NAME = pcparLOGFILE.
    pcparLOGFILE = FILE-INFO:FULL-PATHNAME.
    
    pcMessageFile = REPLACE(rStandardFunksjoner:getTempFileName (),'.tmp','.txt').
    OUTPUT TO VALUE(pcMessageFile).
    PUT UNFORMATTED cBodyTekst
      SKIP.
    OUTPUT CLOSE.

    FILE-INFO:FILE-NAME = pcMessageFile.
    pcMessageFile = FILE-INFO:FULL-PATHNAME.
    
    rStandardFunksjoner:SkrivTilLogg(cLogg,
                  '    Emne ' + cEmne + '.' 
                  ).   
    rStandardFunksjoner:SkrivTilLogg(cLogg,
                  '    Meldingstekst ' + pcMessageFile + '.' 
                  ).   
    rStandardFunksjoner:SkrivTilLogg(cLogg,
                  '    pcparLOGFILE ' + pcparLOGFILE + '.' 
                  ).   
    
    {syspara.i 50 50 70 rSendEMail:parSERVER}
    {syspara.i 50 50 71 rSendEMail:parFromADDRESS}
    {syspara.i 50 50 72 rSendEMail:parUSERNAME}
    {syspara.i 50 50 73 rSendEMail:parPASSWORD}
    {syspara.i 50 50 74 rSendEMail:parTLS}
    {syspara.i 50 50 75 cSmsProvider}
    rSendEMail:parToADDRESS       = cMobilNr + '@' + cSmsProvider.
    rSendEMail:parMailType        = 'SMS'.
    rSendEMail:parSUBJECT         = cEmne.
    rSendEMail:parMessage-Charset = 'iso-8859-1'. /* Blank eller 'UTF-8' når det går fra fil. */
    rSendEMail:parMessage-File    = pcMessageFile.
    rSendEMail:parFILE            = ''.
    rSendEMail:parLOGFILE         = pcparLOGFILE.
    
    

/*    /* TEST TEST TN 19/11-20 Ok.  */                     */
/*    rSendEMail:parSERVER          = 'Mail.uniweb.no:587'.*/
/*    rSendEMail:parFromADDRESS     = 'sms@gantretail.no'. */
/*    rSendEMail:parUSERNAME        = 'sms@gantretail.no'. */
/*    rSendEMail:parPASSWORD        = '64311360Gino'.      */
/*    rSendEMail:parTLS             = 'tls=yes'.           */

    
    rSendEMail:send( ).

    obOK = TRUE.

END PROCEDURE.

