/* Kodrehode_sendMail.p
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEFINE VARIABLE lKOrdre_Id AS DECIMAL NO-UNDO.
DEFINE VARIABLE cFilNavn AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPkSdlFil AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEmne AS CHARACTER NO-UNDO.
DEFINE VARIABLE cBodyTekst AS CHARACTER NO-UNDO.
DEFINE VARIABLE ceMailAdr AS CHARACTER NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.
DEFINE VARIABLE rSendEMail AS cls.SendEMail.SendEMail NO-UNDO.

rStandardFunksjoner = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.
rSendeMail          = NEW cls.SendeMail.SendeMail( ) NO-ERROR.

rStandardFunksjoner:SkrivTilLogg(cLogg,
            '  Start.' 
            ).   
rStandardFunksjoner:SkrivTilLogg(cLogg,
            '    icParam: ' + icParam 
            ).   

ASSIGN 
  lKOrdre_Id = DEC(ENTRY(1,icParam,'|'))
  cEmne      = ENTRY(2,icParam,'|')
  cBodyTekst = ENTRY(3,icParam,'|')
  ceMailAdr  = ENTRY(4,icParam,'|')
  ocReturn   = ''
  cLogg      = 'Kodrehode_sendMail' + REPLACE(STRING(TODAY),'/','')
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
            '    lKOrdre_Id: ' + STRING(lKOrdre_Id) 
            ).   
rStandardFunksjoner:SkrivTilLogg(cLogg,
            '    cEmne: ' + cEmne 
            ).   
rStandardFunksjoner:SkrivTilLogg(cLogg,
            '    cBodytekst: ' + cBodytekst 
            ).   
rStandardFunksjoner:SkrivTilLogg(cLogg,
            '    ceMailAdr: ' + ceMailAdr 
            ).   
            
            
            
IF lKOrdre_Id > 0 THEN 
DO: 
  FIND KOrdreHode NO-LOCK WHERE 
    KOrdreHode.KOrdre_Id = lKORdre_Id NO-ERROR.
  IF AVAILABLE KOrdreHode THEN 
  DO:
    RUN SendeMail.
  END.
  ELSE DO: 
    rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  Fant ikke KOrdre med KOrdre_Id = ' + STRING(lKORdre_Id) + '.' 
                ).   
    RETURN.
  END.
END.

obOk = ocReturn = "".

rStandardFunksjoner:SkrivTilLogg(cLogg,
            '  Resultat: ' + STRING(obOk) + ' ' + ocReturn  
            ).   
rStandardFunksjoner:SkrivTilLogg(cLogg,
            '  Slutt.' 
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
    
    rSendEMail:parToADDRESS       = ceMailAdr.
    rSendEMail:parMailType        = 'KOrdreMail'.
    rSendEMail:parSUBJECT         = cEmne.
    rSendEMail:parMessage-Charset = 'iso-8859-1'. /* Blank eller 'UTF-8' når det går fra fil. */
    rSendEMail:parMessage-File    = pcMessageFile.
    rSendEMail:parFILE            = ''.
    rSendEMail:parLOGFILE         = pcparLOGFILE.  
    rSendEMail:send( ).
    
    obOK = TRUE.

END PROCEDURE.

