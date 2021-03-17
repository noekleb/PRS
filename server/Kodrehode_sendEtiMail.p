/* Kodrehode_manko.p
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
DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.
DEFINE VARIABLE ceMailAdr AS CHARACTER NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.
DEFINE VARIABLE rSendEMail AS cls.SendEMail.SendEMail NO-UNDO.
rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.
rSendeMail  = NEW cls.SendeMail.SendeMail( ) NO-ERROR.

ASSIGN 
  lKOrdre_Id = DEC(ENTRY(1,icParam,'|'))
  cEmne      = ENTRY(2,icParam,'|')
  cTekst     = ENTRY(3,icParam,'|')
  ceMailAdr  = ENTRY(4,icParam,'|')
  ocReturn   = 'Fant ikke PDF filen.'
  cLogg = 'Kodrehode_sendEtiMail' + REPLACE(STRING(TODAY),'/','')
  .

rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner() NO-ERROR.

rStandardFunksjoner:SkrivTilLogg(cLogg,
            '  Start.' 
            ).   
IF lKOrdre_Id > 0 THEN 
DO: 
  FIND KOrdreHode NO-LOCK WHERE 
    KOrdreHode.KOrdre_Id = lKORdre_Id NO-ERROR.
  IF AVAILABLE KOrdreHode THEN 
  DO:   
    FIND LAST KOrdrePostPakke OF KOrdreHode NO-LOCK USE-INDEX idxPostPakke NO-ERROR.
    IF AVAILABLE KOrdrePostPakke THEN 
    DO:
      cFilNavn = REPLACE(rStandardFunksjoner:getTempFileName (),'.tmp','.pdf').
      COPY-LOB FROM KOrdrePostPakke.PdfFil TO FILE cFilNavn NO-ERROR. 

      cPkSdlFil = REPLACE(rStandardFunksjoner:getTempFileName (),'.tmp','.pdf').
      RUN Gant_Skrivutlever.p (STRING(KOrdreHode.KOrdre_Id),cPkSdlFil).
      IF SEARCH(cPkSdlFil) <> ? THEN 
        cPkSdlFil = SEARCH(cPkSdlFil).
      ELSE 
        cPkSdlFil = ''.

      rStandardFunksjoner:SkrivTilLogg(cLogg,
                  '    eMailAdr ' + ceMailAdr + '.' 
                  ).   
      rStandardFunksjoner:SkrivTilLogg(cLogg,
                  '    Vedlegg ' + (IF cFilNavn <> ? THEN cFilNavn ELSE '?') + (IF cPkSdlFil <> ? THEN ',' + cPkSdlFil ELSE '?') + '.' 
                  ).   

      IF SEARCH(cFilNavn) <> ? THEN  
      DO:
        RUN SendeMail (SEARCH(cFilNavn),SEARCH(cPkSdlFil)).
        ocReturn = ''.        
      END.
    END.
  END.
END.

obOk = ocReturn = "".
obOk = TRUE.

rStandardFunksjoner:SkrivTilLogg(cLogg,
            '   Resultat: ' + STRING(obOk) + ' ' + ocReturn  
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
    DEFINE INPUT PARAMETER icPdfFil   AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER icPkSdlFil AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE pcMessageFile AS CHARACTER NO-UNDO.
    DEFINE VARIABLE pcparLOGFILE AS CHARACTER NO-UNDO.
    
    pcparLOGFILE = rStandardFunksjoner:getTempFileName ().
    OUTPUT TO VALUE(pcparLOGFILE).
    OUTPUT CLOSE.
    FILE-INFO:FILE-NAME = pcparLOGFILE.
    pcparLOGFILE = FILE-INFO:FULL-PATHNAME.
    
    pcMessageFile = REPLACE(rStandardFunksjoner:getTempFileName (),'.tmp','.txt').
    OUTPUT TO VALUE(pcMessageFile).
    PUT UNFORMATTED cTekst
      SKIP.
    OUTPUT CLOSE.

    FILE-INFO:FILE-NAME = pcMessageFile.
    pcMessageFile = FILE-INFO:FULL-PATHNAME.
    
    FILE-INFO:FILE-NAME = icPdfFil.
    icPdfFil = FILE-INFO:FULL-PATHNAME.
    
    rStandardFunksjoner:SkrivTilLogg(cLogg,
                  '    Vedlegg ' + icPdfFil + '.' 
                  ).   
    rStandardFunksjoner:SkrivTilLogg(cLogg,
                  '    Meldingstekst ' + pcMessageFile + '.' 
                  ).   
    rStandardFunksjoner:SkrivTilLogg(cLogg,
                  '    pcparLOGFILE ' + pcparLOGFILE + '.' 
                  ).   
    
    rSendEMail:parToADDRESS       = ceMailAdr.
    rSendEMail:parMailType        = 'PAKKSEDDEL'.
    rSendEMail:parSUBJECT         = cEmne.
    rSendEMail:parMessage-Charset = 'iso-8859-1'. /* Blank eller 'UTF-8' når det går fra fil. */
    rSendEMail:parMessage-File    =  pcMessageFile.
    rSendEMail:parFILE            = FILE-INFO:FULL-PATHNAME + (IF icPkSdlFil <> '' THEN ',' ELSE '') + icPkSdlFil.
    rSendEMail:parLOGFILE         = pcparLOGFILE.  
    rSendEMail:send( ).


END PROCEDURE.

