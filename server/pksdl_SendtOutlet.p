/* Hent merkelapper for pakkseddel
   Parameter:  <PkSdlId>
   Opprettet: 07.09.07 av BHa              
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEFINE VARIABLE bAvbestill AS LOG NO-UNDO.

DEF VAR hQuery          AS HANDLE NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.
DEFINE VARIABLE rSendEMail          AS cls.SendEMail.SendEMail        NO-UNDO.

DEFINE TEMP-TABLE ttBestillt
  FIELD PkSdlNr AS CHARACTER
  FIELD SO AS INTEGER FORMAT ">9" 
  FIELD ButikkNr AS INTEGER
  FIELD cOrdretype AS CHARACTER 
  FIELD cOpphav AS CHARACTER 
  FIELD cPalleNr AS CHARACTER 
  FIELD Lokasjon AS CHARACTER 
  FIELD Varetype AS CHARACTER 
  FIELD LandedCost AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99"
  FIELD SesongLst AS CHARACTER FORMAT "x(40)" 
  FIELD DatoTid AS DATETIME FORMAT "99/99/99 HH:MM:SS"
  .

DEFINE STREAM Ut.

ASSIGN 
    ocReturn  = ""
    cLogg = 'pksdl_SendtOutlet' + REPLACE(STRING(TODAY),'/','')
    .

IF icParam = '' /*OR NOT CAN-FIND(Butiker WHERE 
                                Butiker.butik = INT(icParam)
                                ) */ THEN 
  RETURN.                                
IF NUM-ENTRIES(icParam,'|') = 2 THEN 
DO:
  ASSIGN
    bAvbestill = TRUE 
    icParam    = ENTRY(1,icParam,'|')
    .
END. 

rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.
rSendEMail  = NEW cls.SendEMail.SendEMail( ) NO-ERROR.

rStandardFunksjoner:SkrivTilLogg(cLogg,
  'Start.' 
  ).

EMPTY TEMP-TABLE ttBestillt.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " WHERE TRUE").
hQuery:QUERY-OPEN().

hQuery:GET-FIRST().


/* Sjekker bestilling */
IF CAN-DO('10,11',icParam) OR bAvbestill THEN 
BLOKKEN:
REPEAT WHILE NOT hQuery:QUERY-OFF-END TRANSACTION:
  FIND PkSdlHode EXCLUSIVE-LOCK WHERE
    PkSdlHode.PkSdlId = DECIMAL(ihBuffer:BUFFER-FIELD("PkSdlId"):BUFFER-VALUE) NO-ERROR.
    
  IF AVAILABLE PkSdlHode THEN 
  DO:
    /* Sjekker bestillingsmail. */
    IF CAN-DO('10,11',STRING(PkSdlHode.SendtOutlet)) AND bAvbestill = FALSE  THEN
      ocReturn = '** Disse pakkseddlene er allerede bestillt: ' + ocReturn + 
                 (IF ocReturn = '' THEN '' ELSE ',') + 
                 PkSdlHode.PkSdlNr. 
    /* Sjekker avbestillingsmail. */
    IF NOT CAN-DO('10,11',STRING(PkSdlHode.SendtOutlet)) AND bAvbestill = TRUE  THEN
      ocReturn = '** Disse pakkseddlene står ikke som bestilt: ' + ocReturn + 
                 (IF ocReturn = '' THEN '' ELSE ',') + 
                 PkSdlHode.PkSdlNr. 
    
    RELEASE PkSdlHode.
  END.

  hQuery:GET-NEXT().
END. /* BLOKKEN */
IF ocReturn <> '' THEN 
  ocReturn = ocReturn + (IF bAvbestill THEN '. Bestilling AVBRUTT!' ELSE 'AvBestilling AVBRUTT!'). 

IF ocReturn = '' THEN 
DO:
  hQuery:GET-FIRST().
  BLOKKEN:
  REPEAT WHILE NOT hQuery:QUERY-OFF-END TRANSACTION:
    FIND PkSdlHode EXCLUSIVE-LOCK WHERE
      PkSdlHode.PkSdlId = DECIMAL(ihBuffer:BUFFER-FIELD("PkSdlId"):BUFFER-VALUE) NO-ERROR.
      
    IF AVAILABLE PkSdlHode THEN 
    DO:
      /* Sender bestillingsmail. */
      IF CAN-DO('10,11',STRING(PkSdlHode.SendtOutlet)) AND INT(icParam) = 1 THEN 
        RUN settBestillt.
  
      rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  PkSld: ' + PkSdlHode.PkSdlNr + ' Endret fra ' + STRING(PkSdlHode.SendtOutlet) + ' til SO = ' + icParam + '.' 
        ).
  
      ASSIGN 
        PkSdlHode.SendtFraLagerTilOutlet = (IF INT(icParam) = 0 THEN ? ELSE NOW)
        PkSdlHode.SendtOutlet            = INT(icParam)
        .
  
      /* Sender bestillingsmail. */
      IF CAN-DO('10,11',icParam) THEN 
        RUN settBestillt.
  
      RELEASE PkSdlHode.
    END.
  
    hQuery:GET-NEXT().
  END. /* BLOKKEN */
END.

DELETE OBJECT hQuery NO-ERROR.

obOk = ocReturn = ''.

IF CAN-FIND(FIRST ttBestillt) THEN 
  RUN sendeBestillingsMail.

EMPTY TEMP-TABLE ttBestillt.

rStandardFunksjoner:SkrivTilLogg(cLogg,
  'Slutt.' 
  ).


/* **********************  Internal Procedures  *********************** */

PROCEDURE sendeBestillingsMail:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cFilNavn AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cPrefix AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cTekst  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cparTOAdress AS CHARACTER NO-UNDO.

  DEFINE BUFFER bufttBestillt FOR ttBestillt.
    
  FIND FIRST bufttBestillt NO-ERROR.

  IF SEARCH('tnc.txt') <> ? THEN
    ASSIGN
      cPrefix = 'TEST '
      cparTOAdress = 'tomn@nsoft.no'
      .
  ELSE DO:
    ASSIGN
      cPrefix = ''
/*      cparTOAdress = IF bufttBestillt.ButikkNr = 10 THEN                                                                   */
/*                       'henrikh@gant.no;gant.outlet@gantretail.no;freestock@gant.no;are@gant.no;tomn@nsoft.no'             */
/*                     ELSE                                                                                                  */
/*                       'gant.stavanger@gantretail.no;gant.outlet@gantretail.no;freestock@gant.no;are@gant.no;tomn@nsoft.no'*/
      .
    IF bufttBestillt.ButikkNr = 10 THEN 
      {syspara.i 50 50 50 cparTOAdress}
    ELSE IF bufttBestillt.ButikkNr = 40 THEN    
      {syspar2.i 50 50 50 cparTOAdress}
  END.
  
  CASE bufttBestillt.SO:
    WHEN 1 THEN
      cTekst = cPrefix + 'AVBESTILLING ' + (IF bufttBestillt.ButikkNr = 10 THEN 'Vestby' ELSE 'Algard') + ' Dato/Tid: ' + STRING(NOW,"99/99/99 hh:mm:ss") + '.'.
    WHEN 10 THEN
      cTekst = cPrefix + 'VAREBESTILLING Vestby. Dato/Tid: ' + STRING(NOW,"99/99/99 hh:mm:ss") + '.'.
    WHEN 11 THEN
      cTekst = cPrefix + 'VAREBESTILLING Algard. Dato/Tid: ' + STRING(NOW,"99/99/99 hh:mm:ss") + '.'.
  END CASE.

  cFilNavn = rStandardFunksjoner:getTempFileName().
  OUTPUT STREAM Ut TO VALUE(cFilNavn).
    PUT STREAM Ut UNFORMATTED 
      cTekst
      SKIP.
    PUT STREAM Ut UNFORMATTED 
      FILL('-',LENGTH(cTekst) + 25)
      SKIP.
  PUT STREAM Ut  
    'Pakkseddel  ' FORMAT "x(12)"
    'Ordretype   ' FORMAT "x(12)"
    'Opphav      ' FORMAT "x(12)"
    'PalleNr        ' FORMAT "x(15)"
    'Lokasjon                 ' FORMAT "x(25)"
    'Varetype                 ' FORMAT "x(25)"    
    'LSesong                  ' FORMAT "x(40)"
    SKIP.
  PUT STREAM Ut  
    '------------' FORMAT "x(12)"
    '------------' FORMAT "x(12)"
    '------------' FORMAT "x(12)"
    '---------------' FORMAT "x(15)"
    '-------------------------' FORMAT "x(25)"
    '-------------------------' FORMAT "x(25)"
    '-------------------------' FORMAT "x(40)"
    SKIP.
  FOR EACH ttBestillt
    BY ttBestillt.PkSdlNr:
    PUT STREAM Ut  
      ttBestillt.PkSdlNr FORMAT "x(12)"  
      ttBestillt.cOrdretype FORMAT "x(12)"
      ttBestillt.cOpphav FORMAT "x(12)"
      ttBestillt.cPalleNr FORMAT "x(15)" 
      ttBestillt.Lokasjon FORMAT "x(25)" 
      ttBestillt.Varetype FORMAT "x(25)"
      ttBestillt.SesongLst FORMAT "x(40)"
      SKIP.
  END.
  OUTPUT STREAM Ut CLOSE.    

  rSendEMail:parToADDRESS       = cparTOAdress.  
  rSendEMail:parMessage-Charset = 'iso-8859-1'. /* Blank eller 'UTF-8' når det går fra fil. */
  rSendEMail:parMessage-File    = cFilNavn. 
  rSendEMail:parMESSAGE         =  ''.
  rSendEMail:parMailType        = 'FakturaMail'.
  rSendEMail:parSUBJECT         = cTekst. 
  rSendEMail:parFILE            = ''.
  IF rSendEMail:parToADDRESS <> '' THEN   
    rSendEMail:send( ).

END PROCEDURE.

PROCEDURE settBestillt:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE VARIABLE pcTekst AS CHARACTER NO-UNDO.
  
  IF AVAILABLE PkSdlHode THEN 
  DO:
/*    pcTekst = ''.                                       */
/*    FOR EACH PksdlLinje OF PkSdlHode NO-LOCK,           */
/*      FIRST ArtBas NO-LOCK OF PkSdlLinje:               */
/*      IF NOT CAN-DO(pcTekst, STRING(ArtBas.Sasong)) THEN*/
/*        ASSIGN                                          */
/*          pcTekst = pcTekst +                           */
/*                    (IF pctekst = '' THEN '' ELSE ',') +*/
/*                    STRING(ArtBas.Sasong).              */
/*     END.                                               */
    
    CREATE ttBestillt.
    ASSIGN 
      ttBestillt.PkSdlNr    = PkSdlHode.PkSdlNr
      ttBestillt.ButikkNr   = PkSdlHode.butikkNr
      ttBestillt.SO         = PkSdlHode.SendtOutlet
      ttBestillt.cOrdreType = (IF CAN-DO('1,12',STRING(PkSdlHode.OrdreType)) THEN
                                   'Forward'
                                 ELSE IF DEC(PkSdlHode.OrdreType) = 0 THEN
                                   'Overskuddsvarer'
                                 ELSE
                                   'Stock')
      ttBestillt.cPalleNr   = PkSdlHode.cPalleNr
      ttBestillt.Lokasjon   = PkSdlHode.Lokasjon
      ttBestillt.Varetype   = PkSdlHode.VareType
      ttBestillt.LandedCost = PkSdlHode.LandedCost
      ttBestillt.DatoTid    = NOW
      ttBestillt.SesongLst  = PkSdlHode.LagerSesong /* pcTekst*/ 
      .
  END.
  
END PROCEDURE.
