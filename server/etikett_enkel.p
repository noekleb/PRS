/* ovbuffer_opprett.p

      cTekst = ENTRY(LOOKUP(cbPrinter:SCREEN-VALUE,cbPrinter:LIST-ITEMS),cPrinterIdLst) + '|' +
               fcStrekkode:SCREEN-VALUE + '|' + 
               tgAlle:SCREEN-VALUE + '|' + 
               tgLager:SCREEN-VALUE + '|' + 
               iAntEti:SCREEN-VALUE + '|' + 
               iStartEtikett:SCREEN-VALUE + '|' + 
               rsPris:SCREEN-VALUE + '|' +  
               cbButiker:SCREEN-VALUE + '|' +
               STRING(iKampanjeId) + '|' +  
               JBoxSession:Instance:UserId + '|' +
               STRING(rsEtitype) + '|' +
               STRING(iAltBeLayout)
               .    

-----------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER icParam     AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ihBuffer    AS HANDLE NO-UNDO.
DEFINE INPUT  PARAMETER icSessionId AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER ocReturn    AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER obOK        AS LOG NO-UNDO.

DEFINE VARIABLE iBuntNr AS INTEGER NO-UNDO.
DEFINE VARIABLE iLinjeNr AS INTEGER NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.

DEFINE VARIABLE iPristype AS INTEGER NO-UNDO.
DEFINE VARIABLE iKampanjeId AS INTEGER NO-UNDO.
DEFINE VARIABLE cStrekkode AS CHARACTER NO-UNDO.
DEFINE VARIABLE tgAlle AS LOG NO-UNDO.
DEFINE VARIABLE tgLager AS LOG NO-UNDO.
DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.
DEFINE VARIABLE iCL AS INTEGER NO-UNDO.
DEFINE VARIABLE iAntEti AS INTEGER NO-UNDO.
DEFINE VARIABLE iStartEtikett AS INTEGER NO-UNDO.
DEFINE VARIABLE iPrinter AS INTEGER NO-UNDO.
DEFINE VARIABLE iButNr AS INTEGER NO-UNDO.
DEFINE VARIABLE bTest AS LOG NO-UNDO.
DEFINE VARIABLE cBrukerId AS CHARACTER NO-UNDO.
DEFINE VARIABLE iEtiType AS INTEGER NO-UNDO.
DEFINE VARIABLE iAltBeLayout AS INTEGER NO-UNDO.
DEFINE VARIABLE bTilbud AS LOG NO-UNDO.
DEFINE VARIABLE iCount AS INTEGER NO-UNDO.
DEFINE VARIABLE iRFIDEtikett AS INTEGER NO-UNDO.
DEFINE VARIABLE ix AS INTEGER NO-UNDO.
DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE TT_StrekKoder
    FIELD iLopnr AS INTE
    FIELD cKode  AS CHAR
    FIELD iAntall AS INTE
    FIELD Vg        LIKE ArtBas.Vg
    FIELD LopNr     LIKE ArtBas.Lopnr
    FIELD Bongtekst LIKE ArtBas.Bongtekst
    FIELD Pris      AS DECIMAL
    FIELD Pris2     AS DECIMAL
    INDEX Lopnr iLopnr.

DEFINE BUFFER clbutiker FOR Butiker.

ASSIGN 
    cLogg = 'etikett_enkel' + REPLACE(STRING(TODAY),'/','')
    .

rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.

{etikettlogg.i &NEW=NEW}
DEFINE TEMP-TABLE ttEtikettlogg 
  FIELD Strekkode AS CHARACTER
  FIELD Antall AS INTEGER
  FIELD NotatKodeTekst AS CHARACTER 
  .
  
{syspara.i 5 1 1 iCl INT}

IF SEARCH('tnc.txt') <> ? THEN 
  bTest = TRUE.
ELSE 
  bTest = FALSE.
  
IF bTest THEN
DO:   
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      'Start' 
      ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '    Parametre: ' + icParam 
      ).    
END.

ASSIGN 
    iPrinter      = INT(ENTRY(1,icParam,'|'))
    cStrekkode    = ENTRY(2,icParam,'|')
    tgAlle        = IF ENTRY(3,icParam,'|') = 'no' THEN FALSE ELSE TRUE 
    tgLager       = IF ENTRY(4,icParam,'|') = 'no' THEN FALSE ELSE TRUE
    iAntEti       = INTEGER(ENTRY(5,icParam,'|'))
    iStartEtikett = INTEGER(ENTRY(6,icParam,'|'))  
    iPristype     = INTEGER(ENTRY(7,icParam,'|'))
    iButNr        = INTEGER(ENTRY(8,icParam,'|'))
    iKampanjeId   = INTEGER(ENTRY(9,icParam,'|'))
    cBrukerId     = ENTRY(10,icParam,'|')
    iEtiType      = INTEGER(ENTRY(11,icParam,'|'))
    iAltBeLayout  = INTEGER(ENTRY(12,icParam,'|'))
    iRFIDEtikett  = INTEGER(ENTRY(13,icParam,'|'))
    .

IF bTest THEN 
DO:
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '      iPrinter: ' + STRING(iPrinter) 
      ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '      cStrekkode: ' + cStrekkode 
      ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '      tgAlle: ' + STRING(tgAlle) 
      ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '      tgLager: ' + STRING(tgLager) 
      ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '      iAntEti: ' + STRING(iAntEti) 
      ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '      iStartEtikett: ' + STRING(iStartEtikett) 
      ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '      iPristype: ' + STRING(iPristype) 
      ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '      iButNr: ' + STRING(iButNr) 
      ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '      iKampanjeId: ' + STRING(iKampanjeId) 
      ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '      cBrukerId: ' + cBrukerId 
      ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '      iEtiType: ' + STRING(iEtiType) 
      ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '      iAltBeLayout: ' + STRING(iAltBeLayout) 
      ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '      iRFIDEtikett: ' + STRING(iRFIDEtikett) 
      ).    
END.

FIND clButiker NO-LOCK WHERE
  clButiker.Butik = iCl NO-ERROR.
IF NOT AVAILABLE clButiker THEN
  DO:
    ASSIGN 
      ocReturn = "Sentrallager er ikke lagt opp!"
      obOk = FALSE
      .
    IF bTest THEN  
      rStandardFunksjoner:SkrivTilLogg(cLogg,
          '    AVBRYT: ' + ocReturn 
          ).    
    RETURN.
  END.

FIND Butiker NO-LOCK WHERE
  Butiker.Butik = iButNr NO-ERROR.
IF NOT AVAILABLE Butiker OR iButNr = 0 THEN
  DO:
    ASSIGN 
      ocReturn = "Bruker er ikke koblet til butikk!"
      obOk = FALSE
      . 
    IF bTest THEN  
      rStandardFunksjoner:SkrivTilLogg(cLogg,
          '    AVBRYT: ' + ocReturn 
          ).    
    RETURN.
  END.

FIND Strekkode NO-LOCK WHERE 
  Strekkode.Kode = cStrekkode NO-ERROR.
IF NOT AVAILABLE Strekkode THEN 
DO:
  ASSIGN
    obOk = FALSE 
    ocReturn = 'Ukjent strekkode: ' + cStrekkode + '.' 
    .
    IF bTest THEN  
      rStandardFunksjoner:SkrivTilLogg(cLogg,
          '    AVBRYT: ' + ocReturn 
          ).    
    RETURN.
END.

FIND ArtBas WHERE 
  ArtBas.Artikkelnr = strekkode.Artikkelnr NO-LOCK.
IF NOT AVAILABLE ArtBas THEN 
DO:
  ASSIGN
    obOk = FALSE 
    ocReturn = 'Ukjent artikkel - strekkode ikke koblet til artikkel: ' + cStrekkode + '.' 
    .
    IF bTest THEN  
      rStandardFunksjoner:SkrivTilLogg(cLogg,
          '    AVBRYT: ' + ocReturn 
          ).    
    RETURN.
END.

FIND StrKonv OF StrekKode NO-LOCK NO-ERROR.
IF NOT AVAILABLE StrKonv THEN 
DO:
  ASSIGN
    obOk = FALSE 
    ocReturn = 'Ukjent størrelseskode - strekkode ikke koblet til størrelse: ' + cStrekkode + '.' 
    .
    IF bTest THEN  
      rStandardFunksjoner:SkrivTilLogg(cLogg,
          '    AVBRYT: ' + ocReturn 
          ).    
    RETURN.
END.

FIND ArtPris NO-LOCK WHERE
    ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
    ArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.
IF NOT AVAILABLE ArtPris THEN
  FIND ArtPris NO-LOCK WHERE
    ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
    ArtPris.ProfilNr   = clButiker.ProfilNr NO-ERROR.
IF NOT AVAILABLE ArtPris THEN
  FIND FIRST ArtPris NO-LOCK WHERE
    ArtPris.ArtikkelNr = ArtBas.ArtikkelNr  NO-ERROR.
IF NOT AVAILABLE ArtPris THEN
DO:
  ASSIGN
    obOk = FALSE
    ocReturn = 'Ingen pris knyttet til strekkodens artikkel: ' + cStrekkode + '.'
    .
    IF bTest THEN
      rStandardFunksjoner:SkrivTilLogg(cLogg,
          '    AVBRYT: ' + ocReturn
          ).
    RETURN.
END.


EMPTY TEMP-TABLE ttEtikettlogg.
SKAPELSEN:
DO:
  IF NOT iKampanjeId > 0 THEN
    bTilbud = IF iPristype = 1 THEN FALSE ELSE ArtPris.Tilbud.
  ELSE 
    bTilbud = FALSE.
  iCount = 1.
  CREATE ttEtikettLogg.
  ASSIGN 
    ttEtikettLogg.Strekkode = cStrekkode
    ttEtikettLogg.Antall    = iAntEti
    .
    
    IF iRFIDEtikett = 1 THEN 
    RFID:
    DO:
      /* Betyding av Notatkodetekst
      21 = RFID,Antall fra rad 
      22 = RFID,Antall på lager på EAN
      23 = RFID,Antall på lager Alle Ean
      */
      IF tgAlle = FALSE AND tgLager = FALSE THEN 
        ASSIGN 
          ttEtikettLogg.NotatKodeTekst = '21'.
       ELSE IF tgAlle = FALSE AND tgLager = TRUE THEN  
        ASSIGN 
          ttEtikettLogg.NotatKodeTekst = '22'.
       ELSE 
        ASSIGN 
          ttEtikettLogg.NotatKodeTekst = '23'.
    END. /* RFID */
    ELSE 
    NORMAL:
    DO:
      /* Betyding av Notatkodetekst
      11 = Vanlig,Antall fra rad 
      12 = Vanlig,Antall på lager på EAN
      13 = Vanlig,Antall på lager Alle Ean
      */
      IF tgAlle = FALSE AND tgLager = FALSE THEN 
        ASSIGN 
          ttEtikettLogg.NotatKodeTekst = '11'.
       ELSE IF tgAlle = FALSE AND tgLager = TRUE THEN  
        ASSIGN 
          ttEtikettLogg.NotatKodeTekst = '12'.
       ELSE 
        ASSIGN 
          ttEtikettLogg.NotatKodeTekst = '13'.
    END. /* NORMAL */
END. /* SKAPELSEN */

IF CAN-FIND(FIRST ttEtikettLogg) THEN
  DO:
    IF bTest THEN  
      rStandardFunksjoner:SkrivTilLogg(cLogg,
          '    Starter: asEtikett.p (' + STRING(iPrinter) + ').' 
          ).    
          
    RUN asEtikett.p (iButNr,
                     1, /*1=Enkelt etikett. */
                     TABLE ttEtikettLogg,
                     OUTPUT obOk   
                    ) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN  
    DO ix = 1 TO ERROR-STATUS:NUM-MESSAGES:        
        cTekst = '    ** Feil fra asEtikett.p: '+ STRING(ERROR-STATUS:GET-NUMBER(ix)) + ' ' + ERROR-STATUS:GET-MESSAGE(ix).
        rStandardFunksjoner:SkrivTilLogg(cLogg,
                                        '    ' + cTekst
                                        ).
    END.
END.
ELSE 
  obOk = FALSE.
ASSIGN 
  ocReturn = IF obOk THEN '' ELSE '**Feil ved etikettutskrift.'
  .

rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Slutt.' 
    ).    


/* **********************  Internal Procedures  *********************** */
