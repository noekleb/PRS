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
               JBoxSession:Instance:UserId
               .    
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

RUN SkapaEtikettLogg.
IF obOk THEN
  DO:
    IF bTest THEN  
      rStandardFunksjoner:SkrivTilLogg(cLogg,
          '    Starter: x-etikettsend.w (' + STRING(iPrinter) + ').' 
          ).    
    RUN x-etikettsend.w (iPrinter).
      rStandardFunksjoner:SkrivTilLogg(cLogg,
          '    Ferdig: x-etikettsend.w (' + STRING(iPrinter) + ').' 
          ).    
  END.
ASSIGN 
  obOk     = TRUE
  ocReturn = ''
  .

rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Slutt.' 
    ).    


/* **********************  Internal Procedures  *********************** */

PROCEDURE SkapaEtikettLogg:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cStrekkoderTilUtskrift AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cAntallPerStrekKode    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iCount                 AS INTEGER   NO-UNDO.
  DEFINE VARIABLE iTmpAnt                AS INTEGER   NO-UNDO.
  DEFINE VARIABLE cInfoRad1              AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cInfoRad2              AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cInfoRad3              AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cInfoRad4              AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cLagerEan              AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cLagerStr              AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iCOunt2                AS INTEGER    NO-UNDO.
  DEFINE VARIABLE lTilbud                AS LOGICAL    NO-UNDO.
  
  DO:
    IF NOT iKampanjeId > 0 THEN
        lTilbud = IF iPristype = 1 THEN FALSE ELSE ArtPris.Tilbud.
    IF iKampanjeId > 0 THEN DO:
        ASSIGN iCount = 1. /* för att få in en extra etikket innan */
        FIND KampanjeHode WHERE KampanjeHode.KampanjeId = iKampanjeId NO-LOCK NO-ERROR.
        IF AVAIL KampanjeHode THEN DO:
            FOR EACH KampanjeLinje OF KampanjeHode NO-LOCK:
                FIND ArtBas WHERE ArtBas.ArtikkelNr = KampanjeLinje.ArtikkelNr NO-LOCK NO-ERROR.
                IF AVAIL ArtBas THEN DO:
                    FIND ArtPris WHERE ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
                                       ArtPris.ProfilNr   = KampanjeLinje.ProfilNr NO-LOCK NO-ERROR.
                    IF AVAIL Artpris THEN FOR EACH StrekKode OF ArtBas NO-LOCK WHERE StrekKode.KodeType > 0:
                        FIND StrKonv OF StrekKode NO-LOCK NO-ERROR.
                        IF AVAIL StrKonv THEN DO:
                            ASSIGN iTmpAnt = 0.
                            FOR EACH ArtLag NO-LOCK WHERE ArtLag.ArtikkelNr = ArtBas.ArtikkelNr AND
                                                          ArtLag.Storl      = StrKonv.Storl AND
                                                          ArtLag.Butik      = iButNr AND
                                                          ArtLag.lagant     > 0:
                                ASSIGN iTmpAnt = iTmpAnt + ArtLag.LagAnt.
                            END.
                            IF iTmpAnt > 0 THEN DO:
                                CREATE TT_StrekKoder.
                                ASSIGN iCount                = iCount + 1 
                                       TT_StrekKoder.iLopnr  = iCount
                                       TT_StrekKoder.cKode   = StrekKode.Kode
                                       TT_StrekKoder.iAntall = iTmpAnt
                                       TT_StrekKoder.Vg      = ArtBas.Vg   
                                       TT_StrekKoder.LopNr   = ArtBas.Lopnr
                                       TT_StrekKoder.Bongtekst = ArtBas.Bongtekst                             
                                       TT_StrekKoder.Pris      = KampanjeLinje.Pris[2]
                                       TT_StrekKoder.Pris2     = IF KampanjeHode.NormalPris THEN 0 ELSE ArtPris.Pris[1].
                            END.
                        END.
                    END.
                END.
            END.
        END.
    END.
    ELSE IF NOT tgAlle THEN DO:
        IF tgLager THEN DO:
            IF StrekKode.Strkode = 0 THEN DO:
              ASSIGN 
                obOk = FALSE 
                ocReturn = "Strekkode ikke knyttet til størrelse"
                .
                RETURN.
            END.
            FIND StrKonv OF StrekKode NO-LOCK NO-ERROR.
            IF NOT AVAIL StrKonv THEN
                LEAVE.
            FIND butiker WHERE butiker.butik = iButNr NO-LOCK NO-ERROR.
            FIND ArtPris NO-LOCK WHERE
                ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
                ArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.
            IF NOT AVAILABLE ArtPris THEN
              FIND ArtPris NO-LOCK WHERE
                ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
                ArtPris.ProfilNr   = clButiker.ProfilNr NO-ERROR.
            lTilbud = IF iPristype = 1 THEN FALSE ELSE ArtPris.Tilbud.
     
            FOR EACH ArtLag NO-LOCK WHERE ArtLag.ArtikkelNr = ArtBas.ArtikkelNr AND
                                          ArtLag.Storl      = StrKonv.Storl AND
                                          ArtLag.Butik      = iButNr AND
                                          ArtLag.lagant     > 0:
                ASSIGN iTmpAnt = iTmpAnt + ArtLag.LagAnt.
            END.
            IF iTmpAnt > 0 THEN DO:
                CREATE TT_StrekKoder.
                ASSIGN TT_StrekKoder.iLopnr  = 1
                       TT_StrekKoder.cKode   = cStrekkode
                       TT_StrekKoder.iAntall = iTmpAnt
                       TT_StrekKoder.Vg      = ArtBas.Vg   
                       TT_StrekKoder.LopNr   = ArtBas.Lopnr
                       TT_StrekKoder.Bongtekst = ArtBas.Bongtekst
                       TT_StrekKoder.Pris      = ArtPris.Pris[IF lTilbud THEN 2 ELSE 1]
                       TT_StrekKoder.Pris2     = IF lTilbud THEN ArtPris.Pris[1] ELSE 0.
            END.
        END.
        ELSE DO:
            CREATE TT_StrekKoder.
            ASSIGN TT_StrekKoder.iLopnr  = 1
                   TT_StrekKoder.cKode   = cStrekkode
                   TT_StrekKoder.iAntall = iAntEti
                   TT_StrekKoder.Vg      = ArtBas.Vg   
                   TT_StrekKoder.LopNr   = ArtBas.Lopnr
                   TT_StrekKoder.Bongtekst = ArtBas.Bongtekst                             
                   TT_StrekKoder.Pris      = ArtPris.Pris[IF lTilbud THEN 2 ELSE 1]
                   TT_StrekKoder.Pris2     = IF lTilbud THEN ArtPris.Pris[1] ELSE 0.
        END.
    END.
    ELSE DO:
        FOR EACH StrekKode OF ArtBas NO-LOCK WHERE StrekKode.KodeType > 0 AND 
                                               NOT StrekKode.Kode BEGINS "02":
            IF NOT CAN-DO(cLagerStr,STRING(StrekKode.StrKode)) THEN DO:
                ASSIGN cLagerEan = cLagerEan + (IF cLagerEan <> "" THEN "," ELSE "") + StrekKode.Kode
                       cLagerStr = cLagerStr + (IF cLagerStr <> "" THEN "," ELSE "") + STRING(StrekKode.StrKode).
            END.
        END.
        FOR EACH StrekKode OF ArtBas NO-LOCK WHERE StrekKode.KodeType > 0 AND 
                                               StrekKode.Kode BEGINS "02":
            IF NOT CAN-DO(cLagerStr,STRING(StrekKode.StrKode)) THEN DO:
                ASSIGN cLagerEan = cLagerEan + (IF cLagerEan <> "" THEN "," ELSE "") + StrekKode.Kode
                       cLagerStr = cLagerStr + (IF cLagerStr <> "" THEN "," ELSE "") + STRING(StrekKode.StrKode).
            END.
        END.
        DO iCount2 = 1 TO NUM-ENTRIES(cLagerEan):
            FIND StrekKode WHERE StrekKode.Kode = ENTRY(iCount2,cLagerEan) NO-LOCK.
/*         FOR EACH StrekKode OF ArtBas NO-LOCK WHERE StrekKode.KodeType > 0: */
/*             FOR EACH StrekKode OF ArtBas WHERE StrekKode.KodeType = 1: */
            ASSIGN iTmpAnt = 0.
            IF tgLager THEN DO:
                FIND butiker WHERE butiker.butik = iButNr NO-LOCK NO-ERROR.
                FIND ArtPris NO-LOCK WHERE
                    ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
                    ArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.
                IF NOT AVAILABLE ArtPris THEN
                  FIND ArtPris NO-LOCK WHERE
                    ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
                    ArtPris.ProfilNr   = clButiker.ProfilNr NO-ERROR.
                lTilbud = IF iPristype = 1 THEN FALSE ELSE ArtPris.Tilbud.
                FIND StrKonv OF StrekKode NO-LOCK NO-ERROR.
                IF AVAIL StrKonv THEN DO:
                    FOR EACH ArtLag NO-LOCK WHERE ArtLag.ArtikkelNr = ArtBas.ArtikkelNr AND
                                                  ArtLag.Storl      = StrKonv.Storl AND
                                                  ArtLag.Butik      = iButNr AND
                                                  ArtLag.lagant     > 0:
                        ASSIGN iTmpAnt = iTmpAnt + ArtLag.LagAnt.
                    END.
                END.
            END.
            ELSE 
                ASSIGN iTmpAnt = iAntEti.
            IF iTmpAnt > 0 THEN DO:
                CREATE TT_StrekKoder.
                ASSIGN iCount                = iCount + 1 
                       TT_StrekKoder.iLopnr  = iCount
                       TT_StrekKoder.cKode   = StrekKode.Kode
                       TT_StrekKoder.iAntall = iTmpAnt
                       TT_StrekKoder.Vg      = ArtBas.Vg   
                       TT_StrekKoder.LopNr   = ArtBas.Lopnr
                       TT_StrekKoder.Bongtekst = ArtBas.Bongtekst                             
                       TT_StrekKoder.Pris      = ArtPris.Pris[IF lTilbud THEN 2 ELSE 1]
                       TT_StrekKoder.Pris2     = IF lTilbud THEN ArtPris.Pris[1] ELSE 0.
            END.
        END.
    END.

    IF NOT CAN-FIND(FIRST TT_StrekKoder)THEN 
    DO:
      ASSIGN 
        obOk = FALSE 
        ocReturn = "Inget etiketter til utskrift"
        .
        RETURN.
    END.
    IF iStartEtikett > 1 THEN DO:
        CREATE EtikettLogg.
        ASSIGN EtikettLogg.Butik = 0
               EtikettLogg.SeqNr = 0
               EtikettLogg.Storl  = "STARTETIKETT"
               EtikettLogg.Ant = iStartEtikett.
        /* Ant avänds för att ange startetikett */
    END.
    IF iKampanjeId > 0 AND AVAIL Kampanjehode THEN DO:
        FIND butiker WHERE butiker.butik = iButNr NO-LOCK NO-ERROR.
        ASSIGN cInfoRad1 = "KAMPANJE " + STRING(iKampanjeId)
               cInfoRad2 = IF AVAIL kampanjehode THEN STRING(KampanjeHode.StartDato) + " - " +
                             STRING(KampanjeHode.SluttDato) ELSE ""
               cInfoRad3 = IF AVAIL Butiker THEN Butiker.butnamn ELSE ""
               cInfoRad4 = "SLUTT".
        CREATE EtikettLogg.
        ASSIGN
          EtikettLogg.Butik     = Butiker.Butik
          EtikettLogg.Vg        = 0   
          EtikettLogg.LopNr     = 0
          EtikettLogg.Ant       = 0
          EtikettLogg.Storl     = "INFO"
          EtikettLogg.Bongtekst = cInfoRad1 + CHR(1) + cInfoRad2 + CHR(1) + cInfoRad3 + CHR(1) + cInfoRad4
          EtikettLogg.Pris      = 0
          EtikettLogg.Pris2     = 0
          EtikettLogg.SeqNr     = 1.
    END.
    FOR EACH TT_StrekKoder:
        CREATE EtikettLogg.
        ASSIGN
          EtikettLogg.Butik     = Butiker.Butik
          EtikettLogg.Vg        = TT_StrekKoder.Vg   
          EtikettLogg.LopNr     = TT_StrekKoder.LopNr
          EtikettLogg.Ant       = TT_StrekKoder.iAntall
          EtikettLogg.Storl     = TT_StrekKoder.cKode
          EtikettLogg.Bongtekst = TT_StrekKoder.Bongtekst
          EtikettLogg.Pris      = TT_StrekKoder.Pris
          EtikettLogg.Pris2     = TT_StrekKoder.Pris2
          EtikettLogg.SeqNr     = TT_StrekKoder.iLopnr.
    END.
    IF iKampanjeId > 0 AND AVAIL Kampanjehode THEN DO:
        ASSIGN cInfoRad4 = "START".
        CREATE EtikettLogg.
        ASSIGN
          EtikettLogg.Butik     = Butiker.Butik
          EtikettLogg.Vg        = 0   
          EtikettLogg.LopNr     = 0
          EtikettLogg.Ant       = 0
          EtikettLogg.Storl     = "INFO"
          EtikettLogg.Bongtekst = cInfoRad1 + CHR(1) + cInfoRad2 + CHR(1) + cInfoRad3 + CHR(1) + cInfoRad4
          EtikettLogg.Pris      = 0
          EtikettLogg.Pris2     = 0
          EtikettLogg.SeqNr     = 99000.
    END.
  END.
  
  IF bTest THEN 
  DO:
    IF CAN-FIND(FIRST TT_StrekKoder) THEN 
      TEMP-TABLE TT_StrekKoder:WRITE-JSON('file', 'konv\ETIKETT_TT_StrekKoder' + REPLACE(STRING(TODAY),'/','') + '_' + REPLACE(STRING(TIME,"HH:MM:SS"),':','') + ".json", TRUE).
    IF CAN-FIND(FIRST EtikettLogg) THEN 
      TEMP-TABLE EtikettLogg:WRITE-JSON('file', 'konv\ETIKETT_EtikettLogg' + REPLACE(STRING(TODAY),'/','') + '_' + REPLACE(STRING(TIME,"HH:MM:SS"),':','') + ".json", TRUE).
  END.
  
  ASSIGN 
    obOk = TRUE 
    ocReturn = ''
    .
END PROCEDURE.
