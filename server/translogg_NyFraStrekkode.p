/* translogg_NyFraStrekkode.p

-----------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER icParam     AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ihBuffer    AS HANDLE NO-UNDO.
DEFINE INPUT  PARAMETER icSessionId AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER ocReturn    AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER obOK        AS LOG NO-UNDO.

DEFINE VARIABLE iBatchNr            AS INTEGER                        NO-UNDO.
DEFINE VARIABLE iButNr              AS INTEGER                        NO-UNDO.
DEFINE VARIABLE iTransNr            AS INTEGER                        NO-UNDO.
DEFINE VARIABLE iSeqNr              AS INTEGER                        NO-UNDO.
DEFINE VARIABLE cStrekkode          AS CHARACTER                      NO-UNDO.
DEFINE VARIABLE cLogg               AS CHARACTER                      NO-UNDO.
DEFINE VARIABLE lVVareKost          AS DECIMAL                        NO-UNDO.
DEFINE VARIABLE iFeilKode           AS INTEGER                        NO-UNDO.
DEFINE VARIABLE iTTId               AS INTEGER                        NO-UNDO.
DEFINE VARIABLE iTBId               AS INTEGER                        NO-UNDO.
DEFINE VARIABLE bTest               AS LOG                            NO-UNDO.
DEFINE VARIABLE lAntall             AS DECIMAL                        NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.

DEFINE BUFFER bufTranslogg FOR Translogg.

/*STRING(BatchLogg.BatchNr) + "|" + fcStrekkode:SCREEN-VALUE + '|1' + '|16'*/
ASSIGN 
  bTest      = TRUE
  iBatchNr   = DEC(ENTRY(1,icParam,'|'))  
  cStrekkode = ENTRY(2,icParam,'|')
  iSeqNr     = INT(ENTRY(3,icParam,'|'))  
  iButNr     = INT(ENTRY(4,icParam,'|'))  
  iTTId      = INT(ENTRY(5,icParam,'|'))  
  lAntall    = DEC(ENTRY(6,icParam,'|'))
  cLogg      = 'Translogg_NyFraStrekkode' + REPLACE(STRING(TODAY),'/','') 
  iTbId      = 1
    NO-ERROR.

rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.
IF bTest THEN 
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Start' 
    ).    
IF bTest THEN 
DO:
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '  Parametre: ' + icParam 
    ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    iBatchNr: ' + STRING(iBatchNr) 
    ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    cStrekkode: ' + cStrekkode 
    ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    iSeqNr: ' + STRING(iSeqNr) 
    ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    iButNr: ' + STRING(iButNr) 
    ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    iTTId: ' + STRING(iTTId) 
    ).    
END.
FIND StrekKode NO-LOCK WHERE 
  StrekKode.Kode = cStrekkode NO-ERROR.
IF NOT AVAILABLE StrekKode THEN 
DO:
  ASSIGN 
    obOk     = FALSE 
    ocReturn = '** Ukjent strekkode!'
    .
  IF bTest THEN 
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  ocReturn: ' + ocReturn 
      ).    
  RETURN.
END.

DO TRANSACTION:
  FIND ArtBas NO-LOCK WHERE 
    ArtBas.ArtikkelNr = StrekKode.ArtikkelNr NO-ERROR.
  IF NOT AVAILABLE ArtBas THEN 
  DO:
    ASSIGN 
      obOk     = FALSE 
      ocReturn = '** Ukjent Artikkel på strekkoden!'
      .
    IF bTest THEN 
      rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  ocReturn: ' + ocReturn 
        ).    
    RETURN.
  END. 
  FIND FIRST StrKonv NO-LOCK WHERE 
    StrKonv.StrKode = Strekkode.StrKode NO-ERROR.
    
  FIND FIRST Translogg EXCLUSIVE-LOCK WHERE 
    TransLogg.BatchNr = iBatchNr AND 
    TransLogg.butik = ibutNr AND 
    TransLogg.TransNr >= 0 AND 
    TransLogg.SeqNr = 1 AND 
    TransLogg.Kode = Strekkode.Kode 
    NO-WAIT NO-ERROR.
  IF NOT AVAILABLE TransLogg THEN 
  DO:
    FIND LAST TransLogg NO-LOCK WHERE 
      TransLogg.Butik = ibutNr AND 
      TransLogg.TransNr >= 0 AND 
      TransLogg.SeqNr = 1 USE-INDEX TransLogg NO-ERROR.
    IF AVAILABLE TransLogg THEN 
      iTransNr = TransLogg.TransNr + 1.
    ELSE 
      iTransNr = 1.
    
    IF bTest THEN 
      rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  Nytt transnr: ' + STRING(iTransNr) 
        ).    
    /* Oppretter TransLogg */    
    CREATE TransLogg.
    NYTRANSLOGG:
    DO WHILE TRUE ON ERROR UNDO, RETRY:
      ASSIGN 
        TransLogg.Butik      = iButNr
        TransLogg.TransNr    = iTransNr
        TransLogg.SeqNr      = 1
        TransLogg.BatchNr    = iBatchNr
        TransLogg.TTId       = iTTId
        TransLogg.TBId       = iTbId
        TransLogg.Storl      = (IF AVAILABLE StrKonv THEN StrKonv.Storl ELSE '')
        TransLogg.ArtikkelNr = ArtBas.ArtikkelNr
        TransLogg.Kode       = cStrekkode
        Translogg.Postert    = FALSE 
               NO-ERROR.
      IF ERROR-STATUS:ERROR THEN
        iTransNr = iTransNr + 1.
      ELSE LEAVE NYTRANSLOGG.
    END. /* NYTRANSLOGG */
  END.
  
  ASSIGN 
    TransLogg.Antall = TransLogg.antall + lAntall
    .
  
  ASSIGN 
    obOk     = TRUE
    ocReturn = STRING(TransLogg.Butik) + '|' + 
                 STRING(TransLogg.TransNr) + '|' +
                 STRING(TransLogg.SeqNr)
    .
  /* Beriker translogg. */  
  ihbuffer = BUFFER Translogg:HANDLE.    
  RUN translogg_post_update.p(ihBuffer,
    'update',
    '',
    OUTPUT ocReturn
    ).
  IF bTest THEN 
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  Fra translogg_post_update.p: ' + ocReturn 
      ).    
                                     
END. /* TRANSACTION */  
IF bTest THEN 
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Slutt' 
    ).    
