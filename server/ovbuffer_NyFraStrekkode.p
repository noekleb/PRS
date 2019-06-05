/* ovbuffer_NyFraStrekkode.p

-----------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER icParam     AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ihBuffer    AS HANDLE NO-UNDO.
DEFINE INPUT  PARAMETER icSessionId AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER ocReturn    AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER obOK        AS LOG NO-UNDO.

DEFINE VARIABLE iBuntNr AS INTEGER NO-UNDO.
DEFINE VARIABLE iLinjeNr AS INTEGER NO-UNDO.
DEFINE VARIABLE cStrekkode AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEFINE VARIABLE iFraBut AS INTEGER NO-UNDO.
DEFINE VARIABLE iTilbut AS INTEGER NO-UNDO.
DEFINE VARIABLE lVVareKost AS DECIMAL NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.

DEFINE BUFFER bufOvBuffer FOR OvBuffer.

ASSIGN 
    iBuntNr     = INT(ENTRY(1,icParam,'|'))  
    cStrekkode  = ENTRY(2,icParam,'|')
    iLinjeNr    = INT(ENTRY(3,icParam,'|'))  
    iFraBut     = INT(ENTRY(4,icParam,'|'))  
    iTilBut     = INT(ENTRY(5,icParam,'|'))  
    cLogg       = 'ovbuffer_NyFraStrekkode' + REPLACE(STRING(TODAY),'/','') 
    NO-ERROR.

rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.
rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Start' 
    ).    
rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    Parametre: ' + icParam 
    ).    

FIND StrekKode NO-LOCK WHERE 
  StrekKode.Kode = cStrekkode NO-ERROR.
IF NOT AVAILABLE StrekKode THEN 
DO:
  ASSIGN 
    obOk = FALSE 
    ocReturn = '** Ukjent strekkode!'
    .
  RETURN.
END.

DO TRANSACTION:
  FIND ArtBas NO-LOCK WHERE 
    ArtBas.ArtikkelNr = StrekKode.ArtikkelNr NO-ERROR.
  FIND StrKonv NO-LOCK WHERE 
    StrKonv.StrKode = StrekKode.StrKode NO-ERROR.
  IF NOT AVAILABLE ArtBas AND AVAILABLE StrKonv THEN 
  DO:
    ASSIGN 
      obOk = FALSE 
      ocReturn = '** Ukjent strekkode!'
      .
    RETURN.
  END. 
  
  FIND Lager NO-LOCK WHERE 
    Lager.ArtikkelNr = ArtBas.ArtikkelNr AND 
    Lager.Butik = iFrabut NO-ERROR.
  IF AVAILABLE Lager AND Lager.VVarekost <> ? AND Lager.VVarekost > 0 THEN 
    lVVareKost = Lager.VVarekost.
  ELSE IF AVAILABLE ArtPris THEN 
    lVVareKost = ArtPris.VareKost[1].
     
  FIND FIRST OvBuffer EXCLUSIVE-LOCK WHERE 
    OvBuffer.BuntNr = iBuntNr AND 
    OvBuffer.ArtikkelNr = ArtBas.ArtikkelNr AND 
    OvBuffer.Storl = StrKonv.Storl AND 
    OvBuffer.ButikkNrFra = iFraBut AND 
    OvBuffer.ButikkNrTil = itilBut NO-ERROR.
  IF AVAILABLE OvBuffer THEN 
    ASSIGN 
      OvBuffer.Antall = OvBuffer.Antall + 1
      obOk     = TRUE
      ocReturn = STRING(OvBuffer.BuntNr) + '|' + STRING(OvBuffer.LinjeNr)
      .
  ELSE DO:        
    CREATE OvBuffer.
    ASSIGN 
      OvBuffer.BuntNr  = iBuntNr
      OvBuffer.LinjeNr = iLinjeNr
      .
    ASSIGN 
      Ovbuffer.ArtikkelNr     = ArtBas.ArtikkelNr
      Ovbuffer.Vg             = ArtBas.Vg
      Ovbuffer.LopNr          = ArtBas.LopNr
      Ovbuffer.ButikkNrFra    = iFraBut
      Ovbuffer.ButikkNrTil    = iTilBut
      Ovbuffer.Antall         = 1
      Ovbuffer.Storl          = StrKonv.Storl
      Ovbuffer.TilStorl       = StrKonv.Storl
      Ovbuffer.Merknad        = ""
      Ovbuffer.VareKost       = lVVareKost
      Ovbuffer.Mva%           = IF AVAILABLE ArtPris THEN ArtPris.Mva%[1] ELSE 0.
    .
    ASSIGN 
      obOk     = TRUE
      ocReturn = ''
      .
  END.

  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    OvBuffer: ' + STRING(Ovbuffer.ArtikkelNr) + ' ' + OvBuffer.Storl + '.'   
    ).    
  
END. /* TRANSACTION */  

rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Slutt' 
    ).    
