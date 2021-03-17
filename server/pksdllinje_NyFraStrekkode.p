/* pksdllinje_NyFraStrekkode.p

-----------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER icParam     AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ihBuffer    AS HANDLE NO-UNDO.
DEFINE INPUT  PARAMETER icSessionId AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER ocReturn    AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER obOK        AS LOG NO-UNDO.

DEFINE VARIABLE lPkSdlId AS INTEGER NO-UNDO.
DEFINE VARIABLE iLinjeNr AS INTEGER NO-UNDO.
DEFINE VARIABLE cStrekkode AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEFINE VARIABLE iButNr AS INTEGER NO-UNDO.
DEFINE VARIABLE lVVareKost AS DECIMAL NO-UNDO.
DEFINE VARIABLE iFeilKode AS INTEGER NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.

DEFINE BUFFER bufPkSdlLinje FOR PkSdlLinje.

ASSIGN 
    lPkSdlId    = DEC(ENTRY(1,icParam,'|'))  
    cStrekkode  = ENTRY(2,icParam,'|')
    iLinjeNr    = INT(ENTRY(3,icParam,'|'))  
    iButNr      = INT(ENTRY(4,icParam,'|'))  
    iFeilKode   = INT(ENTRY(5,icParam,'|'))
    cLogg       = 'pksdllinje_NyFraStrekkode' + REPLACE(STRING(TODAY),'/','') 
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
  IF NOT AVAILABLE ArtBas THEN 
  DO:
    ASSIGN 
      obOk = FALSE 
      ocReturn = '** Ukjent Artikkel på strekkoden!'
      .
    RETURN.
  END. 
  
  FIND Lager NO-LOCK WHERE 
    Lager.ArtikkelNr = ArtBas.ArtikkelNr AND 
    Lager.Butik = iButNr NO-ERROR.
  IF AVAILABLE Lager AND Lager.VVarekost <> ? AND Lager.VVarekost > 0 THEN 
    lVVareKost = Lager.VVarekost.
  ELSE IF AVAILABLE ArtPris THEN 
    lVVareKost = ArtPris.VareKost[1].
     
  FIND FIRST PkSdlLinje EXCLUSIVE-LOCK WHERE 
    PkSdlLinje.PkSdlId = lPkSdlId AND 
    PkSdlLinje.ArtikkelNr = ArtBas.ArtikkelNr AND 
    PkSdlLinje.StrKode = StrekKode.StrKode AND 
    PkSdlLinje.ButikkNr = iButNr NO-ERROR.
  IF AVAILABLE PkSdlLinje THEN 
    ASSIGN 
      PkSdlLinje.Antall    = PkSdlLinje.Antall + 1
      PkSdlLinje.AntLevert = PkSdlLinje.Antall 
      obOk     = TRUE
      ocReturn = STRING(PkSdlLinje.PkSdlId) + '|' + STRING(PkSdlLinje.PkSdlLinjeId)
      .
  ELSE DO:        
    CREATE PkSdlLinje.
    ASSIGN 
      PkSdlLinje.PkSdlId      = lPkSdlId
      PkSdlLinje.PkSdlLinjeId = iLinjeNr
      PkSdlLinje.Linjenr      = PkSdlLinje.PkSdlLinjeId 
      .
    ASSIGN 
      PkSdlLinje.ArtikkelNr    = (IF AVAILABLE ArtBas THEN ArtBas.ArtikkelNr ELSE 0)
      PkSdlLinje.Beskr         = (IF AVAILABLE ArtBas THEN ArtBas.Beskr ELSE '')
      PkSdlLinje.LevFargKod    = (IF AVAILABLE ArtBas THEN ArtBas.LevFargKod ELSE '')
      PkSdlLinje.Antall        = 1
      PkSdlLinje.AntLevert     = 1
      PkSdlLinje.LevKod        = ArtBas.LevKod
      PkSdlLinje.LevNr         = ArtBas.LevNr
      PkSdlLinje.StrKode       = (IF AVAILABLE Strekkode THEN Strekkode.StrKode ELSE 0)
      PkSdlLinje.Kode          = (IF AVAILABLE Strekkode THEN Strekkode.Kode ELSE '')
      PkSdlLinje.Salgsenhet    = ArtBas.SalgsEnhet
      PkSdlLinje.ButikkNr      = iButNr
      PkSdlLinje.Pakke         = FALSE 
      PkSdlLinje.PakkeNr       = 0      
      PkSdlLinje.BestNr        = 0
      PkSdlLinje.OrdreNr       = 0
      
    .
    /* Sjekker eventuelt oppretter pris. */
    RUN pksdllinje_opprett_pris.p (STRING(PkSdlLinje.PkSdlId) + '|' + STRING(PkSdlLinje.PkSdlLinjeId),
                                   ?,
                                   '',
                                   OUTPUT ocReturn,
                                   OUTPUT obOk
                                   ).

    ASSIGN 
      obOk     = TRUE
      ocReturn = ''
      .
  END.
  
END. /* TRANSACTION */  

rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Slutt' 
    ).    
