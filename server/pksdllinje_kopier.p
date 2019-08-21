/* pksdllinje_kopier.p

-----------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER icParam     AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ihBuffer    AS HANDLE NO-UNDO.
DEFINE INPUT  PARAMETER icSessionId AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER ocReturn    AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER obOK        AS LOG NO-UNDO.

DEFINE VARIABLE lPkSdlId AS DECIMAL NO-UNDO.
DEFINE VARIABLE iLinjeNr AS INTEGER NO-UNDO.
DEFINE VARIABLE iNyttLinjeNr AS INTEGER NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.

DEFINE BUFFER bufPkSdlLinje FOR PkSdlLinje.

ASSIGN 
    obOk         = TRUE
    lPkSdlId     = INT(ENTRY(1,icParam,'|'))  
    iLinjeNr     = INT(ENTRY(2,icParam,'|'))
    iNyttLinjeNr = INT(ENTRY(3,icParam,'|')) 
    cLogg        = 'pksdllinje_kopier' + REPLACE(STRING(TODAY),'/','') 
    .

rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.
rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Start' 
    ).    
rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    Parametre: ' + icParam 
    ).    

DO TRANSACTION:
  FIND PkSdlLinje EXCLUSIVE-LOCK WHERE 
    PkSdlLinje.PkSdlId = lPkSdlId AND 
    PkSdlLinje.PkSdlLinjeId = iLinjeNr NO-ERROR.
  IF AVAILABLE PkSdlLinje THEN 
  DO:
    CREATE bufPkSdlLinje.
    BUFFER-COPY PkSdlLinje 
      EXCEPT PkSdlLinjeId LinjeNr
      TO bufPkSdlLinje
      ASSIGN 
        bufPkSdlLinje.PkSdlLinjeId = iNyttLinjeNr
        bufPkSdlLinje.LinjeNr      = iNyttLinjeNr
        .
    ASSIGN 
      obOk = TRUE
      ocReturn = ''
      .
  END.
  ELSE 
    ASSIGN 
      obOk     = FALSE 
      ocReturn = '**Finner ikke linje det skal kopieres fra. PkSdlId: ' + STRING(lPkSdlId) + ' LinjeNr: ' + STRING(iLinjeNr) + '.'
      .
      
END.  

rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Slutt' 
    ).    
