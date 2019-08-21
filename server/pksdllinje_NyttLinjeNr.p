/* PkSdlLinje_NyttLinjeNr.p

-----------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER icParam     AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ihBuffer    AS HANDLE NO-UNDO.
DEFINE INPUT  PARAMETER icSessionId AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER ocReturn    AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER obOK        AS LOG NO-UNDO.

DEFINE VARIABLE lPkSdlId AS DECIMAL NO-UNDO.
DEFINE VARIABLE iLinjeNr AS INTEGER NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.

ASSIGN 
    obOk        = TRUE
    lPkSdlId    = INT(ENTRY(1,icParam,'|'))  
    cLogg       = 'PkSdlHode_NyttLinjeNr' + REPLACE(STRING(TODAY),'/','') 
    .

rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.
rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Start' 
    ).    
rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    Parametre: ' + icParam 
    ).    

DO:
  FIND LAST PkSdlLinje NO-LOCK WHERE 
    PkSdlLinje.PkSdlId = lPkSdlId USE-INDEX PkSdlLinje NO-ERROR.
  IF AVAILABLE PkSdlLinje THEN 
    iLinjeNr = PkSdlLinje.PkSdlLinjeId + 1.
  ELSE 
    iLinjeNr = 1.
END.

rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    LinjeNr: ' + STRING(iLinjeNr) 
    ).    

ASSIGN 
  obOk = TRUE
  ocReturn = STRING(iLinjeNr)
  .