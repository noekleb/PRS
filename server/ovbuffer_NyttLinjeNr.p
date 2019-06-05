/* ovbuffer_opprett.p

-----------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER icParam     AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ihBuffer    AS HANDLE NO-UNDO.
DEFINE INPUT  PARAMETER icSessionId AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER ocReturn    AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER obOK        AS LOG NO-UNDO.

DEFINE VARIABLE iBuntNr AS INTEGER NO-UNDO.
DEFINE VARIABLE iLinjeNr AS INTEGER NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.

ASSIGN 
    obOk        = TRUE
    iBuntNr     = INT(ENTRY(1,icParam,'|'))  
    cLogg       = 'ovbuffer_NyttLinjeNr' + REPLACE(STRING(TODAY),'/','') 
    .

rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.
rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Start' 
    ).    
rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    Parametre: ' + icParam 
    ).    

DO:
  FIND LAST Ovbuffer NO-LOCK WHERE 
    Ovbuffer.BuntNr = iBuntNr USE-INDEX BuntLinjeNr NO-ERROR.
  IF AVAILABLE OvBuffer THEN 
    iLinjeNr = OvBuffer.LinjeNr + 1.
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