/* ovbunt_Hent_tilBut.p

-----------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER icParam     AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ihBuffer    AS HANDLE NO-UNDO.
DEFINE INPUT  PARAMETER icSessionId AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER ocReturn    AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER obOK        AS LOG NO-UNDO.

DEFINE VARIABLE iBuntNr AS INTEGER NO-UNDO.
DEFINE VARIABLE iTilBut AS INTEGER NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.

ASSIGN 
    obOk        = TRUE
    ibuntNr     = INT(ENTRY(1,icParam,'|'))  
    cLogg       = 'ovbunt_Hent_tilBut' + REPLACE(STRING(TODAY),'/','') 
    .

rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.
rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Start' 
    ).    
rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    Parametre: ' + icParam 
    ).    

DO:
  FIND FIRST OvBuffer NO-LOCK WHERE 
    OvBuffer.BuntNr = iBuntNr NO-ERROR.
  IF AVAILABLE Ovbuffer THEN 
    iTilBut = OvBuffer.ButikkNrTil.
  ELSE 
    iTilBut = 0.
END.

rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    TilBut: ' + STRING(iTilBut) 
    ).    

ASSIGN 
  obOk = TRUE
  ocReturn = STRING(iTilBut)
  .