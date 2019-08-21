/* kordrelinje_retur_sjekk.p

-----------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER icParam     AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ihBuffer    AS HANDLE NO-UNDO.
DEFINE INPUT  PARAMETER icSessionId AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER ocReturn    AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER obOK        AS LOG NO-UNDO.

DEFINE VARIABLE lKOrdre_Id AS DECIMAL NO-UNDO.
DEFINE VARIABLE iAntLinjer AS INTEGER NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEFINE VARIABLE bTest AS LOG NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.

ASSIGN 
    bTest       = TRUE
    obOk        = TRUE
    lKOrdre_Id  = INT(ENTRY(1,icParam,'|'))  
    cLogg       = 'kordrelinje_retur_sjekk' + REPLACE(STRING(TODAY),'/','') 
    .

rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.
IF bTest THEN
  DO: 
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        'Start' 
        ).    
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '    Parametre: ' + icParam 
        ).    
  END.
DO:
  iAntLinjer = 0.
  FOR EACH KORdreLinje NO-LOCK WHERE 
    KORdreLinje.KOrdre_Id = lKOrdre_ID AND 
    KOrdreLinje.Returnert = FALSE:
    IF KOrdreLinje.VareNr = 'BETALT' THEN 
      NEXT.
    iAntLinjer = iAntLinjer + 1.  
  END. 
END.

ASSIGN 
  obOk = TRUE
  ocReturn = STRING(iAntLinjer)
  .
IF bTest THEN
  DO: 
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '    AntLinjer: ' + STRING(iAntLinjer) 
        ).    
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        'Slutt' 
        ).    
  END.