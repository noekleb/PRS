/* kordrelinje_NyttLinjeNr.p

-----------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER icParam     AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ihBuffer    AS HANDLE NO-UNDO.
DEFINE INPUT  PARAMETER icSessionId AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER ocReturn    AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER obOK        AS LOG NO-UNDO.

DEFINE VARIABLE lKOrdre_Id AS DECIMAL NO-UNDO.
DEFINE VARIABLE iLinjeNr AS INTEGER NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.

ASSIGN 
    obOk        = TRUE
    lKOrdre_Id  = INT(ENTRY(1,icParam,'|'))  
    cLogg       = 'kordrelinje_NyttLinjeNr' + REPLACE(STRING(TODAY),'/','') 
    .

rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.
rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Start' 
    ).    
rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    Parametre: ' + icParam 
    ).    

DO:
  FIND LAST KOrdreLinje NO-LOCK WHERE 
    KOrdreLinje.KOrdre_Id = lKOrdre_Id USE-INDEX FaktLinje NO-ERROR.
  IF AVAILABLE KOrdreLinje THEN 
    iLinjeNr = KOrdreLinje.KOrdreLinjeNr + 1.
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