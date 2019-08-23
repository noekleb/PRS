/* kordrlinje_frigjor_linje.p

-----------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER icParam     AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ihBuffer    AS HANDLE NO-UNDO.
DEFINE INPUT  PARAMETER icSessionId AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER ocReturn    AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER obOK        AS LOG NO-UNDO.

DEFINE VARIABLE lKOrdre_Id AS INTEGER NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEFINE VARIABLE bTest AS LOG NO-UNDO.
DEFINE VARIABLE iLinjeNr AS INTEGER NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.

DEFINE BUFFER bufKOrdreLinje FOR KOrdreLinje.

ASSIGN 
    lKOrdre_Id   = DEC(ENTRY(1,icParam,'|'))  
    iLinjeNr     = INT(ENTRY(2,icParam,'|'))
    cLogg        = 'kordrlinje_frigjor_linje.p' + REPLACE(STRING(TODAY),'/','')
    bTest        = TRUE 
    NO-ERROR.

rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.
IF bTest THEN 
DO:
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      'Start' 
      ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '    Parametre: ' + icParam 
      ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '      lKOrdre_Id: ' + STRING(lKOrdre_Id) 
      ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '      LinjeNr   : ' + STRING(iLinjeNr) 
      ).    
END.

/* Henter ordrelinjen fra opprinnelig ordre. */
FIND FIRST bufKOrdreLinje EXCLUSIVE-LOCK WHERE 
  bufKOrdreLinje.KOrdre_Id     = lKOrdre_Id AND 
  bufKOrdreLinje.KOrdreLinjeNr = iLinjeNr NO-ERROR.
IF NOT AVAILABLE bufKOrdreLinje THEN 
  DO:
    ASSIGN 
      obOk = FALSE 
      ocReturn = 'Finner ikke opprinnelig ordrelinje for å kunne frigjøre denne.'
      .
    IF bTest THEN 
      rStandardFunksjoner:SkrivTilLogg(cLogg,
          '  ' + ocReturn 
          ).    
    RETURN.
  END.
ELSE 
  ASSIGN 
    bufKOrdreLinje.Returnert = FALSE.

ASSIGN 
  obOk     = TRUE
  ocReturn = ''
  .
IF bTest THEN 
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      'Slutt' 
      ).    
