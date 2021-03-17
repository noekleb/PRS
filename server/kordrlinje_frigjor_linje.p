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

/* Bruker buffer. Sendes KOrdreLinje via ihBuffer, vil KOrdreLinje bare gjelde temp tabellen. */
DEFINE BUFFER bufKOrdreLinje FOR KOrdreLinje.
DEFINE BUFFER buf2KOrdreLinje FOR KOrdreLinje.

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
      '      LinjeNr   : ' + STRING(iLinjeNr)  + ' ' + (IF iLinjeNr = 0 THEN 'ALLE Linje' ELSE '' )
      ).    
END.

/* Frigjør enkeltlinje fra ordre */
IF iLinjeNr > 0 THEN 
ENKELTLINJE:
DO:
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
  ELSE DO: 
    ASSIGN 
      bufKOrdreLinje.Returnert = FALSE.

      IF bTest THEN 
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '  Frigjort hode/linje' +  STRING(bufKOrdreLinje.KOrdre_Id) + '/' + STRING(bufKOrdreLinje.KOrdreLinjeNr) 
            ).    
  END.
      
      
END. /* ENKELTLINJE */

/* Frigjør alle linjer når hele returordren slettes. */
ELSE IF iLinjeNr = 0 THEN
ALLELINJER: 
DO:
  /* Henter retur ordre. */
  FIND KOrdreHode NO-LOCK WHERE 
    KOrdreHode.KOrdre_Id = lKOrdre_Id NO-ERROR.
  IF NOT AVAILABLE KOrdreHode THEN 
    LEAVE ALLELINJER.
      
  /* Leser alle linjene på retur ordren og frigjør linjene på den opprinnelige ordren. */
  /* Denne runden tar både aktive og passive linjer.                                   */
  FOR EACH buf2KOrdreLinje NO-LOCK WHERE 
    buf2KOrdreLinje.KOrdre_Id = KOrdreHode.KOrdre_Id:

  /* Finner opphavslinjen via RefKOrdre_Id og linjenr.. */ 
  FIND FIRST bufKOrdreLinje EXCLUSIVE-LOCK WHERE 
    bufKOrdreLinje.KOrdre_Id     = KOrdreHode.RefKOrdre_Id AND
    bufKOrdreLinje.KOrdreLinjeNr = buf2KOrdreLinje.KOrdreLinjeNr NO-ERROR.
  IF AVAILABLE bufKOrdreLinje THEN 
    ASSIGN 
      bufKOrdreLinje.Returnert = FALSE.
      
  END.
END. /* ALLELINJER */

ASSIGN 
  obOk     = TRUE
  ocReturn = ''
  .
IF bTest THEN 
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      'Slutt' 
      ).    
