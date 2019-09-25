/* kordrelinje_NyFraStrekkode.p

-----------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER icParam     AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ihBuffer    AS HANDLE NO-UNDO.
DEFINE INPUT  PARAMETER icSessionId AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER ocReturn    AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER obOK        AS LOG NO-UNDO.

DEFINE VARIABLE lKOrdre_Id AS INTEGER NO-UNDO.
DEFINE VARIABLE cStrekkode AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEFINE VARIABLE iReturKodeId AS INTEGER NO-UNDO.
DEFINE VARIABLE dSum AS DECIMAL NO-UNDO.
DEFINE VARIABLE bTest AS LOG NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.

DEFINE BUFFER bufKOrdreLinje FOR KOrdreLinje.

ASSIGN 
    lKOrdre_Id   = DEC(ENTRY(1,icParam,'|'))  
    cStrekkode   = ENTRY(2,icParam,'|')
    iReturKodeId = INT(ENTRY(3,icParam,'|'))
    cLogg        = 'kordrelinje_NyFraStrekkode' + REPLACE(STRING(TODAY),'/','')
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
      '      lKOrdre_Id : ' + STRING(lKOrdre_Id) 
      ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '      Strekkode  : ' + cStrekkode 
      ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '      ReturKodeId: ' + STRING(iReturKodeId) 
      ).    
END.

FIND StrekKode NO-LOCK WHERE 
  StrekKode.Kode = cStrekkode NO-ERROR.
IF NOT AVAILABLE StrekKode THEN 
DO:
  ASSIGN 
    obOk = FALSE 
    ocReturn = '** Ukjent strekkode!'
    .
  IF bTest THEN 
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  ' + ocReturn 
        ).    
  RETURN.
END.

/* Henter retur ordren. */
FIND KOrdreHode NO-LOCK WHERE 
  KOrdreHode.KOrdre_Id = lKOrdre_Id NO-ERROR.
IF NOT AVAILABLE KORdreHode THEN 
  DO:
    ASSIGN 
      obOk = FALSE 
      ocReturn = '** Ukjent kordre_id! (' + STRING(lKOrdre_Id) + ').'
      .
    IF bTest THEN 
      rStandardFunksjoner:SkrivTilLogg(cLogg,
          '  ' + ocReturn 
          ).    
    RETURN.
  END.

/* Henter ordrelinjen fra opprinnelig ordre. */
FIND FIRST bufKOrdreLinje NO-LOCK WHERE 
  bufKOrdreLinje.KOrdre_Id = KOrdreHode.RefKOrdre_Id AND 
  bufKOrdreLinje.Kode      = cStrekkode AND 
  bufKOrdreLinje.Aktiv     = TRUE AND 
  bufKOrdreLinje.Returnert = FALSE AND 
  bufKOrdreLinje.VareNr   <> 'BETALT' NO-ERROR.
IF NOT AVAILABLE bufKORdreLinje THEN 
  DO:
    ASSIGN 
      obOk = FALSE 
      ocReturn = 'Ingen fler ordrelinjer med denne strekkoden som kan returneres. '
      .
    IF bTest THEN 
      rStandardFunksjoner:SkrivTilLogg(cLogg,
          '  ' + ocReturn 
          ).    
    RETURN.
  END.
ELSE DO: 
  IF bTest THEN 
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  Returnerer linje KOrdre_Id: ' + STRING(bufKOrdreLinje.KOrdre_Id) + ' Linje: ' +
        STRING(bufKOrdreLinje.KORdreLinjeNr) + ' Strekkode: ' +  
        bufKOrdreLinje.Kode + ' Aktiv: ' + 
        STRING(bufKOrdreLinje.Aktiv) + ' Returnert: ' + 
        STRING(bufKOrdreLinje.Returnert) + ' VareNr: ' +  
        bufKOrdreLinje.VareNr + ' Storl: ' +
        bufKOrdreLinje.Storl 
        ).    

  RUN kordrelinje_opprett_retur_linjer.p (KOrdreHode.RefKOrdre_Id, /* Opprinnelig ordre */
                                          KOrdreHode.KOrdre_Id, /* RETUR ordre */ 
                                          bufKOrdreLinje.KORdreLinjeNr, 
                                          iReturKodeId,
                                          bufKOrdreLinje.Antall,
                                          OUTPUT dSum,
                                          OUTPUT ocReturn,
                                          OUTPUT obOk
                                          ).
  IF obOk = FALSE THEN
  DO: 
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  ' + ocReturn 
        ).    
    RETURN.
  END.          
END.
ASSIGN 
  obOk     = TRUE
  ocReturn = ''
  .
IF bTest THEN 
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      'Slutt' 
      ).    
