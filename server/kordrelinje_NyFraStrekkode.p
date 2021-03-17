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
DEFINE VARIABLE icModus AS CHARACTER NO-UNDO.
DEFINE VARIABLE iX AS INTEGER NO-UNDO.
DEFINE VARIABLE rRecid AS RECID NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.

DEFINE BUFFER bufKOrdreLinje FOR KOrdreLinje.

ASSIGN 
    lKOrdre_Id   = DEC(ENTRY(1,icParam,'|'))  
    cStrekkode   = ENTRY(2,icParam,'|')
    iReturKodeId = INT(ENTRY(3,icParam,'|'))
    cLogg        = 'kordrelinje_NyFraStrekkode' + REPLACE(STRING(TODAY),'/','')
    bTest        = TRUE 
    NO-ERROR.
IF NUM-ENTRIES(icParam,'|') >= 4 THEN 
  icModus = ENTRY(4,icParam,'|'). /* 20=Bytte. Eller er det retur. */

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
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '      Modus: ' + icModus 
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
IF bTest THEN  
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '      RefReturKodeId: ' + STRING(KOrdreHode.RefKOrdre_Id) 
      ).    
  
/* Henter ordrelinjen fra opprinnelig ordre for RETUR. */
IF icModus = '10' THEN 
DO:
  FIND FIRST bufKOrdreLinje NO-LOCK WHERE 
    bufKOrdreLinje.KOrdre_Id = KOrdreHode.RefKOrdre_Id AND 
    bufKOrdreLinje.Kode      = cStrekkode AND 
    bufKOrdreLinje.Aktiv     = TRUE AND 
    bufKOrdreLinje.Returnert = FALSE AND 
    bufKOrdreLinje.VareNr   <> 'BETALT' NO-ERROR.
    IF bTest THEN 
      rStandardFunksjoner:SkrivTilLogg(cLogg,
          '  Funnet retur: ' + STRING(AVAILABLE bufKOrdreLinje) 
          ).    
END.
IF icModus = '20' THEN /* Varebytte */ 
DO:
  rRecid = ?.
  iX     = 0.
  LOOPEN2:
  FOR EACH bufKOrdreLinje NO-LOCK WHERE 
    bufKOrdreLinje.KOrdre_Id = KOrdreHode.RefKOrdre_Id AND 
    bufKOrdreLinje.Kode      = cStrekkode AND 
    bufKOrdreLinje.Aktiv     = TRUE AND 
    bufKOrdreLinje.VareNr   <> 'BETALT':
    iX = iX + 1.
    IF CAN-FIND(KOrdreLinje WHERE 
                KOrdreLinje.KOrdre_Id     = KOrdreHode.KOrdre_Id AND 
                KOrdreLinje.KOrdreLinjeNr = bufKOrdreLinje.KORdreLinjeNr) THEN 
      NEXT.
    ELSE DO:
      rRecid = RECID(bufKOrdreLinje).
      LEAVE LOOPEN2.
    END.  
  END. /* LOOPEN2 */
  IF rRecid <> ? THEN 
    FIND bufKOrdreLinje NO-LOCK WHERE 
      RECID(bufKOrdreLinje) = rRecid.
  IF bTest THEN 
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  Funnet bytte: ' + STRING(AVAILABLE bufKOrdreLinje) 
        ).
  IF NOT AVAILABLE bufKOrdreLinje THEN 
  DO:
    IF iX > 0 THEN 
      ASSIGN 
        obOk     = FALSE 
        ocReturn = 'OK:Alle varelinjene med denne strekkoden ligger allerede på bytteordren.'
        .
    ELSE 
      ASSIGN 
        obOk     = FALSE 
        ocReturn = 'OK:Ingen varelinjer med angitt strekkode finnes på opprinnelig ordre (Bytte).'
        .
    RETURN.
  END.    
END.

IF AVAILABLE bufKOrdreLinje THEN 
DO: 
  IF bTest AND icModus = '20' THEN
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  Varebytte(X) linje KOrdre_Id: ' + STRING(bufKOrdreLinje.KOrdre_Id) + ' Linje: ' +
        STRING(bufKOrdreLinje.KORdreLinjeNr) + ' Returkode: ' +  
        STRING(iReturKodeId) + ' Strekkode: ' + 
        bufKOrdreLinje.Kode + ' Aktiv: ' + 
        STRING(bufKOrdreLinje.Aktiv) + ' Byttet: ' + 
        STRING(bufKOrdreLinje.Returnert) + ' VareNr: ' +  
        bufKOrdreLinje.VareNr + ' Storl: ' +
        bufKOrdreLinje.Storl 
        ).    
  ELSE  
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  Returnerer(X) linje KOrdre_Id: ' + STRING(bufKOrdreLinje.KOrdre_Id) + ' Linje: ' +
        STRING(bufKOrdreLinje.KORdreLinjeNr) + ' Returkode: ' +  
        STRING(iReturKodeId) + ' Strekkode: ' + 
        bufKOrdreLinje.Kode + ' Aktiv: ' + 
        STRING(bufKOrdreLinje.Aktiv) + ' Returnert: ' + 
        STRING(bufKOrdreLinje.Returnert) + ' VareNr: ' +  
        bufKOrdreLinje.VareNr + ' Storl: ' +
        bufKOrdreLinje.Storl 
        ).    

  RUN kordrelinje_opprett_retur_linjer.p (KOrdreHode.RefKOrdre_Id, /* Opprinnelig ordre */
                                          KOrdreHode.KOrdre_Id, /* RETUR/BYTTE ordre */ 
                                          bufKOrdreLinje.KOrdreLinjeNr, 
                                          iReturKodeId,
                                          bufKOrdreLinje.Antall,
                                          OUTPUT dSum,
                                          OUTPUT ocReturn,
                                          OUTPUT obOk
                                          ) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN 
  DO:
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  Error1: ' + ocReturn 
        ).    
    ocReturn = ''. 
    DO ix = 1 TO ERROR-STATUS:NUM-MESSAGES:    
      ocReturn = ocReturn + 
          (IF ocReturn <> '' THEN CHR(10) ELSE '') + 
          STRING(ERROR-STATUS:GET-NUMBER(ix)) + ' ' + ERROR-STATUS:GET-MESSAGE(ix).
    END.
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  Error2: ' + ocReturn 
        ).    
  END.
    
  IF obOk = FALSE THEN
  DO: 
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  obOk fra opprettelse av linje: ' + STRING(obOk) 
        ).    
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  Feil fra opprettelse av linje: ' + ocReturn 
        ).    
  END.     
  ELSE      
    ASSIGN 
      obOk     = TRUE
      ocReturn = ''
      .
END.
ELSE DO:
  ASSIGN 
    obOk     = FALSE
    ocReturn = '  Ingen fler av den varen som kan returneres.'
    .
END.
IF bTest THEN 
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      'Slutt' 
      ).    
