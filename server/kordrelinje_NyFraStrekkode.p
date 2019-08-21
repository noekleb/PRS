/* kordrelinje_NyFraStrekkode.p

-----------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER icParam     AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ihBuffer    AS HANDLE NO-UNDO.
DEFINE INPUT  PARAMETER icSessionId AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER ocReturn    AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER obOK        AS LOG NO-UNDO.

DEFINE VARIABLE lKOrdre_Id AS INTEGER NO-UNDO.
DEFINE VARIABLE iLinjeNr AS INTEGER NO-UNDO.
DEFINE VARIABLE cStrekkode AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEFINE VARIABLE iButNr AS INTEGER NO-UNDO.
DEFINE VARIABLE lVVareKost AS DECIMAL NO-UNDO.
DEFINE VARIABLE iReturKodeId AS INTEGER NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.

DEFINE BUFFER bufKOrdreLinje FOR KOrdreLinje.

ASSIGN 
    lKOrdre_Id   = DEC(ENTRY(1,icParam,'|'))  
    cStrekkode   = ENTRY(2,icParam,'|')
    iLinjeNr     = INT(ENTRY(3,icParam,'|'))  
    iButNr       = INT(ENTRY(4,icParam,'|'))  
    iReturKodeId = INT(ENTRY(5,icParam,'|'))
    cLogg        = 'kordrelinje_NyFraStrekkode' + REPLACE(STRING(TODAY),'/','') 
    NO-ERROR.

rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.
rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Start' 
    ).    
rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    Parametre: ' + icParam 
    ).    

FIND StrekKode NO-LOCK WHERE 
  StrekKode.Kode = cStrekkode NO-ERROR.
IF NOT AVAILABLE StrekKode THEN 
DO:
  ASSIGN 
    obOk = FALSE 
    ocReturn = '** Ukjent strekkode!'
    .
  RETURN.
END.

DO TRANSACTION:
  FIND ArtBas NO-LOCK WHERE 
    ArtBas.ArtikkelNr = StrekKode.ArtikkelNr NO-ERROR.
  IF NOT AVAILABLE ArtBas THEN 
  DO:
    ASSIGN 
      obOk = FALSE 
      ocReturn = '** Ukjent Artikkel på strekkoden!'
      .
    RETURN.
  END. 
  
  FIND Lager NO-LOCK WHERE 
    Lager.ArtikkelNr = ArtBas.ArtikkelNr AND 
    Lager.Butik = iButNr NO-ERROR.
  IF AVAILABLE Lager AND Lager.VVarekost <> ? AND Lager.VVarekost > 0 THEN 
    lVVareKost = Lager.VVarekost.
  ELSE IF AVAILABLE ArtPris THEN 
    lVVareKost = ArtPris.VareKost[1].
  
  /* Akkumulerer */   
  FIND FIRST KOrdreLinje EXCLUSIVE-LOCK WHERE 
    KOrdreLinje.KORdre_Id = lKOrdre_Id AND 
    KOrdreLinje.VareNr = STRING(ArtBas.ArtikkelNr) AND 
    KOrdreLinje.StrKode = StrekKode.StrKode NO-ERROR.
  IF AVAILABLE KOrdreLinje THEN 
    ASSIGN 
      KOrdreLinje.Antall    = KOrdreLinje.Antall + 1
      obOk     = TRUE
      ocReturn = STRING(KOrdreLinje.KORdre_Id) + '|' + STRING(KOrdreLinje.KOrdreLinjeNr)
      .
  /* Legger opp ny. */
  ELSE DO:        
    CREATE KOrdreLinje.
    ASSIGN 
      KOrdreLinje.KORdre_Id     = lKOrdre_Id
      KOrdreLinje.KOrdreLinjeNr = iLinjeNr
      .
    ASSIGN 
      KOrdreLinje.VareNr        = STRING((IF AVAILABLE ArtBas THEN ArtBas.ArtikkelNr ELSE 0))
      KOrdreLinje.Varetekst     = (IF AVAILABLE ArtBas THEN ArtBas.Beskr ELSE '')
      KOrdreLinje.LevFargKod    = (IF AVAILABLE ArtBas THEN ArtBas.LevFargKod ELSE '')
      KOrdreLinje.Antall        = 1
      KOrdreLinje.StrKode       = (IF AVAILABLE Strekkode THEN Strekkode.StrKode ELSE 0)
      
      KOrdreLinje.Kode          = (IF AVAILABLE Strekkode THEN Strekkode.Kode ELSE '')
    .

/*
  ASSIGN
    {1}.VareNr            = {2}.VareNr
    {1}.Varetekst         = {2}.Varetekst
    {1}.Antall            = {2}.Antall
    {1}.LinjeRab%         = {2}.LinjeRab%
    {1}.LinjeRabattKr     = {2}.LinjeRabattKr
    {1}.OrdreRabattKr     = {2}.OrdreRabattKr
    {1}.NettoPris         = {2}.NettoPris
    {1}.MomsKod           = {2}.MomsKod
    {1}.MvaKr             = {2}.MvaKr
    {1}.NettoLinjesum     = {2}.NettoLinjesum
    {1}.Notat             = {2}.Notat
    {1}.Leveringsdato     = {2}.Leveringsdato.
    {1}.Db%               = {2}.Db%
    {1}.DbKr              = {2}.DbKr
    {1}.Storl             = {2}.Storl
    {1}.Mva%              = {2}.Mva%
    {1}.BruttoPris        = {2}.BruttoPris
    {1}.Pris              = {2}.Pris
    {1}.VareKost          = {2}.VareKost
    {1}.RefNr             = {2}.RefNr
    {1}.RefTekst          = {2}.RefTekst
    {1}.Linjesum          = {2}.Linjesum
    {1}.Bestillingsnummer = {2}.Bestillingsnummer
    {1}.VareBehNr         = {2}.VareBehNr
    {1}.BestNr            = {2}.BestNr
    {1}.ValKod            = {2}.ValKod
    {1}.PlukkButikk       = {2}.PlukkButikk
    {1}.UtleverButikk     = {2}.UtleverButikk
    {1}.Kode              = {2}.Kode
    {1}.ReturKodeId       = {2}.ReturKodeId
    {1}.OpprAntall        = {2}.OpprAntall
    {1}.Opphav            = {2}.Opphav
    {1}.Returnert         = {2}.Returnert
    {1}.KopiKOrdreLinjeNr = {2}.KopiKOrdreLinjeNr
    {1}.Aktiv             = {2}.Aktiv
    {1}.OrgLinjesum       = {2}.OrgLinjesum
*/

    ASSIGN 
      obOk     = TRUE
      ocReturn = ''
      .
  END.
  
END. /* TRANSACTION */  

rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Slutt' 
    ).    
