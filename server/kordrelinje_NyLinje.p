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
DEFINE VARIABLE iButNr AS INTEGER NO-UNDO.
DEFINE VARIABLE iLinjeNr AS INTEGER NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.

DEFINE BUFFER bufKOrdreLinje FOR KOrdreLinje.

{syspara.i 150 1 2 iButNr INT}
FIND Butiker NO-LOCK WHERE 
  Butiker.butik = iButNr NO-ERROR.
IF NOT AVAILABLE Butiker THEN 
DO:
  ASSIGN 
    obOk = FALSE 
    ocReturn = 'Ukjent butikk.'
    .
  RETURN.
END.

ASSIGN 
    lKOrdre_Id   = DEC(ENTRY(1,icParam,'|'))  
    cStrekkode   = ENTRY(2,icParam,'|')
    iReturKodeId = INT(ENTRY(3,icParam,'|'))
    cLogg        = 'kordrelinje_NyLinje' + REPLACE(STRING(TODAY),'/','')
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
  
IF AVAILABLE Strekkode THEN
BLOKKEN:   
DO TRANSACTION:
  FIND ArtBas NO-LOCK WHERE 
    ArtBas.ArtikkelNr = StrekKode.ArtikkelNr NO-ERROR.
  IF NOT AVAILABLE ArtBas THEN 
    LEAVE BLOKKEN.
  FIND ArtPris NO-LOCK WHERE 
    ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND 
    ArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.
  IF NOT AVAILABLE ArtPris THEN 
    FIND FIRST ArtPris NO-LOCK WHERE 
      ArtPris.ArtikkelNr = ArtBas.ArtikkelNr NO-ERROR.
  IF NOT AVAILABLE ArtPris THEN 
    LEAVE BLOKKEN.
  FIND StrKonv NO-LOCK WHERE 
    StrKonv.StrKode = Strekkode.StrKode NO-ERROR.
  FIND LAST KOrdreLinje NO-LOCK WHERE 
    KOrdreLinje.KOrdre_Id = KOrdreHode.KOrdre_Id USE-INDEX FaktLinje NO-ERROR.
  IF AVAILABLE KOrdreLinje THEN 
    iLinjeNr = KOrdreLinje.KOrdreLinjeNr + 1.
  ELSE 
    iLinjeNr = 1.
  
  CREATE KOrdreLinje.
  ASSIGN
      KOrdreLinje.KOrdre_ID         = KOrdreHode.KOrdre_Id
      KOrdreLinje.KOrdreLinjeNr     = iLinjeNr 
      KOrdreLinje.VareNr            = STRING(ArtBas.ArtikkelNr)
      KOrdreLinje.Varetekst         = (IF AVAILABLE ArtBas THEN ArtBas.Beskr ELSE '** Ukjent ' + KOrdreLinje.VareNr)
      KOrdreLinje.Antall            = 1
      KOrdreLinje.NettoPris         = ArtPris.Pris[IF ArtPris.Tilbud THEN 2 ELSE 1]
      KOrdreLinje.NettoLinjesum     = KOrdreLinje.NettoPris
      KOrdreLinje.LinjeRabattKr     = 0
      KOrdreLinje.LinjeRabattKr     = 0
      KOrdreLinje.LinjeRab%         = 0
      KOrdreLinje.LinjeRab%         = 0
      KOrdreLinje.ReturKodeId       = iReturKodeId

      KOrdreLinje.MomsKod           = ArtPris.MomsKod[IF ArtPris.Tilbud THEN 2 ELSE 1]
      KOrdreLinje.MvaKr             = ArtPris.MvaKr[IF ArtPris.Tilbud THEN 2 ELSE 1]
      KOrdreLinje.Storl             = IF AVAILABLE StrKonv THEN StrKonv.Storl ELSE ''
      KOrdreLinje.Mva%              = ArtPris.Mva%[IF ArtPris.Tilbud THEN 2 ELSE 1]
      KOrdreLinje.StrKode           = IF AVAILABLE StrKonv THEN StrKonv.StrKode ELSE 0
      KOrdreLinje.VareKost          = ArtPris.Varekost[IF ArtPris.Tilbud THEN 2 ELSE 1]
      KOrdreLinje.VareKost          = IF (KOrdreLinje.VareKost = ? OR KOrdreLinje.VareKost <= 0) THEN 0 ELSE KOrdreLinje.VareKost

      KOrdreLinje.BruttoPris        = KOrdreLinje.NettoPris + (KOrdreLinje.LinjeRabattKr / KOrdreLinje.Antall)
      KOrdreLinje.Pris              = KOrdreLinje.NettoPris + (KOrdreLinje.LinjeRabattKr / KOrdreLinje.Antall)
      KOrdreLinje.Linjesum          = KOrdreLinje.NettoLinjesum + KOrdreLinje.LinjeRabattKr
      KOrdreLinje.DbKr              = KOrdreLinje.NettoLinjesum - (KOrdreLinje.VareKost * KOrdreLinje.Antall)
      KOrdreLinje.Db%               = (KOrdreLinje.DbKr / KOrdreLinje.NettoLinjesum) * 100
      KOrdreLinje.Db%               = (IF KOrdreLinje.Db% = ? THEN 0 ELSE KOrdreLinje.Db%)
      KOrdreLinje.RefNr             = 0
      KOrdreLinje.RefTekst          = 'Varebytte'
      KOrdreLinje.Bestillingsnummer = IF AVAILABLE ArtBas THEN ArtBas.LevKod ELSE ''
      KOrdreLinje.LevFargKod        = IF AVAILABLE ArtBas THEN ArtBas.LevFargKod ELSE ''
      KOrdreLinje.ValKod            = KOrdreHode.ValKod
      /* Snur sakene */
      KOrdreLinje.nettolinjesum     = KOrdreLinje.nettolinjesum * -1
      KOrdreLinje.NettoPris         = KOrdreLinje.NettoPris * -1     
      KOrdreLinje.MvaKr             = KOrdreLinje.MvaKr * -1         
      KOrdreLinje.Mva%              = KOrdreLinje.Mva% * -1          
      KOrdreLinje.BruttoPris        = KOrdreLinje.BruttoPris * -1    
      KOrdreLinje.Pris              = KOrdreLinje.Pris * -1          
      KOrdreLinje.Linjesum          = KOrdreLinje.Linjesum * -1      
     NO-ERROR.  
  
END. /* BLOKKEN TRANSACTION */

/*  DO ix = 1 TO ERROR-STATUS:NUM-MESSAGES:                                        */
/*    ocReturn = ocReturn +                                                        */
/*        (IF ocReturn <> '' THEN CHR(10) ELSE '') +                               */
/*        STRING(ERROR-STATUS:GET-NUMBER(ix)) + ' ' + ERROR-STATUS:GET-MESSAGE(ix).*/
/*  END.                                                                           */
  
ASSIGN 
  obOk     = TRUE
  ocReturn = ''
  .

IF bTest THEN 
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      'Slutt' 
      ).    
