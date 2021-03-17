
/*------------------------------------------------------------------------
    File        : rekalkulerBong.p
    Purpose     : Et sted at bong regnes om.

    Syntax      :

    Description : Sjekker bongen mot rekalkulet bong fra Dintero og regner om hvis nødvnedig.

    Author(s)   : Tom Nøkleby
    Created     : Tue Nov 24 16:37:04 CET 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{cls\dintero\ttPOSBong.i}
{cls\dintero\dsPOSBong.i}
{cls\dintero\ttReceipts.i}
{cls\dintero\dsReceipts.i}

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsPOSbong.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsReceipts.
DEFINE OUTPUT       PARAMETER bReturn AS LOG NO-UNDO.

DEFINE VARIABLE lLinjeRabKr AS DECIMAL NO-UNDO.
DEFINE VARIABLE lLinjeRabDinteroKr AS DECIMAL NO-UNDO.
DEFINE VARIABLE lSubTotRabKr AS DECIMAL NO-UNDO.
DEFINE VARIABLE lForsteKjop AS DECIMAL NO-UNDO.
DEFINE VARIABLE lDinteroRabatt AS DECIMAL NO-UNDO.
DEFINE VARIABLE bTest AS LOG NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
rStandardFunksjoner = NEW cls.StdFunk.StandardFunksjoner( ) NO-ERROR.

ASSIGN 
  bReturn = TRUE
  bTest   = TRUE
  cLogg = 'rekalkulerBong' + STRING(TODAY,"99999999") 
  .

rStandardFunksjoner:SkrivTilLogg(cLogg,
  'Start.' 
  ).    
  
FIND FIRST ttPOSbongHode NO-ERROR.  
FIND FIRST ttReceipts NO-ERROR.

IF NOT AVAILABLE ttPOSbongHode OR 
  NOT AVAILABLE ttReceipts THEN 
  DO:
    ASSIGN 
      bReturn = FALSE 
      .
    RETURN.
  END.
  
ASSIGN 
  ttPOSBongHode.Is_Changed = ttReceipts.is_changed
  . 

rStandardFunksjoner:SkrivTilLogg(cLogg,
  '  is_changed: ' + STRING(ttReceipts.is_changed)
  ).    
  
/* Ingen rekalkulering nødvendig. Det er ikke gitt rabatt fra Dintero. */
IF ttReceipts.is_changed = FALSE THEN 
  DO:
    ASSIGN 
      bReturn = TRUE 
      .
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      'Slutt. (1)' 
      ).    
    RETURN. 
  END.
  
FIND FIRST ttStatistics WHERE 
  ttStatistics.receipt_id = ttReceipts.receipt_id NO-ERROR.
IF AVAILABLE ttStatistics THEN
STATISTICSBLOKK: 
DO:
  /* Summerer opp rabatt på bongen fra kassen. */
  /* Bare en bong her :)                       */  
  FOR EACH ttPOSbongLinje WHERE 
    ttPOSbongLinje.B_Id = ttPOSbongHode.B_Id:
    ASSIGN
      lLinjeRabKr  = lLinjeRabKr  + ttPOSbongLinje.SubtotalRab  
      lSubTotRabKr = lSubTotRabKr + ttPOSbongLinje.LinjeRab
      .
  END.
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '  lLinjeRabKr: ' + STRING(lLinjeRabKr)
    ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '  lSubTotRabKr: ' + STRING(lLinjeRabKr)
    ).    
  
  /* Summerer opp rabattene fra dintero.                         */
  /* Slår flere kampanjer til samtidig, ligger det flere record. */
  FOR EACH ttRefs WHERE 
     ttRefs.receipt_id = ttReceipts.receipt_id:
    /* Summerer opp verdien av de ulike rabattene som har slått til. */    
    ASSIGN 
      lDinteroRabatt = lDinteroRabatt + ttRefs.amount
      .

    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  Summerer dintero rabatt: Sum: ' + STRING(lDinteroRabatt) + ' ttRefs.Amaunt:  ' + STRING(ttRefs.amount)
      ).    

    /* Er det førstekjøps rabatt, summeres den opp spesifikt her. */
    /* Den skal tas med i kassens rabatt.                         */
    FIND FIRST ttLimitation WHERE 
      ttLimitation.receipt_id = ttRefs.receipt_id AND 
      ttLimitation.discounts_id = ttRefs.discounts_id NO-ERROR.
    IF AVAILABLE ttLimitation AND ttLimitation.discount_repeat_usage = 1 THEN   
      lForsteKjop = lForsteKjop + ttRefs.amount.       
  END.
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '  lDinteroRabatt: ' + STRING(lDinteroRabatt)
    ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '  lForsteKjop: ' + STRING(lForsteKjop)
    ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '  Diff dintero < (lForsteKjop + KasseRab + KassesubRab): ' + STRING((lDinteroRabatt / 100) <= (lSubTotRabKr + lLinjeRabKr + (lForsteKjop / 100)))
    ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '  Diff dintero < (lForsteKjop + KasseRab + KassesubRab): ' + 
      STRING(lDinteroRabatt / 100) + ' ' + 
      STRING(lSubTotRabKr + lLinjeRabKr + (lForsteKjop / 100))
    ).    

  /* Er kassens rabatt størst eller lik gjøres ingenting. */
  IF (lDinteroRabatt / 100) < (lSubTotRabKr + lLinjeRabKr) THEN   
    DO:
      ASSIGN 
        ttPOSBongHode.Is_Changed = FALSE
        bReturn = TRUE 
        .
      rStandardFunksjoner:SkrivTilLogg(cLogg,
        'Slutt. (Kasserabatt > dintero rabatt)' 
        ).    
    END.

  /* Aktiverer Dintero rabattene da Dintero har størst rabatt. */
  IF ttPOSBongHode.Is_Changed THEN 
  REKALKULER:
  DO:
    /* Tar hver varelinje på bongen, nullstiller og regner om. */
    /* Og det kommer bare varelinjer her.                      */
    BONGLINJEOMREGNING:
    FOR EACH ttPOSbongLinje:
      /* Leser rabattene fra Dintero og legger dem i linjerabatten */
      /* Finner først matchene linje i ttReceipt. */
      FIND ttItem WHERE 
        ttItem.receipt_id = ttReceipts.receipt_id AND  
        ttItem.line_id    = ttPOSbongLinje.LinjeNr NO-ERROR.  
      
      /* Leser og summerer deretter rabattlinjene under denne linjen */
      ASSIGN 
        lLinjeRabDinteroKr = 0
        .
      /* Her summeres vanlig rabatt og eventuell førstekjøps rabatt på linjenivå. */
      FOR EACH ttDiscount_lines WHERE  
        ttDiscount_lines.receipt_id = ttReceipts.receipt_id AND  
        ttDiscount_lines.line_id    = ttPOSbongLinje.LinjeNr:
        ASSIGN 
          lLinjeRabDinteroKr = lLinjeRabDinteroKr + ttDiscount_lines.amount
          .     
      END.
      
      /* Legger rabatten på plass på linjen og justerer beløpene. */
      ASSIGN 
        ttPOSbongLinje.Medlemsrabatt = lLinjeRabDinteroKr / 100
        . 
    END.
    /* Flagger ferdig. */
    ASSIGN 
      bReturn = TRUE 
      .
  END. /* REKALKULER */  
END. /* STATISTICSBLOKK */ 
ELSE DO:
  ASSIGN 
    bReturn = TRUE 
    .
END.
  
IF bTest THEN
DO: 
  DATASET dsPOSBong:WRITE-JSON ('file','konv\rekalkulerBong' + STRING(ttPOSbongHode.B_Id) + '.Json').
END.

rStandardFunksjoner:SkrivTilLogg(cLogg,
  'Slutt.' 
  ).    
  
RETURN.

    
  