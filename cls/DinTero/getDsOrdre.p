
/*------------------------------------------------------------------------
    File        : getDsOrdre.p
    Purpose     : Mottar et ordreid, henter kundeordren og bygger datasettet.

    Syntax      :

    Description : Bygger datasett dsOrdre.

    Author(s)   : Tom Nøkleby
    Created     : Fri Oct 16 14:22:47 CEST 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{cls\DinTero\ttOrder.i}
{cls\DinTero\dsOrder.i}

DEFINE INPUT  PARAMETER plKOrdre_Id AS DECIMAL FORMAT ">>>>>>>>>>>>9" NO-UNDO.
DEFINE OUTPUT PARAMETER pbOk AS LOG NO-UNDO.
DEFINE OUTPUT PARAMETER DATASET FOR dsOrder.

DEFINE VARIABLE iAntLinjer AS INTEGER NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

FIND KOrdreHode NO-LOCK WHERE 
  KOrdreHode.KOrdre_Id = plKOrdre_Id NO-ERROR.
IF NOT AVAILABLE KOrdreHode THEN 
DO:
  pbOk = FALSE.
  RETURN 'Ukjent kundeordreid ' + STRING(plKOrdre_Id) + '.'.
END.

ELSE 
BYGGDATASETT:
DO:
  /* Ordehode. */
  CREATE ttOrder.
  ASSIGN
    ttOrder.orderid              = KOrdreHode.KOrdre_Id            
    ttOrder.amount               = KOrdreHode.Totalt
    ttOrder.vat_amount           = (IF KOrdreHode.MvaKr <> 0 THEN KOrdreHode.MvaKr ELSE ROUND((KOrdreHode.Totalt * 0.20),2))
    ttOrder.currency             = 'NOK'            
    ttOrder.merchant_reference   = STRING(KOrdreHode.KOrdre_Id) 
    ttOrder.merchant_reference_2 = KOrdreHode.EkstOrdreNr 
    ttOrder.partial_payment      = FALSE      
    ttOrder.shipping_id          = STRING(KOrdreHode.LevFNr)
    ttOrder.customer_id          = STRING(KOrdreHode.KundeNr)
    . 
  
  /* Legger opp kundeinformasjonen. */
  FIND Kunde NO-LOCK WHERE 
    Kunde.KundeNr = KOrdreHode.KundeNr NO-ERROR.
  IF AVAILABLE Kunde THEN 
  KUNDEDATA:
  DO:
    CREATE ttCustomer.
    ASSIGN
      ttcustomer.customer_id  = STRING(Kunde.KundeNr) 
      ttcustomer.email        = TRIM((IF KOrdreHode.ePostAdresse <> '' THEN KOrdreHode.ePostAdresse ELSE Kunde.ePostAdresse)) 
      ttcustomer.phone_number = TRIM(REPLACE((IF KOrdreHode.MobilTlf <> '' THEN KOrdreHode.MobilTlf ELSE Kunde.MobilTlf),' ','')) 
      ttcustomer.email        = 'tomn@nsoft.no' 
      ttcustomer.phone_number = '41365436' 
      .  
  END. /* KUNDEDATA */
  
  /* Ordren skal hentes i butikk - Klikk&Hent. */
  IF KOrdreHode.LevFNr = 8 AND KOrdreHode.Butik > 0 THEN
  HENTEBUTIKKADRESSE: 
  DO:
    FIND Butiker NO-LOCK WHERE 
      Butiker.butik = KOrdreHode.Butik NO-ERROR.
    IF NOT AVAILABLE Butiker THEN 
      LEAVE HENTEBUTIKKADRESSE.
    FIND Post NO-LOCK WHERE 
      Post.PostNr = Butiker.LevPostNr NO-ERROR.     
      
    FIND LeveringsForm NO-LOCK WHERE 
      LeveringsForm.LevFNr = KOrdreHode.LevFNr NO-ERROR. 
    CREATE ttShipping_Option.
    ASSIGN
      ttShipping_Option.orderid         = KOrdreHode.KOrdre_Id 
      ttShipping_Option.shipping_id     = STRING(KOrdreHode.LevFNr)
      ttShipping_Option.line_id         = 'FRAKT'
      ttShipping_Option.amount          = 0
      ttShipping_Option.vat_amount      = 0
      ttShipping_Option.vat             = 0
      ttShipping_Option.shipping_title  = (IF AVAILABLE LeveringsForm THEN LeveringsForm.LevFormBeskrivelse ELSE '')
      ttShipping_Option.shipping_option_id  = (IF AVAILABLE LeveringsForm THEN LeveringsForm.LevFormMetode ELSE '')
      ttShipping_Option.description     = (IF AVAILABLE LeveringsForm THEN LeveringsForm.LevFormBeskrivelse ELSE '')
      ttShipping_Option.delivery_method =  (IF AVAILABLE LeveringsForm THEN LeveringsForm.LevFormMetode ELSE '')
      .      
      
    CREATE ttPick_Up_Address.
    ASSIGN 
      ttPick_Up_Address.orderid        = KOrdreHode.KOrdre_Id 
      ttPick_Up_Address.shipping_id    = STRING(KOrdreHode.LevFNr)
      ttPick_Up_Address.line_id        = ttShipping_Option.line_id
      ttPick_Up_Address.business_name  = (IF Butiker.butFirmaNavn <> '' THEN Butiker.butFirmaNavn ELSE Butiker.ButNamn)
      ttPick_Up_Address.address_line   = Butiker.LevAdresse1 
      ttPick_Up_Address.address_line_2 = Butiker.LevAdresse2
      ttPick_Up_Address.postal_code    = Butiker.LevPostNr
      ttPick_Up_Address.postal_place   = (IF AVAILABLE Post THEN Post.Beskrivelse ELSE '') 
      ttPick_Up_Address.country        = Butiker.ButLand
      ttPick_Up_Address.phone_number   = Butiker.LevTelefon
      ttPick_Up_Address.email          = Butiker.ePostAdresse
      ttPick_Up_Address.comment        = ''
      .
  END. /* HENTEBUTIKKADRESSE */
  
  /* Varelinjene */
  iAntLinjer = 0.
  FOR EACH KOrdreLinje OF KOrdrEHode NO-LOCK:
    IF KOrdreLinje.VareNr = 'BETALT' THEN 
      NEXT.
    IF KORdreLinje.VareTekst = 'FRAKT' THEN 
    OPPRETTFRAKT:
    DO:
      FIND LeveringsForm NO-LOCK WHERE 
        LeveringsForm.LevFNr = KOrdreHode.LevFNr NO-ERROR. 
      CREATE ttShipping_Option.
      ASSIGN
        ttShipping_Option.orderid         = KOrdreHode.KOrdre_Id 
        ttShipping_Option.shipping_id     = STRING(KOrdreHode.LevFNr)
        ttShipping_Option.line_id         = KOrdreLinje.Varetekst
        ttShipping_Option.amount          = KOrdreLinje.Linjesum
        ttShipping_Option.vat_amount      = (IF KOrdreLinje.MvaKr <> 0 THEN KOrdreLinje.MvaKr ELSE ROUND((KOrdreLinje.Linjesum * 0.20),2))
        ttShipping_Option.vat             = (IF KOrdreLinje.Mva% <> 0 THEN KOrdreLinje.Mva% ELSE 25)
        ttShipping_Option.shipping_title  = (IF AVAILABLE LeveringsForm THEN LeveringsForm.LevFormBeskrivelse ELSE '')
        ttShipping_Option.shipping_option_id  = (IF AVAILABLE LeveringsForm THEN LeveringsForm.LevFormMetode ELSE '')
        ttShipping_Option.description     = (IF AVAILABLE LeveringsForm THEN LeveringsForm.LevFormBeskrivelse ELSE '')
        ttShipping_Option.delivery_method =  (IF AVAILABLE LeveringsForm THEN LeveringsForm.LevFormMetode ELSE '')
        .      
    END. /* OPPRETTFRAKT */
    ELSE 
    OPPRETTVARELINJE:
    DO:
      IF AVAILABLE ArtBas THEN RELEASE ArtBas.
      FIND ArtBas NO-LOCK WHERE 
        ArtBas.ArtikkelNr = DEC(KOrdreLinje.VareNr) NO-ERROR.
      IF NOT AVAILABLE ArtBas AND KOrdreLinje.Kode <> '' THEN 
      DO:
        FIND Strekkode NO-LOCK WHERE 
          Strekkode.Kode = KOrdreLinje.Kode NO-ERROR.
        IF AVAILABLE Strekkode THEN 
          FIND ArtBas NO-LOCK WHERE 
            ArtBas.ArtikkelNr = Strekkode.ArtikkelNr NO-ERROR.
      END.
      IF NOT AVAILABLE ArtBas THEN 
        LEAVE OPPRETTVARELINJE.
      iAntLinjer = iAntLinjer + 1.
      CREATE ttItems.
      ASSIGN 
        ttItems.orderid     = KOrdreHode.KOrdre_Id     
        ttItems.id          = KOrdreLinje.Kode      
        ttItems.line_id     = STRING(iAntLinjer)      
        ttItems.Group_Id    = STRING(ArtBas.HovedKatNr)   
        ttItems.description = KOrdreLinje.Varetekst + ' ' + STRING(AVAILABLE ArtBAs) + ' ' + STRING(ArtBas.HovedKatNr) + ' ' + STRING(ArtBas.ArtikkelNr) 
        ttItems.quantity    = INT(KOrdreLinje.Antall)    
        ttItems.amount      = KOrdreLinje.Linjesum        
        ttItems.vat_amount  = (IF KOrdreLinje.MvaKr <> 0 THEN KOrdreLinje.MvaKr ELSE ROUND((KOrdreLinje.Linjesum * 0.20),2))
        ttItems.vat         = (IF KOrdreLinje.Mva% <> 0 THEN KOrdreLinje.Mva% ELSE 25)
        .
      IF NOT CAN-FIND(FIRST ttGroups WHERE 
                      ttGroups.Group_Id = STRING(ArtBas.HovedKatNr)) THEN 
      DO:
        FIND HovedKategori NO-LOCK WHERE 
          HovedKategori.HovedKatNr = ArtBas.HovedKatNr NO-ERROR.
        CREATE ttGroups.
        ASSIGN
          ttGroups.Group_Id  = STRING(ArtBas.HovedKatNr)
          ttGroups.GroupName = (IF AVAILABLE HovedKategori THEN HovedKategori.HovedKatTekst ELSE '') 
          .
      END.        
    END. /* OPPRETTVARELINJE */
  END.
  
END. /* BYGGDATASETT */

pbOk = TRUE.
RETURN ''.  
