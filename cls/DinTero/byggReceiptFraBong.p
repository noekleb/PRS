
/*------------------------------------------------------------------------
    File        : byggReceiptFraBong.p
    Purpose     : 

    Syntax      :

    Description : Bygger datasett dsReceipts fra dsBong.

    Author(s)   : Tom Nøkleby
    Created     : Sun Jan 24 12:34:21 CET 2021
    
    Notes       : Programmet tar hensyn til at dsBong kan innehode mer 
                  enn en bong. 
                  TN 27/3-21
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
BLOCK-LEVEL ON ERROR UNDO, THROW.

{cls\dintero\ttBong.i}
{cls\dintero\dsBong.i}
{cls\dintero\ttCustomerObj.i}
{cls\dintero\dsCustomerObj.i}
{cls\dintero\ttReceipts.i}
{cls\dintero\dsReceipts.i}

DEFINE INPUT        PARAMETER DATASET FOR dsBong.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsReceipts.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsCustCustomer.

DEFINE VARIABLE httItem     AS HANDLE NO-UNDO.
DEFINE VARIABLE plMvaKr     AS DECIMAL NO-UNDO.
DEFINE VARIABLE plRabKr     AS DECIMAL NO-UNDO.
DEFINE VARIABLE piAntLinjer AS INTEGER NO-UNDO.
DEFINE VARIABLE bTest AS LOG NO-UNDO.
DEFINE VARIABLE lBelop AS DECIMAL NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

bTest = FALSE.    

/* Tømmer datasettet før det fylles opp igjen. */
DATASET dsReceipts:EMPTY-DATASET ().
DATASET dsCustCustomer:EMPTY-DATASET ().

/* For å få riktig datoformat i JSon meldingen. */
SESSION:DATE-FORMAT = 'ymd'.

BONGBLOKK:
FOR EACH ttBongHode:
  /* Henter medlemmet hvis det er lagt inn på bongen. */
  IF ttBongHode.MedlemsNr > 0 THEN 
    FIND medlem NO-LOCK WHERE 
      Medlem.MedlemsNr = ttBongHode.MedlemsNr NO-ERROR.
  ELSE 
    RELEASE Medlem NO-ERROR.

  /* Er bongen allrede logget, skal den ikke gjøres no med her. */
  IF CAN-FIND(FIRST ttReceipts WHERE 
    ttReceipts.receipt_id = STRING(ttBongHode.b_id)) THEN 
    NEXT BONGBLOKK.
    
  plMvaKr = 0.
  plRabKr = 0.
  MVABLOKK:
  FOR EACH ttBongLinje WHERE 
    ttBongLinje.B_Id = ttBongHode.B_Id:
            
    plMvaKr = plMvaKr + (ttBongLinje.MvaKr * 100).        
    plRabKr = plRabKr + ((ttBongLinje.LinjeRab + ttBongLinje.SubtotalRab) * 100).

    /* Teller varelinjer. */
    IF (ttBongLinje.TTId >= 1 AND ttBongLinje.TTId <= 11) THEN 
      piAntLinjer = piantLinjer + 1.
  END. /* MVABLOKK */
    
  ASSIGN 
    lBelop = ttBongHode.Belop
    .  
  /* Summerer opp linjene. */  
  IF lBelop = 0 THEN 
    DO:
      FOR EACH ttBongLinje OF ttBongHode:
        IF NOT CAN-DO('1,3,10',STRING(ttBongLinje.TTId)) THEN 
          NEXT.
        lBelop = lBelop + ttBongLinje.LinjeSum.
      END.
    END.

  CREATE ttReceipts.
  ASSIGN 
    ttReceipts.receipt_id        = STRING(ttBongHode.b_id)
    ttReceipts.gross_amount      = (lBelop * 100) + plRabKr
    ttReceipts.net_amount        = lBelop * 100    
    ttReceipts.round_off_to_coin = 100
    ttReceipts.currency          = 'NOK'
    ttReceipts.purchase_at       = STRING(DATETIME(ttBongHode.Dato, ttBongHode.Tid * 1000),"9999-99-99THH:MM:SS") + 'Z' 
    ttReceipts.customer_id       = (IF AVAILABLE Medlem THEN Medlem.EksterntMedlemsNr ELSE '') 
    ttReceipts.no_of_items       = piantLinjer
    ttReceipts.total_discount    = plRabKr
    ttReceipts.operator_id       = STRING(ttBongHode.KassererNr)
    ttReceipts.operator_name     = ttBongHode.KassererNavn
    ttReceipts.salesperson_id    = STRING(ttBongHode.SelgerNr)
    ttReceipts.salesperson_name  = ttBongHode.SelgerNavn
    ttReceipts.comment           = ""
    .
    
  CREATE ttExtra_info.
  ASSIGN
    ttExtra_info.receipt_id = STRING(ttBongHode.b_id)
    ttExtra_info.cKey       = 'KasseNr'
    ttExtra_info.cValue     = STRING(ttbongHode.KasseNr)
    ttExtra_info.value_type = 'integer' 
    .  
  CREATE ttExtra_info.
  ASSIGN
    ttExtra_info.receipt_id = STRING(ttBongHode.b_id)
    ttExtra_info.cKey       = 'bongnr'
    ttExtra_info.cValue     = STRING(ttbongHode.BongNr)
    ttExtra_info.value_type = 'integer' 
    .  

  /* Legger opp info linjene. */  
  INFOLINJEBLOKK:
  FOR EACH ttBongLinje WHERE 
    ttBongLinje.B_Id = ttBongHode.B_Id AND 
    ttBongLinje.TTId = 95:

    FIND TransType NO-LOCK WHERE 
      TransType.TTId = ttBongLinje.TTId NO-ERROR.
    FIND TransBeskr NO-LOCK WHERE 
      TransBeskr.TTId = ttBongLinje.TTId AND 
      TransBeskr.TBId = (IF ttBongLinje.TBId = 0 THEN 1 ELSE ttBongLinje.TBId) NO-ERROR.
    CREATE ttExtra_info.
    ASSIGN 
      ttExtra_info.receipt_id = STRING(ttBongHode.B_Id)          
      ttExtra_info.cKey       = (IF AVAILABLE Transtype THEN TransType.Beskrivelse ELSE '')
      ttExtra_info.cValue     = ttBongLinje.BongTekst
      ttExtra_info.value_type = 'INFO'
      . 
          
  END. /* INFOLINJEBLOKK */
    
  IF AVAILABLE Medlem THEN
  DO:    
    CREATE ttCustCustomer.
    ASSIGN
      ttCustCustomer.customer_id    =  Medlem.EksterntMedlemsNr 
      ttCustCustomer.custType       = IF Medlem.MedType = 3 THEN 'customer' 
                                      ELSE IF Medlem.MedType = 2 THEN 'company'
                                      ELSE 'customer' 
      ttCustCustomer.date_of_birth  = IF Medlem.FodselsDato <> ? THEN
                                        STRING(Medlem.FodselsDato,"9999-99-99")
                                      ELSE
                                        ''
      ttCustCustomer.first_name     =  Medlem.ForNavn
      ttCustCustomer.last_name      = Medlem.EtterNavn
      ttCustCustomer.email          = Medlem.ePostAdresse 
      ttCustCustomer.phone_number   = Medlem.MobilTlf
      ttCustCustomer.custStatus     = IF Medlem.Aktiv THEN 'yes' ELSE 'no'
      ttCustCustomer.favorite_store = STRING(Medlem.ButikkNr)
      ttCustCustomer.gender         = IF Medlem.Kjonn THEN 'male' ELSE 'female' 
      .          
    CREATE ttCustAddresses.
    FIND Post NO-LOCK WHERE 
      Post.PostNr = Medlem.PostNr NO-ERROR.
    ASSIGN 
      ttCustAddresses.customer_id    = Medlem.EksterntMedlemsNr
      ttCustAddresses.addrType       = 'customer'
      ttCustAddresses.address_line   = Medlem.Adresse1
      ttCustAddresses.address_line_2 = Medlem.Adresse2
      ttCustAddresses.postal_code    = Medlem.PostNr
      ttCustAddresses.postal_place   = (IF AVAILABLE Post THEN Post.Beskrivelse ELSE '')
      ttCustAddresses.country        = Medlem.Land
      .
  END.  

  /* Gyldig butikk settes inn. */
  FIND Butiker NO-LOCK WHERE 
    Butiker.butik = ttBongHode.Butik NO-ERROR.
  IF AVAILABLE Butiker THEN 
    DO:    
      /* Butikk */  
      CREATE ttStore.
      ASSIGN 
        ttStore.receipt_id = STRING(ttBongHode.B_Id)
        ttStore.id         = STRING(Butiker.Butik)
        ttStore.cName      = Butiker.ButNamn
        ttStore.organization_number = Butiker.OrganisasjonsNr
        .
      FIND Post NO-LOCK WHERE 
        Post.PostNr = Butiker.BuPadr NO-ERROR.
      CREATE ttStoreAdress.
      ASSIGN 
        ttStoreAdress.receipt_id = STRING(ttBongHode.B_Id)
        ttStoreAdress.store_id = STRING(Butiker.Butik)
        ttStoreAdress.address_line = Butiker.BuAdr
        ttStoreAdress.address_line_2 = ''
        ttStoreAdress.postal_code = Butiker.BuPadr
        ttStoreAdress.postal_place = (IF AVAILABLE Post THEN Post.Beskrivelse ELSE '')
        ttStoreAdress.country = 'NO'
        .
    END.
    
  /* Legger opp varelinjene. */  
  VARELINJEBLOKK:
  FOR EACH ttBongLinje WHERE 
    ttBongLinje.B_Id = ttBongHode.B_Id AND 
    ttBongLinje.Antall <> 0:

    /* Bare varelinjene har varegruppe påført. */
    IF NOT ttBongLinje.VareGr > 0 THEN
      NEXT VARELINJEBLOKK. 

    FIND ArtBas NO-LOCK WHERE 
      ArtBas.ArtikkelNr = DEC(ttBongLinje.ArtikkelNr) NO-ERROR.
        
    CREATE ttItem.
    ASSIGN 
      ttItem.receipt_id                 = STRING(ttBongHode.B_Id)
      ttItem.line_id                    = ttBongLinje.LinjeNr
      ttItem.item_id                    = (IF AVAILABLE ArtBas THEN (ArtBas.LevKod + '/' + ArtBas.LevFargKod) ELSE '')
      ttItem.quantity                   = ttBongLinje.Antall
      ttItem.unit                       = (IF AVAILABLE ArtBas THEN ArtBas.SalgsEnhet ELSE '')
      ttItem.description                = (IF AVAILABLE ArtBAs THEN ArtBas.Beskr ELSE ttBongLinje.BongTekst)
      ttItem.description_alias          = ttBongLinje.BongTekst
      ttItem.net_amount                 = (ttBongLinje.Linjesum - (ttBongLinje.LinjeRab + ttBongLinje.SubtotalRab)) * 100
      ttItem.gross_amount               = ttBongLinje.Linjesum * 100
      ttItem.tax_percent                = ttBongLinje.Mva%
      ttItem.barcode                    = ttBongLinje.Strekkode
      ttItem.cost_price                 = ttBongLinje.VVarekost * 100
      ttItem.voided                     = ttBongLinje.Makulert
      ttItem.included_in_total_discount = (ttBongLinje.LinjeRab + ttBongLinje.SubtotalRab) > 0
      ttItem.is_return_item             = ttBongLinje.Antall < 0
      ttItem.is_virtual_product         = (IF AVAILABLE ArtBas THEN ArtBas.OPris ELSE FALSE)
      ttItem.comment                    = 'Farge: ' + (IF AVAILABLE ArtBas THEN ArtBas.LevFargKod ELSE '') + 
                           ' Str: ' + ttBongLinje.Storrelse 
      .
    IF AVAILABLE ArtBas AND 
      (ArtBas.ManRabIKas = TRUE OR ArtBas.KundeRabatt = TRUE OR ArtBas.Bonus_Givende = TRUE) THEN       
      ttItem.eligible_for_discount = TRUE. 

    /* Oppretter gruppene hvis det er lagt inn på artikkelen. */
    IF AVAILABLE ArtBas THEN 
    DO:
      CREATE ttItemDimension.
      ASSIGN 
        ttItemDimension.receipt_id = ttItem.receipt_id  
        ttItemDimension.line_id    = ttItem.line_id
        ttItemDimension.cColor     = ArtBas.LevFargKod
        ttItemDimension.cSize      = ttBongLinje.Storrelse 
        ttItemDimension.cVariant   = ArtBas.LevKod
        .
              
      FIND Anv-Kod NO-LOCK WHERE 
        Anv-Kod.anv-Id = ArtBas.anv-id NO-ERROR.
      IF AVAILABLE Anv-Kod AND Anv-Kod.anv-Id > 0 THEN 
      DO:
        CREATE ttItemGroups.
        ASSIGN
          ttItemGroups.receipt_id = ttItem.receipt_id  
          ttItemGroups.line_id    = ttItem.line_id
          ttItemGroups.group_id   = STRING(Anv-Kod.anv-Id)
          ttItemGroups.group_name = Anv-Kod.AnvBeskr
          .
      END.
      FIND HovedKategori NO-LOCK WHERE 
        HovedKategori.HovedKatNr = ArtBas.HovedKatNr NO-ERROR.
      IF AVAILABLE HovedKategori AND HovedKategori.HovedKatNr > 0 THEN 
      DO:
        CREATE ttItemGroups.
        ASSIGN
          ttItemGroups.receipt_id = ttItem.receipt_id
          ttItemGroups.line_id    = ttItem.line_id
          ttItemGroups.group_id   = STRING(HovedKategori.HovedKatNr)
          ttItemGroups.group_name = HovedKategori.HovedKatTekst
          .
      END. 
    END.
  END. /* VARELINJEBLOKK */
  
END. /* BONGBLOKK */

IF bTest THEN 
  DO:
    FIND FIRST ttReceipts NO-LOCK NO-ERROR.
    IF AVAILABLE ttReceipts THEN 
      DATASET dsReceipts:write-json('file', 'konv\ByggReceiptsFraBongReceipt' + ttReceipts.receipt_id + '.json', FALSE).
    FIND FIRST ttCustcustomer NO-LOCK NO-ERROR.
    IF AVAILABLE ttCustCustomer THEN
      DATASET dsCustcustomer:write-json('file', 'konv\ByggReceiptsFraBongCustcustomer' + ttCustcustomer.customer_id + '.json', FALSE).
  END.      

/* Setter tilbake dato formatet. */
SESSION:DATE-FORMAT = 'dmy'.

        