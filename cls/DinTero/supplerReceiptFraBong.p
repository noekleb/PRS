
/*------------------------------------------------------------------------
    File        : supplerReceiptFraBong.p
    Purpose     : 

    Syntax      :

    Description : Supplerer informasjonen i datasettet dsReceipts med info fra dsBong.

    Author(s)   : Tom nøkleby
    Created     : Sun Jan 24 12:58:18 CET 2021
    Notes       :
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
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dscustCustomer.

DEFINE VARIABLE plMvaKr AS INTEGER NO-UNDO.
DEFINE VARIABLE piMvaGr AS INTEGER NO-UNDO.
DEFINE VARIABLE piMva%  AS INTEGER NO-UNDO.
DEFINE VARIABLE bTest AS LOG NO-UNDO.

DEFINE BUFFER bttBongLinje FOR ttBongLinje.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

bTest = TRUE.

BONGHODEBLOKK:
FOR EACH ttBongHode:
  
  IF NOT CAN-FIND(FIRST ttTax_Lines WHERE 
    ttTax_Lines.receipt_id = STRING(ttBongHode.B_Id)) THEN
    MVAHODEDBLOKK: 
    DO:

      plMvaKr = 0.
      piMvaGr = 0.
      MVABLOKK:
      FOR EACH ttBongLinje WHERE 
        ttBongLinje.B_Id = ttBongHode.B_Id:
        plMvaKr = plMvaKr + (ttBongLinje.MvaKr * 100).
        piMvaGr = IF piMvaGr = 0 THEN ttBongLinje.MvaGr ELSE piMvaGr.
        piMva%  = IF piMva% = 0 THEN ttBongLinje.Mva% * 100 ELSE piMva%.        
      END. /* MVABLOKK */
      
      FIND FIRST Moms NO-LOCK WHERE 
        Moms.MomsProc = piMva% NO-ERROR.

      CREATE ttTax_Lines.
      ASSIGN 
        ttTax_Lines.receipt_id = STRING(ttBongHode.B_Id)
        ttTax_Lines.tax_code   = STRING(piMvaGr)
        ttTax_Lines.amount     = plMvaKr
        ttTax_Lines.exempt     = FALSE
        ttTax_Lines.included_in_price = TRUE
        ttTax_Lines.percentage = piMva%
        ttTax_Lines.tax_basis  = plMvaKr
        ttTax_Lines.tax_group  = IF AVAILABLE Moms THEN Moms.Beskrivelse ELSE ''
        . 
    END. /* MVAHODEDBLOKK */
  
  /* Legger opp betalingslinjene. */  
  BETALLINJEBLOKK:
  FOR EACH ttBongLinje WHERE 
    ttBongLinje.B_Id = ttBongHode.B_Id:

    /* Mva linje spes. */
    IF NOT CAN-FIND(FIRST ttItemTax_Lines WHERE 
      ttItemTax_Lines.receipt_id = STRING(ttBongHode.B_Id) AND 
      ttItemTax_Lines.line_id = ttBongLinje.LinjeNr) THEN 
    TAXLINE:
    DO:
      IF ttBongLinje.TTId > 11 THEN 
        LEAVE TAXLINE.

      FIND Moms NO-LOCK WHERE 
        Moms.MomsKod = ttBongLinje.MvaGr NO-ERROR.
      CREATE ttItemTax_Lines.
      ASSIGN  
        ttItemTax_Lines.receipt_id = STRING(ttBongHode.B_Id)
        ttItemTax_Lines.line_id = ttBongLinje.LinjeNr
        ttItemTax_Lines.tax_code = STRING(ttBongLinje.MvaGr) 
        ttItemTax_Lines.amount = ttBongLinje.LinjeSum 
        ttItemTax_Lines.exempt = FALSE 
        ttItemTax_Lines.included_in_price = TRUE 
        ttItemTax_Lines.percentage = ttBongLinje.Mva%
        ttItemTax_Lines.tax_basis = ttBongLinje.LinjeSum 
        ttItemTax_Lines.tax_group = IF AVAILABLE Moms THEN Moms.Beskrivelse ELSE ''  
        . 
        
    END.

    /* Ikke bongInfo linjer. */
    IF ttBongLinje.TTId = 95 THEN 
      NEXT BETALLINJEBLOKK.

    /* Bare varelinjene har varegruppe påført. Betalingslinjenjene har 0 i dette feltet. */
    IF NOT ttBongLinje.VareGr = 0 THEN
      NEXT BETALLINJEBLOKK. 
    
    /* Payment */
    IF NOT CAN-FIND(FIRST ttPayment WHERE 
      ttPayment.receipt_id   = STRING(ttBongHode.B_Id) AND           
      ttPayment.line_id      = ttBongLinje.LinjeNr) THEN 
    PAYBLOKK:
    DO:
      /* SelgerNr. */
      IF ttBongLinje.TTId = 146 THEN 
        LEAVE PAYBLOKK.
        
      FIND TransType NO-LOCK WHERE 
        TransType.TTId = ttBongLinje.TTId NO-ERROR.
      FIND TransBeskr NO-LOCK WHERE 
        TransBeskr.TTId = ttBongLinje.TTId AND 
        TransBeskr.TBId = (IF ttBongLinje.TBId = 0 THEN 1 ELSE ttBongLinje.TBId) NO-ERROR.
      CREATE ttPayment.
      ASSIGN 
        ttPayment.receipt_id   = STRING(ttBongHode.B_Id)          
        ttPayment.line_id      = ttBongLinje.LinjeNr
        ttPayment.amount       = ttBongLinje.LinjeSum * 100
        ttPayment.voided       = ttBongLinje.Makulert
        ttPayment.cDescription = (IF AVAILABLE TransType THEN TransType.Beskrivelse ELSE '') + '/' + 
                                     (IF AVAILABLE TransBeskr THEN TransBeskr.Beskrivelse ELSE '')
        .
      IF ttBongLinje.TTId = 52 AND AVAILABLE TransType THEN 
        DO:
          FIND ttCard_Info WHERE 
            ttCard_Info.receipt_id = STRING(ttBongHode.B_Id) AND 
            ttCard_Info.line_id    = ttBongLinje.LinjeNr AND 
            ttCard_Info.card_type  = TransType.Beskrivelse NO-ERROR.
          IF NOT AVAILABLE ttCard_info THEN 
            DO:
              CREATE ttCard_Info.
              ASSIGN 
                ttCard_Info.receipt_id = STRING(ttBongHode.B_Id) 
                ttCard_Info.line_id    = ttBongLinje.LinjeNr 
                ttCard_Info.card_type  = TransType.Beskrivelse
                .
              FIND FIRST bttBongLinje WHERE 
                bttBongLinje.B_Id = ttBongHode.B_Id AND 
                bttBongLinje.TTId = 95 AND 
                bttBongLinje.BongTekst BEGINS 'Bax:' NO-ERROR.
              IF NOT AVAILABLE bttBongLinje THEN
                FIND FIRST bttBongLinje WHERE
                  bttBongLinje.B_Id = ttBongHode.B_Id AND
                  bttBongLinje.TTId = 95 AND
                  bttBongLinje.BongTekst BEGINS 'REF:' NO-ERROR.
              IF AVAILABLE bttBongLinje THEN 
                ttCard_Info.receipt = bttBongLinje.BongTekst.

              FIND FIRST bttBongLinje WHERE 
                bttBongLinje.B_Id = ttBongHode.B_Id AND 
                bttBongLinje.TTId = 95 AND 
                bttBongLinje.BongTekst BEGINS '*****' NO-ERROR.
              IF NOT AVAILABLE bttBongLinje THEN
                FIND FIRST bttBongLinje WHERE
                  bttBongLinje.B_Id = ttBongHode.B_Id AND
                  bttBongLinje.TTId = 95 AND
                  bttBongLinje.BongTekst BEGINS 'BankAx' NO-ERROR.
              IF AVAILABLE bttBongLinje THEN 
                ttCard_Info.card_number = bttBongLinje.BongTekst.
            END.
        END. 
    END. /* PAYBLOKK*/ 
  END. /* BETALLINJEBLOKK */

END. /* BONGHODEBLOKK */

IF bTest THEN 
  DO:
    FIND FIRST ttReceipts NO-LOCK NO-ERROR.
    IF AVAILABLE ttReceipts THEN 
      DATASET dsReceipts:WRITE-JSON('file', 'konv\SupplerReceiptsFraBong' + ttReceipts.receipt_id + '.json', FALSE).
  END.      



      