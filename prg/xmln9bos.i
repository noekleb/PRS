DEFINE {&NEW} SHARED TEMP-TABLE TT_Receipt NO-UNDO
    FIELD ReceiptSeq     AS INTE
    FIELD iRownr         AS INTE
    FIELD terminalType   AS CHAR      /* pos, opt, attended */
    FIELD terminalId     AS INTE      /* 1-Max POS or Max OPT */
    FIELD shiftId        AS INTE      /* never reset        */
    FIELD receiptId      AS INTE      /* never reset        */
    FIELD receiptNumber  AS DECIMAL   /* wraps              */
    FIELD receiptType    AS CHAR      /* sale, refund, void */
    FIELD CannisterNumber AS INTE     /* Vid drop */
    FIELD dato           AS DATE
    FIELD tid            AS INTE
    FIELD amount         AS DECI
    FIELD cardamount     AS DECIMAL   /* vi loggar hur mycket för att se om växel är cashback */
    FIELD utecash        AS LOG       /* om vi har betalat i sedelautomet, försäljningssumman skall avrundas */
    FIELD ptypetargetId  AS INTE      /* vid inoutpayment */
    FIELD currency       AS CHAR
    FIELD currencyAmount AS DECI
    INDEX Seq IS PRIMARY UNIQUE ReceiptSeq.

DEFINE {&NEW} SHARED TEMP-TABLE TT_ItemLine NO-UNDO  /* Only in "sale", "refund" and "void" receipts. */
    FIELD ReceiptSeq    AS INTE
    FIELD Rownr         AS INTE
    FIELD itemType      AS CHAR     /* @itemType is "fuel", "sku" …                                                                */
    FIELD lineType      AS CHAR     /* @lineType is "normal" for registration or "corr" for correction.                            */
    FIELD ItemId        AS DECI     /*                                                                                             */
    FIELD BarCode       AS DECI     /* Only reported when the Item was scanned.                                                    */
    FIELD Quantity      AS DECIMAL  /* Sold quantity.                                                                              */
    FIELD UnitPrice     AS DECIMAL  /*                                                                                             */
    FIELD Amount        AS DECIMAL  /* Total line amount, after discounts.                                                         */
    FIELD AccountId     AS INTE     /*                                                                                             */
    FIELD EnterpriseId  AS INTE     /*                                                                                             */
    FIELD UserDefinedId AS INTE     /*                                                                                             */
    FIELD EFTCodeId     AS INTE     /*                                                                                             */
    FIELD SalesType     AS CHAR     /* Only used when for item with more then one consumption place, i.e. "takeaway" else "eatin". */
    FIELD TaxClassId    AS INTE     /* 1 - 10                                                                                      */
    FIELD TaxAmount     AS DECI     /* After discounts.                                                                            */
    FIELD Man_discount             AS DECIMAL  /* Manual The total discount amount.                                                                               */
    FIELD Man_discountType         AS CHAR  /* Manual "%" or "amount".                                                                                         */
    FIELD Prom_Discount             AS DECIMAL     /* Promotion The total discount amount.                                                                               */
    FIELD Prom_CampaignId           AS INTE     /* Promotion CampaignId that triggered the discount.                                                                  */
    FIELD Prom_PromotionId          AS INTE     /* PromotionId that triggered the discount.                                                                 */
    FIELD Prom_CampaignOwnerId      AS INTE     /* Promotion The owner Id of the campaign.                                                                            */
    FIELD Pack_discount            AS DECI     /* @discount is the total discount amount.                                                                  */
    FIELD ParentItemId         AS INTE     /* Only used when the item is a child sold in a linked product.                                             */
    FIELD IsSupressedOnReceipt AS CHAR     /* true if the item was not printed on the receipt. Only used when the item is a child in a linked product. */
    FIELD registrationType   AS CHAR     /* @registrationType is either "pump" or "manual".                             */
    FIELD PumpId             AS INTE     /* Omitted for "manual" registration.                                          */
    FIELD NozzleId           AS INTE     /* Omitted for "manual" registration.                                          */
    FIELD FuelGradeId        AS INTE     /* 1 - max fuel grade number                                                   */
    FIELD PriceGroup         AS CHAR     /* See Filling.                                                                */
    FIELD FillIingId         AS DECI     /* Running filling sequence never reset.                                       */
    FIELD FillIingSequenceId AS INTE     /* Running filling sequence number, from pump control. Resets to 1 after 9999. */
    INDEX Seq IS PRIMARY UNIQUE ReceiptSeq RowNr.

DEFINE {&NEW} SHARED TEMP-TABLE TT_TenderLine NO-UNDO  /* Only in "sale", "refund" and "void" receipts. */
    FIELD ReceiptSeq    AS INTE
    FIELD Rownr         AS INTE
    FIELD isExchange    AS LOG     /* true if exchange money to customer        */
    FIELD paymentType   AS CHAR    /* cash,card,voucher,customeraccount,other,driveoff,rounding */
    FIELD subPaymentId  AS INTE    /* card:1-100,voucher:1-20,other:1-20 */
    FIELD currencyId    AS CHAR    /* vid cash */
    FIELD domesticValue AS DECI
    FIELD amountValue   AS DECI
    INDEX Seq IS PRIMARY UNIQUE ReceiptSeq RowNr.

DEFINE {&NEW} SHARED TEMP-TABLE TT_Shift NO-UNDO
    FIELD ButikkNr AS INTE
    FIELD Dato     AS DATE
    FIELD OpenTid  AS INTE
    FIELD TermTid  AS INTE
    FIELD TermDato AS DATE
    FIELD SkiftId  AS DECI
    FIELD BokforingsId AS DECI
/*     FIELD KassererNr AS INTE     */
    FIELD SkiftNr  AS DECI
    FIELD sequenceNumber AS DECIMAL
    FIELD TermDatoTid AS DECI
    FIELD terminert AS LOG.

DEFINE {&NEW} SHARED TEMP-TABLE TT_Period NO-UNDO
    FIELD ButikkNr AS INTE
    FIELD Dato     AS DATE
    FIELD OpenTid  AS INTE
    FIELD TermTid  AS INTE
    FIELD TermDatoTid AS DECI
    FIELD periodId AS DECI.

DEFINE {&NEW} SHARED TEMP-TABLE TT_TaxSum NO-UNDO
    FIELD ReceiptSeq AS INTE
    FIELD ReceiptNumber AS DECIMAL
/*     FIELD ButikkNr   AS INTE */
/*     FIELD TerminalId AS INTE */
    FIELD taxClassId AS INTE
    FIELD taxAmount  AS DECI.

DEFINE {&NEW} SHARED TEMP-TABLE TT_BongShift NO-UNDO
    FIELD ShiftId AS DECI
    INDEX ShiftId ShiftId.
