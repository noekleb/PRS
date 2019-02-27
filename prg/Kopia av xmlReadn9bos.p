&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* DEFINE INPUT  PARAMETER cFilename AS CHARACTER  NO-UNDO. */
/* DEFINE VARIABLE cFilename AS CHARACTER  NO-UNDO. */

DEFINE INPUT  PARAMETER iButikkNr AS INTEGER    NO-UNDO.
DEFINE INPUT  PARAMETER cFileName AS CHARACTER  NO-UNDO.

DEFINE VARIABLE hDoc AS HANDLE NO-UNDO.
DEFINE VARIABLE hRoot AS HANDLE NO-UNDO.
DEFINE VARIABLE hTable AS HANDLE NO-UNDO.
DEFINE VARIABLE hReceipt AS HANDLE NO-UNDO.
DEFINE VARIABLE hReceiptRows AS HANDLE NO-UNDO.
DEFINE VARIABLE hItemLine AS HANDLE NO-UNDO.
DEFINE VARIABLE hItemLineValues AS HANDLE NO-UNDO.
DEFINE VARIABLE hTenderLine AS HANDLE     NO-UNDO.
DEFINE VARIABLE hTenderAmount AS HANDLE     NO-UNDO.
DEFINE VARIABLE hTenderAmountValue AS HANDLE     NO-UNDO.
DEFINE VARIABLE hField2 AS HANDLE NO-UNDO.
DEFINE VARIABLE hText2 AS HANDLE NO-UNDO.
DEFINE VARIABLE hBuf AS HANDLE NO-UNDO.
DEFINE VARIABLE hDBFld AS HANDLE NO-UNDO.
DEFINE VARIABLE hState AS HANDLE NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE j AS INTEGER NO-UNDO.
DEFINE VARIABLE hPosEvent AS HANDLE     NO-UNDO.
DEFINE VARIABLE cName AS CHARACTER  NO-UNDO.

/* {xmln9bos.i &NEW=NEW} */
{xmln9bos.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-getDato) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDato Procedure 
FUNCTION getDato RETURNS DATE
  ( INPUT cYYYYMMDD AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTid) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTid Procedure 
FUNCTION getTid RETURNS INTEGER
  ( INPUT cHHMMSS AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15.91
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* cFilename = "posevent.xml". */

RUN LesInnFil.
                   
IF RETURN-VALUE = "ERROR" THEN 
    RETURN "ERROR".
ELSE 
    RETURN "OK".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-LesInnFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LesInnFil Procedure 
PROCEDURE LesInnFil PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cValue AS CHAR NO-UNDO.
DEF VAR dDato  AS DATE NO-UNDO.
DEF VAR iTime  AS INT  NO-UNDO.
DEF VAR ii AS INT NO-UNDO.
DEF VAR jj AS INT NO-UNDO.
DEF VAR kk AS INT NO-UNDO.
DEF VAR ll AS INT NO-UNDO.
DEF VAR mm AS INT NO-UNDO.
DEF VAR iReceiptSeq AS INT NO-UNDO.
DEF VAR iRowNr AS INT NO-UNDO.
DEF VAR dAmount AS DECI NO-UNDO.
DEF VAR cTL_Names AS CHAR NO-UNDO.
DEF VAR cTA_Names AS CHAR NO-UNDO.
DEF VAR dReceiptAmount AS DEC NO-UNDO.
DEF VAR dSkiftId  AS DEC NO-UNDO.
CREATE X-DOCUMENT hDoc.
CREATE X-NODEREF hRoot.
CREATE X-NODEREF hTable.
CREATE X-NODEREF hReceipt.
CREATE X-NODEREF hReceiptRows.
CREATE X-NODEREF hPosEvent.
CREATE X-NODEREF hItemLine.
CREATE X-NODEREF hItemLineValues.
CREATE X-NODEREF hTenderLine.
CREATE X-NODEREF hTenderAmount.
CREATE X-NODEREF hTenderAmountValue.
CREATE X-NODEREF hField2.
CREATE X-NODEREF hText2.
CREATE X-NODEREF hState.
IF NOT hDoc:LOAD ("file",cFileName, FALSE) THEN
    RETURN "ERROR".
IF NOT hDoc:GET-DOCUMENT-ELEMENT (hRoot) THEN /* Get root of structure */
    RETURN "ERROR".
    /* Riktigt dokument ?. */
IF hRoot:NAME <> "POSBOFile" THEN
    RETURN "ERROR".
REPEAT i = 1 TO hRoot:NUM-CHILDREN:
    hRoot:GET-CHILD (hPosEvent,i). /* Här har vi alla element till våra kvitton */
    cNAME = hPosEvent:ATTRIBUTE-NAMES.
    REPEAT j = 1 TO hPosEvent:NUM-CHILDREN:
      hPosEvent:GET-CHILD (hReceipt,j).
      /* skip any null values */
      IF hReceipt:NUM-CHILDREN < 1 THEN
          NEXT.
      IF hReceipt:NAME = "Receipt" OR hReceipt:NAME = "InOutReceipt" OR hReceipt:NAME = "SafeDropReceipt" OR 
         hReceipt:NAME = "FinancialReceipt" OR hReceipt:NAME = "ExchangeReceipt" OR hReceipt:NAME = "CustomerAccountInPayReceipt" THEN DO:
          IF NOT DECI(hReceipt:GET-ATTRIBUTE ("receiptNumber")) > 0 THEN
              NEXT.
          ASSIGN dDato = getDato(hReceipt:GET-ATTRIBUTE ("date"))
                 iTime = getTid(hReceipt:GET-ATTRIBUTE ("time")).
          CREATE TT_Receipt.
          ASSIGN iReceiptSeq               = iReceiptSeq + 1
                 TT_Receipt.ReceiptSeq     = iReceiptSeq
                 TT_Receipt.terminalType   = hReceipt:GET-ATTRIBUTE ("terminalType")
                 TT_Receipt.terminalId     = INT(hReceipt:GET-ATTRIBUTE ("terminalId"))
                 TT_Receipt.shiftId        = INT(hReceipt:GET-ATTRIBUTE ("shiftId"))
                 TT_Receipt.receiptId      = INT(hReceipt:GET-ATTRIBUTE ("receiptId"))
                 TT_Receipt.receiptNumber  = DECI(hReceipt:GET-ATTRIBUTE ("receiptNumber"))
                 TT_Receipt.receiptType    = IF hReceipt:NAME = "SafeDropReceipt" THEN "drop" ELSE IF hReceipt:NAME = "FinancialReceipt" THEN "sale" ELSE 
                                             IF hReceipt:NAME = "CustomerAccountInPayReceipt" THEN "CustomerAccountInPayReceipt" ELSE hReceipt:GET-ATTRIBUTE ("receiptType")
                 /* FinancialReceipt = inbetalning på preemkort */
                 TT_Receipt.dato           = dDato
                 TT_Receipt.tid            = iTime
                 iRowNr                    = 0
                 dReceiptAmount            = 0.
          IF NOT CAN-FIND(FIRST TT_BongShift WHERE TT_BongShift.ShiftId = TT_Receipt.shiftId) THEN DO:
              CREATE TT_BongShift.
              ASSIGN TT_BongShift.ShiftId = TT_Receipt.shiftId.
          END.
          /* Här tar vi hand om alla kvittoelement */
          REPEAT ii = 1 TO hReceipt:NUM-CHILDREN:
            hReceipt:GET-CHILD (hReceiptRows,ii).
            CASE hReceiptRows:NAME:
              WHEN "ItemLines" OR WHEN "FuelItemLines" THEN DO:
                REPEAT jj = 1 TO hReceiptRows:NUM-CHILDREN:
                    hReceiptRows:GET-CHILD (hItemLine,jj).
                    CREATE TT_ItemLine.
                    ASSIGN iRowNr                           = iRowNr + 1
                           TT_ItemLine.ReceiptSeq           = TT_Receipt.ReceiptSeq
                           TT_ItemLine.Rownr                = iRowNr
                           TT_ItemLine.itemType             = IF hReceiptRows:NAME = "FuelItemLines" THEN "fuel" ELSE hItemLine:GET-ATTRIBUTE ("itemType")
                           TT_ItemLine.lineType             = hItemLine:GET-ATTRIBUTE ("lineType").
                    REPEAT kk = 1 TO hItemLine:NUM-CHILDREN:
                      hItemLine:GET-CHILD (hItemLineValues,kk).
/*                       IF receiptid = 166100 THEN */
                      IF CAN-DO("ItemId,BarCode,Quantity,UnitPrice,Amount,AccountId,EnterpriseId,UserDefinedId,EFTCodeId,SalesType,TaxClassId,TaxAmount",hItemLineValues:NAME) THEN DO:
                         hItemLineValues:GET-CHILD (hText2,1).
                         CASE hItemLineValues:NAME:
                             WHEN "ItemId"        THEN TT_ItemLine.ItemId         = DECI(hText2:NODE-VALUE).
                             WHEN "BarCode"       THEN TT_ItemLine.BarCode       = DECI(hText2:NODE-VALUE).
                             WHEN "Quantity"      THEN TT_ItemLine.Quantity      = ROUND(DECI(REPLACE(hText2:NODE-VALUE,".",",")),3).
                             WHEN "UnitPrice"     THEN TT_ItemLine.UnitPrice     = ROUND(DECI(REPLACE(hText2:NODE-VALUE,".",",")),2).
                             WHEN "Amount"        THEN ASSIGN TT_ItemLine.Amount = ABS(ROUND(DECI(REPLACE(hText2:NODE-VALUE,".",",")),2)) /* vid corr är antalet negativt och amount negativt */
                                                        dReceiptAmount = dReceiptAmount + IF TT_ItemLine.LineType = "normal" THEN TT_ItemLine.Amount ELSE -1 * TT_ItemLine.Amount. /* else corr */
                             WHEN "AccountId"     THEN TT_ItemLine.AccountId     = INT(hText2:NODE-VALUE).
                             WHEN "EnterpriseId"  THEN TT_ItemLine.EnterpriseId  = INT(hText2:NODE-VALUE).
                             WHEN "UserDefinedId" THEN TT_ItemLine.UserDefinedId = INT(hText2:NODE-VALUE).
                             WHEN "EFTCodeId"     THEN TT_ItemLine.EFTCodeId     = INT(hText2:NODE-VALUE).
                             WHEN "SalesType"     THEN TT_ItemLine.SalesType     = hText2:NODE-VALUE.
                             WHEN "TaxClassId"    THEN TT_ItemLine.TaxClassId    = INT(hText2:NODE-VALUE).
                             WHEN "TaxAmount"     THEN TT_ItemLine.TaxAmount     = ABS(ROUND(DECI(REPLACE(hText2:NODE-VALUE,".",",")),2)).
                         END CASE.
                      END.
                      ELSE IF hItemLineValues:NAME = "ManualDiscount" THEN DO:
                          IF CAN-DO(hItemLineValues:ATTRIBUTE-NAMES,"discount") THEN
                              TT_ItemLine.Man_discount = ROUND(DECI(REPLACE(hItemLineValues:GET-ATTRIBUTE ("discount"),".",",")),2).
                      END.
                      ELSE IF hItemLineValues:NAME = "PromotionDiscount" THEN DO:
                          IF CAN-DO(hItemLineValues:ATTRIBUTE-NAMES,"Discount") THEN
                              TT_ItemLine.Prom_Discount = ROUND(DECI(REPLACE(hItemLineValues:GET-ATTRIBUTE ("Discount"),".",",")),2).
                          REPEAT ll = 1 TO hItemLineValues:NUM-CHILDREN:
                              hItemLineValues:GET-CHILD (hField2,ll).
                              IF hField2:NUM-CHILDREN > 0 THEN DO:
                                  hField2:GET-CHILD (hText2,1).
                                  CASE hField2:NAME:
                                      WHEN "CampaignId" THEN TT_ItemLine.Prom_CampaignId = INT(hText2:NODE-VALUE).
                                      WHEN "PromotionId" THEN TT_ItemLine.Prom_PromotionId = INT(hText2:NODE-VALUE).
                                      WHEN "CampaignOwnerId" THEN TT_ItemLine.Prom_CampaignOwnerId = INT(hText2:NODE-VALUE).
                                  END CASE.
                              END.
                          END.
                      END.
/*  .Man_discountType     = hItemLineValues:GET-ATTRIBUTE ("discountType")                               */
/*  .Pack_discount        = ROUND(DECI(REPLACE(hItemLineValues:GET-ATTRIBUTE ("discount"),".",",")),2)   */
/*  .ParentItemId         = INT(hItemLineValues:GET-ATTRIBUTE ("ParentItemId"))                          */
/* .IsSupressedOnReceipt = hItemLineValues:GET-ATTRIBUTE ("IsSupressedOnReceipt")                       */
/* .registrationType     = hItemLineValues:GET-ATTRIBUTE ("registrationType")                           */
/* .PumpId               = INT(hItemLineValues:GET-ATTRIBUTE ("PumpId"))                                */
/* .NozzleId             = INT(hItemLineValues:GET-ATTRIBUTE ("NozzleId"))                              */
/* .FuelGradeId          = INT(hItemLineValues:GET-ATTRIBUTE ("FuelGradeId"))                           */
/* .PriceGroup           = hItemLineValues:GET-ATTRIBUTE ("PriceGroup")                                 */
/* .FillIingId           = DECI(hItemLineValues:GET-ATTRIBUTE ("FillIingId"))                           */
/* .FillIingSequenceId   = INT(hItemLineValues:GET-ATTRIBUTE ("FillIingSequenceId")).                   */
                  END.
                END.
              END.
              WHEN "TenderLines" THEN DO:
                REPEAT ll = 1 TO hReceiptRows:NUM-CHILDREN:
                  hReceiptRows:GET-CHILD (hTenderLine,ll).
                  IF hTenderLine:NAME = "TenderLine" OR hTenderLine:NAME = "CustomerAccountTenderLine" THEN DO:
                    REPEAT mm = 1 TO hTenderLine:NUM-CHILDREN:
                      hTenderLine:GET-CHILD (hTenderAmount,mm).
                      ASSIGN dAmount = 0.
                      IF hTenderAmount:NUM-CHILDREN = 1 THEN DO:
                         hTenderAmount:GET-CHILD (hTenderAmountValue,1).
                         ASSIGN dAmount = ROUND(DECI(REPLACE(hTenderAmountValue:NODE-VALUE,".",",")),2).
                      END.
                      ASSIGN cTL_Names = hTenderLine:ATTRIBUTE-NAMES.
                      ASSIGN cTA_Names = hTenderAmount:ATTRIBUTE-NAMES.
                      CREATE TT_TenderLine.
                      ASSIGN iRowNr                     = iRowNr + 1
                             TT_TenderLine.ReceiptSeq   = TT_Receipt.ReceiptSeq
                             TT_TenderLine.Rownr        = iRowNr
                             TT_TenderLine.isExchange   = IF CAN-DO(cTL_Names,"isExchange") THEN hTenderLine:GET-ATTRIBUTE ("isExchange") = "true" ELSE FALSE
                             TT_TenderLine.paymentType  = IF CAN-DO(cTL_Names,"paymentType") THEN hTenderLine:GET-ATTRIBUTE ("paymentType") ELSE hTenderLine:NAME
                             TT_TenderLine.subPaymentId = IF CAN-DO(cTL_Names,"subPaymentId") THEN INT(hTenderLine:GET-ATTRIBUTE ("subPaymentId")) ELSE ?
                             TT_TenderLine.currencyId   = IF CAN-DO(cTA_Names,"currencyId") THEN hTenderAmount:GET-ATTRIBUTE ("currencyId") ELSE ""
                             TT_TenderLine.domesticValue = ROUND(DECI(REPLACE(hTenderAmount:GET-ATTRIBUTE ("domesticValue"),".",",")),2)
                             TT_TenderLine.amountValue   = dAmount.
                             
                      IF TT_TenderLine.paymentType = "card" AND AVAIL TT_Receipt THEN
                          ASSIGN TT_Receipt.cardamount = TT_Receipt.cardamount + dAmount.
                      ELSE IF AVAIL TT_Receipt AND TT_Receipt.terminalType = "opt" AND TT_TenderLine.paymentType = "cash" THEN
                          ASSIGN TT_Receipt.utecash = TRUE. /* om utecash skall vi avrunda kvittobelopp senare */
                    END.
                  END.
                END.
              END.
              WHEN "TenderLine" THEN DO: /* Vid in/outpayment hReceiptRows:NAME: samt currencyBuy/Sell*/
                REPEAT mm = 1 TO hReceiptRows:NUM-CHILDREN:
                  hReceiptRows:GET-CHILD (hTenderAmount,mm).
                  ASSIGN dAmount = 0.
                  IF hTenderAmount:NUM-CHILDREN = 1 THEN DO:
                      hTenderAmount:GET-CHILD (hTenderAmountValue,1).
                      ASSIGN dAmount = ROUND(DECI(REPLACE(hTenderAmountValue:NODE-VALUE,".",",")),2).
                  END.
                  ASSIGN cTL_Names = hReceiptRows:ATTRIBUTE-NAMES.
                  ASSIGN cTA_Names = hTenderAmount:ATTRIBUTE-NAMES.
                  CREATE TT_TenderLine.
                  ASSIGN iRowNr                     = iRowNr + 1
                         TT_TenderLine.ReceiptSeq   = TT_Receipt.ReceiptSeq
                         TT_TenderLine.Rownr        = iRowNr
                         TT_TenderLine.isExchange   = hReceiptRows:GET-ATTRIBUTE ("isExchange") = "true"
                         TT_TenderLine.paymentType  = hReceiptRows:GET-ATTRIBUTE ("paymentType")
                         TT_TenderLine.subPaymentId = IF CAN-DO(cTL_Names,"subPaymentId") THEN INT(hReceiptRows:GET-ATTRIBUTE ("subPaymentId")) ELSE ?
                         TT_TenderLine.currencyId   = IF CAN-DO(cTA_Names,"currencyId") THEN hTenderAmount:GET-ATTRIBUTE ("currencyId") ELSE ""
                         TT_TenderLine.domesticValue = ROUND(DECI(REPLACE(hTenderAmount:GET-ATTRIBUTE ("domesticValue"),".",",")),2)
                         TT_TenderLine.amountValue   = dAmount.
                  ASSIGN TT_Receipt.Amount = TT_Receipt.Amount + IF TT_Receipt.receiptType = "void" THEN 0 ELSE dAmount.
/*                                          ROUND(DECI(REPLACE(hTenderAmount:GET-ATTRIBUTE ("amountValue"),".",",")),2). */
                END.
              END.
              WHEN "Currency" THEN DO:
                  ASSIGN TT_Receipt.currency = IF CAN-DO(hReceiptRows:ATTRIBUTE-NAMES,"currencyId") THEN hReceiptRows:GET-ATTRIBUTE ("currencyId") ELSE ""
                         TT_Receipt.currencyAmount = IF CAN-DO(hReceiptRows:ATTRIBUTE-NAMES,"amount") THEN 
                                                          ROUND(DECI(REPLACE(hReceiptRows:GET-ATTRIBUTE ("amount"),".",",")),2) ELSE 0.
              END.
              WHEN "DropAmount" THEN DO: /* Vid Drop */
                IF hReceiptRows:NUM-CHILDREN = 1 THEN DO:
                  hReceiptRows:GET-CHILD (hTenderAmountValue,1).
                  CREATE TT_TenderLine.
                  ASSIGN iRowNr                     = iRowNr + 1
                         TT_TenderLine.ReceiptSeq   = TT_Receipt.ReceiptSeq
                         TT_TenderLine.Rownr        = iRowNr
                         TT_TenderLine.isExchange   = FALSE
                         TT_TenderLine.paymentType  = "cash"
                         TT_TenderLine.subPaymentId = ?
                         TT_TenderLine.currencyId   = IF CAN-DO(hReceiptRows:ATTRIBUTE-NAMES,"currencyId") THEN hReceiptRows:GET-ATTRIBUTE ("currencyId") ELSE ""
                         TT_TenderLine.domesticValue = IF CAN-DO(hReceiptRows:ATTRIBUTE-NAMES,"domesticValue") THEN 
                                                          ROUND(DECI(REPLACE(hReceiptRows:GET-ATTRIBUTE ("domesticValue"),".",",")),2) ELSE 0
                         TT_TenderLine.amountValue   = ROUND(DECI(REPLACE(hTenderAmountValue:NODE-VALUE,".",",")),2).
                END.
              END.
              WHEN "PaymentType" THEN DO: /* Vid in/outpayment */
                IF can-do(hReceiptRows:ATTRIBUTE-NAMES,"TargetId") THEN
                    ASSIGN TT_Receipt.ptypetargetId = INT(hReceiptRows:GET-ATTRIBUTE ("TargetId")) NO-ERROR.
              END.
              WHEN "TaxSummaryLine" THEN DO:
                CREATE TT_TaxSum.
                ASSIGN TT_TaxSum.ReceiptSeq = TT_Receipt.ReceiptSeq
                       TT_TaxSum.ReceiptNumber = TT_Receipt.ReceiptNumber
                       TT_TaxSum.taxClassId = INT(hReceiptRows:GET-ATTRIBUTE ("taxClassId"))
                       TT_TaxSum.taxAmount  = ROUND(DECI(REPLACE(hReceiptRows:GET-ATTRIBUTE ("taxAmount"),".",",")),2).
              END.
            END CASE.
          END.
          TT_Receipt.Amount = dReceiptAmount.
      END.
      ELSE IF CAN-DO("FCShiftChange,POSShiftChange,PeriodChange",hReceipt:NAME) THEN DO:
        hReceipt:GET-CHILD (hState,1).
        hState:GET-CHILD(hText2,1).
        IF CAN-DO("open,closed,terminated",hText2:NODE-VALUE) THEN DO:
            IF CAN-DO("FCShiftChange,POSShiftChange",hReceipt:NAME) AND CAN-DO("open,terminated",hText2:NODE-VALUE) THEN DO:
              ASSIGN dDato = getDato(hReceipt:GET-ATTRIBUTE ("date"))
                     iTime = getTid(hReceipt:GET-ATTRIBUTE ("time"))
                     dSkiftId = DECI(hReceipt:GET-ATTRIBUTE ("shiftId")).
              FIND FIRST TT_Shift WHERE TT_Shift.SkiftNr = dSkiftId NO-ERROR.
              IF NOT AVAIL TT_Shift THEN DO:
                  CREATE TT_Shift.
                  ASSIGN TT_Shift.Butikknr = iButikknr
                         TT_Shift.Dato = dDato
                         TT_Shift.SkiftNr = dSkiftId
                         TT_Shift.TermTid = iTime
                         TT_Shift.sequenceNumber = IF CAN-DO(hReceipt:ATTRIBUTE-NAMES,"sequenceNumber") THEN
                                           DECI(hReceipt:GET-ATTRIBUTE ("sequenceNumber")) ELSE TT_Shift.sequenceNumber.
              END.
              ASSIGN TT_Shift.OpenTid = IF hText2:NODE-VALUE = "open" THEN iTime ELSE TT_Shift.OpenTid
                     TT_Shift.TermTid = IF hText2:NODE-VALUE = "terminated" THEN iTime ELSE TT_Shift.TermTid
                     TT_Shift.SkiftNr = dSkiftId
                     TT_Shift.Terminert = IF hText2:NODE-VALUE = "terminated" THEN TRUE ELSE TT_Shift.Terminert
                     TT_Shift.TermDato = IF TT_Shift.Terminert THEN dDato ELSE TT_Shift.TermDato
                     TT_Shift.TermDatoTid = IF TT_Shift.Terminert THEN DECI(REPLACE(hReceipt:GET-ATTRIBUTE ("date"),"-","") + "00000") + iTime ELSE TT_Shift.TermDatoTid.
            END.
            ELSE IF hReceipt:NAME = "PeriodChange" THEN DO: /* PeriodChange */
              IF hText2:NODE-VALUE = "closed" THEN DO: /* för tillfället hanterar vi bara closed i.e vi skapar dagsavslut här */
                ASSIGN dDato = getDato(hReceipt:GET-ATTRIBUTE ("date"))
                       iTime = getTid(hReceipt:GET-ATTRIBUTE ("time")).
                FIND TT_Period WHERE TT_Period.ButikkNr = iButikkNr AND TT_Period.Dato     = dDato NO-ERROR.
                IF NOT AVAIL TT_Period THEN DO:
                  CREATE TT_Period.
                  ASSIGN TT_Period.ButikkNr = iButikkNr
                         TT_Period.Dato     = dDato
                         TT_Period.TermTid  = iTime
                         TT_Period.TermDatoTid = DECI(REPLACE(hReceipt:GET-ATTRIBUTE ("date"),"-","") + "00000") + iTime.
                END.
                ELSE
                  ASSIGN TT_Period.TermTid  = iTime
                         TT_Period.TermDatoTid = DECI(REPLACE(hReceipt:GET-ATTRIBUTE ("date"),"-","") + "00000") + iTime.
              END.
            END.
        END.
      END.
    END.
END.
/* Delete the objects we created. */
DELETE OBJECT hDoc.
DELETE OBJECT hRoot.
DELETE OBJECT hTable.
DELETE OBJECT hPosEvent.
DELETE OBJECT hReceipt.
DELETE OBJECT hReceiptRows.
DELETE OBJECT hItemLine.
DELETE OBJECT hItemLineValues.
DELETE OBJECT hTenderLine.
DELETE OBJECT hTenderAmount.
DELETE OBJECT hTenderAmountValue.
DELETE OBJECT hField2.
DELETE OBJECT hText2.
DELETE OBJECT hState.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-getDato) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDato Procedure 
FUNCTION getDato RETURNS DATE
  ( INPUT cYYYYMMDD AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes: cYYYYMMDD är separerad med "-" 
------------------------------------------------------------------------------*/
  RETURN DATE(INT(ENTRY(2,cYYYYMMDD,"-")),INT(ENTRY(3,cYYYYMMDD,"-")),INT(ENTRY(1,cYYYYMMDD,"-"))).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTid) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTid Procedure 
FUNCTION getTid RETURNS INTEGER
  ( INPUT cHHMMSS AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes: cHHMMSS är separerad med ":" (kolon) 
------------------------------------------------------------------------------*/
  RETURN INT(ENTRY(1,cHHMMSS,":")) * 3600 + INT(ENTRY(2,cHHMMSS,":")) * 60 + INT(ENTRY(3,cHHMMSS,":")).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

