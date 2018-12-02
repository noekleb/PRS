&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          data             PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE CRECEIPT_NO LIKE joweRECEIPT.RECEIPT_NO  NO-UNDO.
DEF VAR piModulo AS INT INITIAL 1000 NO-UNDO.

DEF STREAM ErrLogg.

{jowereceipt-pbntmegapb.i}
{jowereceipt-pbntkvtemp.i}
{jowereceipt-s10001.i}

{jowerec_row-pbntmegapb.i}
{jowerec_row-pbntxlent.i}
{jowerec_row-s10001.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-RECEIPT

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES joweRECEIPT joweREC_PAY joweREC_ROW

/* Definitions for BROWSE BROWSE-RECEIPT                                */
&Scoped-define FIELDS-IN-QUERY-BROWSE-RECEIPT joweRECEIPT.RECEIPT_NO ~
joweRECEIPT.ACT_DATE joweRECEIPT.ACT_TIME joweRECEIPT.ROWSUM ~
joweRECEIPT.VAT joweRECEIPT.DISCOUNT joweRECEIPT.DISCOUNT_P ~
joweRECEIPT.ROUND_OFF joweRECEIPT.INSERTED joweRECEIPT.ACT_NO ~
joweRECEIPT.LogicDate joweRECEIPT.OUR_ID joweRECEIPT.OUR_NO ~
joweRECEIPT.INDIVID_NO joweRECEIPT.INDIVID_ID joweRECEIPT.NAME ~
joweRECEIPT.SALESP_ID joweRECEIPT.USER_ID joweRECEIPT.EME_NO ~
joweRECEIPT.ACCOUNT_2 joweRECEIPT.PROJECT_ID joweRECEIPT.NOTES ~
joweRECEIPT.UPDATED joweRECEIPT.I_USER_NO joweRECEIPT.USER_NO ~
joweRECEIPT.CAMP_NO joweRECEIPT.CMP_ACT_NO joweRECEIPT.IS_INTERN ~
joweRECEIPT.IS_DONE joweRECEIPT.STOCKVALUE joweRECEIPT.FREIGHT ~
joweRECEIPT.INV_FEE joweRECEIPT.TOTAL joweRECEIPT.CURR_RATE ~
joweRECEIPT.CURRENCYID joweRECEIPT.IS_APPROVE joweRECEIPT.IS_SENT ~
joweRECEIPT.BUYER_TYPE joweRECEIPT.BALANCE_NO joweRECEIPT.TillType ~
joweRECEIPT.TillUnitId joweRECEIPT.S_CHANGE joweRECEIPT.PRINT_SAP ~
joweRECEIPT.ORIGIN_NO joweRECEIPT.OFFLINE_NO joweRECEIPT.CUSTGRP_ID ~
joweRECEIPT.SHOP_ID joweRECEIPT.CASHREG_NO joweRECEIPT.SIGNATURE ~
joweRECEIPT.SUPP_PRICE joweRECEIPT.CARTYPEID joweRECEIPT.CREDCARDID ~
joweRECEIPT.LEGETIMAT joweRECEIPT.THEIR_ID joweRECEIPT.EME_NAME ~
joweRECEIPT.IS_STAT joweRECEIPT.IS_REPORT joweRECEIPT.IS_STAFF ~
joweRECEIPT.IS_LOCSOLD joweRECEIPT.IS_PRINTED joweRECEIPT.IS_EXPORT 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-RECEIPT 
&Scoped-define OPEN-QUERY-BROWSE-RECEIPT OPEN QUERY BROWSE-RECEIPT FOR EACH joweRECEIPT NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-RECEIPT joweRECEIPT
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-RECEIPT joweRECEIPT


/* Definitions for BROWSE BROWSE-REC_PAY                                */
&Scoped-define FIELDS-IN-QUERY-BROWSE-REC_PAY joweREC_PAY.RECEIPT_NO ~
joweREC_PAY.PAYTYPE_ID joweREC_PAY.PAYMENT_ID joweREC_PAY.RATE_OUT ~
joweREC_PAY.AMOUNT joweREC_PAY.AMOUNT_ORG joweREC_PAY.PAIDAMOUNT ~
joweREC_PAY.AMOUNT_RET 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-REC_PAY 
&Scoped-define OPEN-QUERY-BROWSE-REC_PAY OPEN QUERY BROWSE-REC_PAY FOR EACH joweREC_PAY NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-REC_PAY joweREC_PAY
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-REC_PAY joweREC_PAY


/* Definitions for BROWSE BROWSE-REC_ROW                                */
&Scoped-define FIELDS-IN-QUERY-BROWSE-REC_ROW joweREC_ROW.RECEIPT_NO ~
joweREC_ROW.INSERTED joweREC_ROW.ROW_NO joweREC_ROW.SEQNO ~
joweREC_ROW.PRODUCT_NO joweREC_ROW.UNIT_ID joweREC_ROW.PriceTypeId ~
joweREC_ROW.QUANTITY joweREC_ROW.STK_CONVF joweREC_ROW.SUPP_PRICE ~
joweREC_ROW.UNIT_PRICE joweREC_ROW.AMOUNT joweREC_ROW.DISCOUNT_P ~
joweREC_ROW.DISCOUNT joweREC_ROW.VAT_P joweREC_ROW.VAT ~
joweREC_ROW.CONTRACTNO joweREC_ROW.GROUP_NO joweREC_ROW.ACCOUNT_4 ~
joweREC_ROW.ACCOUNT_5 joweREC_ROW.IS_MAIN_ROW joweREC_ROW.IS_TEXT ~
joweREC_ROW.IS_MANDISC joweREC_ROW.IS_DISC_P joweREC_ROW.IS_N_PRICE ~
joweREC_ROW.PRODUCT_ID joweREC_ROW.DESCRIPT joweREC_ROW.PRODGR_ID ~
joweREC_ROW.ORIGIN_ID joweREC_ROW.PROD_CLASS joweREC_ROW.DISC_CUST ~
joweREC_ROW.DISC_EMPL joweREC_ROW.DISC_ACT joweREC_ROW.CRED_QTY ~
joweREC_ROW.CUSTORD_NO joweREC_ROW.CORDROW_NO joweREC_ROW.REPORT_CODE ~
joweREC_ROW.STAT_CODE joweREC_ROW.IS_PRESOLD joweREC_ROW.IsTillUnitStock ~
joweREC_ROW.OUR_NO joweREC_ROW.UnitPriceWoAct joweREC_ROW.DiscTypeId 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-REC_ROW 
&Scoped-define OPEN-QUERY-BROWSE-REC_ROW OPEN QUERY BROWSE-REC_ROW FOR EACH joweREC_ROW NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-REC_ROW joweREC_ROW
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-REC_ROW joweREC_ROW


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BROWSE-RECEIPT}~
    ~{&OPEN-QUERY-BROWSE-REC_PAY}~
    ~{&OPEN-QUERY-BROWSE-REC_ROW}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BUTTON-5 BUTTON-6 BUTTON-7 CB-Receipt ~
BROWSE-RECEIPT BUTTON-1 BUTTON-2 BUTTON-3 CB-Rec-Row BROWSE-REC_ROW ~
BUTTON-9 BUTTON-10 BUTTON-11 BROWSE-REC_PAY BUTTON-12 BUTTON-13 ~
BUTTON-EXPORT B-Katalog 
&Scoped-Define DISPLAYED-OBJECTS FI-ReceiptFil CB-Receipt CB-Rec-Row ~
FI-RecRowFil FI-RecPayFil FI-Katalog 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Katalog 
     LABEL "..." 
     SIZE 4.6 BY 1.1.

DEFINE BUTTON BUTTON-1 
     LABEL "Fil" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-10 
     LABEL "Läs in" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-11 
     LABEL "Ta bort" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-12 
     LABEL "Läs in alle filer" 
     SIZE 21 BY 1.14.

DEFINE BUTTON BUTTON-13 
     LABEL "Ta bort alle" 
     SIZE 21 BY 1.14.

DEFINE BUTTON BUTTON-2 
     LABEL "Läs in" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-3 
     LABEL "Ta bort" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-5 
     LABEL "Fil" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-6 
     LABEL "Läs in" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-7 
     LABEL "Ta bort" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-9 
     LABEL "Fil" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-EXPORT 
     LABEL "EXPORTERA" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE CB-Rec-Row AS INTEGER FORMAT "9":U INITIAL 1 
     VIEW-AS COMBO-BOX INNER-LINES 20
     LIST-ITEM-PAIRS "PBNTMEGAPB",1,
                     "PBNTXLENTPB",2,
                     "S10001",3
     DROP-DOWN-LIST
     SIZE 28 BY 1 NO-UNDO.

DEFINE VARIABLE CB-Receipt AS INTEGER FORMAT "9":U INITIAL 1 
     VIEW-AS COMBO-BOX INNER-LINES 20
     LIST-ITEM-PAIRS "PBNTMEGAPB",1,
                     "PBNTKVTEMP",2,
                     "S10001",3
     DROP-DOWN-LIST
     SIZE 28 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Katalog AS CHARACTER FORMAT "X(256)":U 
     LABEL "Rootdir export" 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-ReceiptFil AS CHARACTER FORMAT "X(256)":U INITIAL "C:~\appdir~\SkoTex~\kom~\IN~\receipt.dat" 
     LABEL "RECEIPT-fil" 
     VIEW-AS FILL-IN 
     SIZE 41 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-RecPayFil AS CHARACTER FORMAT "X(256)":U INITIAL "C:~\appdir~\SkoTex~\kom~\IN~\rec_pay.dat" 
     LABEL "REC_PAY-fil" 
     VIEW-AS FILL-IN 
     SIZE 41 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-RecRowFil AS CHARACTER FORMAT "X(256)":U INITIAL "C:~\appdir~\SkoTex~\kom~\IN~\rec_row.dat" 
     LABEL "REC_ROW-fil" 
     VIEW-AS FILL-IN 
     SIZE 41 BY 1
     BGCOLOR 15  NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-RECEIPT FOR 
      joweRECEIPT SCROLLING.

DEFINE QUERY BROWSE-REC_PAY FOR 
      joweREC_PAY SCROLLING.

DEFINE QUERY BROWSE-REC_ROW FOR 
      joweREC_ROW SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-RECEIPT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-RECEIPT C-Win _STRUCTURED
  QUERY BROWSE-RECEIPT NO-LOCK DISPLAY
      joweRECEIPT.RECEIPT_NO FORMAT "X(14)":U WIDTH 17.4
      joweRECEIPT.ACT_DATE FORMAT "99/99/99":U
      joweRECEIPT.ACT_TIME FORMAT "X(5)":U
      joweRECEIPT.ROWSUM FORMAT "->>>>>>>>>9.9999":U
      joweRECEIPT.VAT FORMAT "->>>>>>>>>9.9999":U
      joweRECEIPT.DISCOUNT FORMAT "->>>>>>>>>9.99":U
      joweRECEIPT.DISCOUNT_P FORMAT "->>>>>>9.999":U
      joweRECEIPT.ROUND_OFF FORMAT "->>>>>>>9.9999":U
      joweRECEIPT.INSERTED FORMAT "99/99/99":U
      joweRECEIPT.ACT_NO FORMAT "X(14)":U
      joweRECEIPT.LogicDate FORMAT "99/99/99":U
      joweRECEIPT.OUR_ID FORMAT "X(15)":U
      joweRECEIPT.OUR_NO FORMAT "X(14)":U
      joweRECEIPT.INDIVID_NO FORMAT "X(14)":U
      joweRECEIPT.INDIVID_ID FORMAT "X(15)":U
      joweRECEIPT.NAME FORMAT "X(36)":U
      joweRECEIPT.SALESP_ID FORMAT "X(15)":U
      joweRECEIPT.USER_ID FORMAT "X(15)":U
      joweRECEIPT.EME_NO FORMAT "X(14)":U
      joweRECEIPT.ACCOUNT_2 FORMAT "X(12)":U
      joweRECEIPT.PROJECT_ID FORMAT "X(12)":U
      joweRECEIPT.NOTES FORMAT "X(40)":U
      joweRECEIPT.UPDATED FORMAT "99/99/99":U
      joweRECEIPT.I_USER_NO FORMAT "X(14)":U
      joweRECEIPT.USER_NO FORMAT "X(14)":U
      joweRECEIPT.CAMP_NO FORMAT "X(14)":U
      joweRECEIPT.CMP_ACT_NO FORMAT "X(14)":U
      joweRECEIPT.IS_INTERN FORMAT "X":U
      joweRECEIPT.IS_DONE FORMAT "X":U
      joweRECEIPT.STOCKVALUE FORMAT "->>>>>>>9.99":U
      joweRECEIPT.FREIGHT FORMAT "->>>>>>>9.99":U
      joweRECEIPT.INV_FEE FORMAT "->>>>>>>9.99":U
      joweRECEIPT.TOTAL FORMAT "->>>>>>>>>9.99":U
      joweRECEIPT.CURR_RATE FORMAT "->>>9.999999":U
      joweRECEIPT.CURRENCYID FORMAT "X(3)":U
      joweRECEIPT.IS_APPROVE FORMAT "X":U
      joweRECEIPT.IS_SENT FORMAT "X":U
      joweRECEIPT.BUYER_TYPE FORMAT "X":U
      joweRECEIPT.BALANCE_NO FORMAT "X(14)":U
      joweRECEIPT.TillType FORMAT "X(2)":U
      joweRECEIPT.TillUnitId FORMAT "X(15)":U
      joweRECEIPT.S_CHANGE FORMAT "->>>>>>>>>9.99":U
      joweRECEIPT.PRINT_SAP FORMAT "X(20)":U
      joweRECEIPT.ORIGIN_NO FORMAT "X(14)":U
      joweRECEIPT.OFFLINE_NO FORMAT "X(40)":U
      joweRECEIPT.CUSTGRP_ID FORMAT "X(12)":U
      joweRECEIPT.SHOP_ID FORMAT "X(15)":U
      joweRECEIPT.CASHREG_NO FORMAT "->>>>>>9":U
      joweRECEIPT.SIGNATURE FORMAT "X(6)":U
      joweRECEIPT.SUPP_PRICE FORMAT "->>>>>>>>>9.99":U
      joweRECEIPT.CARTYPEID FORMAT "X(12)":U
      joweRECEIPT.CREDCARDID FORMAT "X(16)":U
      joweRECEIPT.LEGETIMAT FORMAT "X(12)":U
      joweRECEIPT.THEIR_ID FORMAT "X(15)":U
      joweRECEIPT.EME_NAME FORMAT "X(30)":U
      joweRECEIPT.IS_STAT FORMAT "X":U
      joweRECEIPT.IS_REPORT FORMAT "X":U
      joweRECEIPT.IS_STAFF FORMAT "X":U
      joweRECEIPT.IS_LOCSOLD FORMAT "X":U
      joweRECEIPT.IS_PRINTED FORMAT "X":U
      joweRECEIPT.IS_EXPORT FORMAT "X":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 133 BY 4.29 EXPANDABLE.

DEFINE BROWSE BROWSE-REC_PAY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-REC_PAY C-Win _STRUCTURED
  QUERY BROWSE-REC_PAY NO-LOCK DISPLAY
      joweREC_PAY.RECEIPT_NO FORMAT "X(14)":U WIDTH 17.4
      joweREC_PAY.PAYTYPE_ID FORMAT "X(12)":U
      joweREC_PAY.PAYMENT_ID FORMAT "X(22)":U
      joweREC_PAY.RATE_OUT FORMAT "->>>9.999999":U
      joweREC_PAY.AMOUNT FORMAT "->>>>>>>9.99":U
      joweREC_PAY.AMOUNT_ORG FORMAT "->>>>>>>9.99":U
      joweREC_PAY.PAIDAMOUNT FORMAT "->>>>>>>9.99":U
      joweREC_PAY.AMOUNT_RET FORMAT "->>>>>>>9.99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 99 BY 6.67 EXPANDABLE.

DEFINE BROWSE BROWSE-REC_ROW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-REC_ROW C-Win _STRUCTURED
  QUERY BROWSE-REC_ROW NO-LOCK DISPLAY
      joweREC_ROW.RECEIPT_NO FORMAT "X(14)":U WIDTH 17.4
      joweREC_ROW.INSERTED FORMAT "99/99/99":U
      joweREC_ROW.ROW_NO FORMAT ">>>9":U
      joweREC_ROW.SEQNO FORMAT "X(14)":U
      joweREC_ROW.PRODUCT_NO FORMAT "X(14)":U
      joweREC_ROW.UNIT_ID FORMAT "X(12)":U
      joweREC_ROW.PriceTypeId FORMAT "X(12)":U
      joweREC_ROW.QUANTITY FORMAT "->>>>>9.999":U
      joweREC_ROW.STK_CONVF FORMAT "->>>9.9999":U
      joweREC_ROW.SUPP_PRICE FORMAT "->>>>>>>9.99":U
      joweREC_ROW.UNIT_PRICE FORMAT "->>>>>>>9.99":U
      joweREC_ROW.AMOUNT FORMAT "->>>>>>>>>9.9999":U
      joweREC_ROW.DISCOUNT_P FORMAT "->>>9.99":U
      joweREC_ROW.DISCOUNT FORMAT "->>>>>>>>>9.9999":U
      joweREC_ROW.VAT_P FORMAT "->9.9999":U
      joweREC_ROW.VAT FORMAT "->>>>>>>>>9.9999":U
      joweREC_ROW.CONTRACTNO FORMAT "X(18)":U
      joweREC_ROW.GROUP_NO FORMAT "X(14)":U
      joweREC_ROW.ACCOUNT_4 FORMAT "X(6)":U
      joweREC_ROW.ACCOUNT_5 FORMAT "X(6)":U
      joweREC_ROW.IS_MAIN_ROW FORMAT "X":U
      joweREC_ROW.IS_TEXT FORMAT "X":U
      joweREC_ROW.IS_MANDISC FORMAT "X":U
      joweREC_ROW.IS_DISC_P FORMAT "X":U
      joweREC_ROW.IS_N_PRICE FORMAT "X":U
      joweREC_ROW.PRODUCT_ID FORMAT "X(20)":U
      joweREC_ROW.DESCRIPT FORMAT "X(73)":U
      joweREC_ROW.PRODGR_ID FORMAT "X(12)":U
      joweREC_ROW.ORIGIN_ID FORMAT "X":U
      joweREC_ROW.PROD_CLASS FORMAT "X(20)":U
      joweREC_ROW.DISC_CUST FORMAT "->>>>>>>>>9.9999":U
      joweREC_ROW.DISC_EMPL FORMAT "->>>>>>>>>9.9999":U
      joweREC_ROW.DISC_ACT FORMAT "->>>>>>>>>9.9999":U
      joweREC_ROW.CRED_QTY FORMAT "->>>>>9":U
      joweREC_ROW.CUSTORD_NO FORMAT "X(14)":U
      joweREC_ROW.CORDROW_NO FORMAT "X(14)":U
      joweREC_ROW.REPORT_CODE FORMAT "X":U
      joweREC_ROW.STAT_CODE FORMAT "X":U
      joweREC_ROW.IS_PRESOLD FORMAT "X":U
      joweREC_ROW.IsTillUnitStock FORMAT "X":U
      joweREC_ROW.OUR_NO FORMAT "X(14)":U
      joweREC_ROW.UnitPriceWoAct FORMAT "->>>>>>>9.99":U
      joweREC_ROW.DiscTypeId FORMAT "X(12)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 133 BY 5 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     FI-ReceiptFil AT ROW 1.71 COL 18 COLON-ALIGNED
     BUTTON-5 AT ROW 1.71 COL 64
     BUTTON-6 AT ROW 1.71 COL 80.2
     BUTTON-7 AT ROW 1.71 COL 96
     CB-Receipt AT ROW 1.71 COL 110 COLON-ALIGNED NO-LABEL
     BROWSE-RECEIPT AT ROW 3.14 COL 7
     BUTTON-1 AT ROW 7.67 COL 65
     BUTTON-2 AT ROW 7.67 COL 81.2
     BUTTON-3 AT ROW 7.67 COL 97
     CB-Rec-Row AT ROW 7.67 COL 111 COLON-ALIGNED NO-LABEL
     FI-RecRowFil AT ROW 7.71 COL 20 COLON-ALIGNED
     BROWSE-REC_ROW AT ROW 9.33 COL 7
     BUTTON-9 AT ROW 14.57 COL 63
     BUTTON-10 AT ROW 14.57 COL 79.2
     BUTTON-11 AT ROW 14.57 COL 95
     FI-RecPayFil AT ROW 14.62 COL 18 COLON-ALIGNED
     BROWSE-REC_PAY AT ROW 16 COL 6
     BUTTON-12 AT ROW 16.24 COL 113
     BUTTON-13 AT ROW 17.43 COL 113
     BUTTON-EXPORT AT ROW 22.91 COL 67.2
     B-Katalog AT ROW 22.95 COL 61
     FI-Katalog AT ROW 23.05 COL 19 COLON-ALIGNED
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 141 BY 23.14.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert window title>"
         HEIGHT             = 23.14
         WIDTH              = 141
         MAX-HEIGHT         = 34.86
         MAX-WIDTH          = 141
         VIRTUAL-HEIGHT     = 34.86
         VIRTUAL-WIDTH      = 141
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
/* BROWSE-TAB BROWSE-RECEIPT CB-Receipt DEFAULT-FRAME */
/* BROWSE-TAB BROWSE-REC_ROW FI-RecRowFil DEFAULT-FRAME */
/* BROWSE-TAB BROWSE-REC_PAY FI-RecPayFil DEFAULT-FRAME */
ASSIGN 
       BROWSE-RECEIPT:COLUMN-RESIZABLE IN FRAME DEFAULT-FRAME       = TRUE
       BROWSE-RECEIPT:COLUMN-MOVABLE IN FRAME DEFAULT-FRAME         = TRUE.

ASSIGN 
       BROWSE-REC_PAY:COLUMN-RESIZABLE IN FRAME DEFAULT-FRAME       = TRUE
       BROWSE-REC_PAY:COLUMN-MOVABLE IN FRAME DEFAULT-FRAME         = TRUE.

ASSIGN 
       BROWSE-REC_ROW:COLUMN-RESIZABLE IN FRAME DEFAULT-FRAME       = TRUE
       BROWSE-REC_ROW:COLUMN-MOVABLE IN FRAME DEFAULT-FRAME         = TRUE.

/* SETTINGS FOR FILL-IN FI-Katalog IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-ReceiptFil IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-RecPayFil IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-RecRowFil IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-RECEIPT
/* Query rebuild information for BROWSE BROWSE-RECEIPT
     _TblList          = "data.joweRECEIPT"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > data.joweRECEIPT.RECEIPT_NO
"joweRECEIPT.RECEIPT_NO" ? ? "character" ? ? ? ? ? ? no ? no no "17.4" yes no no "U" "" ""
     _FldNameList[2]   = data.joweRECEIPT.ACT_DATE
     _FldNameList[3]   = data.joweRECEIPT.ACT_TIME
     _FldNameList[4]   = data.joweRECEIPT.ROWSUM
     _FldNameList[5]   = data.joweRECEIPT.VAT
     _FldNameList[6]   = data.joweRECEIPT.DISCOUNT
     _FldNameList[7]   = data.joweRECEIPT.DISCOUNT_P
     _FldNameList[8]   = data.joweRECEIPT.ROUND_OFF
     _FldNameList[9]   = data.joweRECEIPT.INSERTED
     _FldNameList[10]   = data.joweRECEIPT.ACT_NO
     _FldNameList[11]   = data.joweRECEIPT.LogicDate
     _FldNameList[12]   = data.joweRECEIPT.OUR_ID
     _FldNameList[13]   = data.joweRECEIPT.OUR_NO
     _FldNameList[14]   = data.joweRECEIPT.INDIVID_NO
     _FldNameList[15]   = data.joweRECEIPT.INDIVID_ID
     _FldNameList[16]   = data.joweRECEIPT.NAME
     _FldNameList[17]   = data.joweRECEIPT.SALESP_ID
     _FldNameList[18]   = data.joweRECEIPT.USER_ID
     _FldNameList[19]   = data.joweRECEIPT.EME_NO
     _FldNameList[20]   = data.joweRECEIPT.ACCOUNT_2
     _FldNameList[21]   = data.joweRECEIPT.PROJECT_ID
     _FldNameList[22]   = data.joweRECEIPT.NOTES
     _FldNameList[23]   = data.joweRECEIPT.UPDATED
     _FldNameList[24]   = data.joweRECEIPT.I_USER_NO
     _FldNameList[25]   = data.joweRECEIPT.USER_NO
     _FldNameList[26]   = data.joweRECEIPT.CAMP_NO
     _FldNameList[27]   = data.joweRECEIPT.CMP_ACT_NO
     _FldNameList[28]   = data.joweRECEIPT.IS_INTERN
     _FldNameList[29]   = data.joweRECEIPT.IS_DONE
     _FldNameList[30]   = data.joweRECEIPT.STOCKVALUE
     _FldNameList[31]   = data.joweRECEIPT.FREIGHT
     _FldNameList[32]   = data.joweRECEIPT.INV_FEE
     _FldNameList[33]   = data.joweRECEIPT.TOTAL
     _FldNameList[34]   = data.joweRECEIPT.CURR_RATE
     _FldNameList[35]   = data.joweRECEIPT.CURRENCYID
     _FldNameList[36]   = data.joweRECEIPT.IS_APPROVE
     _FldNameList[37]   = data.joweRECEIPT.IS_SENT
     _FldNameList[38]   = data.joweRECEIPT.BUYER_TYPE
     _FldNameList[39]   = data.joweRECEIPT.BALANCE_NO
     _FldNameList[40]   = data.joweRECEIPT.TillType
     _FldNameList[41]   = data.joweRECEIPT.TillUnitId
     _FldNameList[42]   = data.joweRECEIPT.S_CHANGE
     _FldNameList[43]   = data.joweRECEIPT.PRINT_SAP
     _FldNameList[44]   = data.joweRECEIPT.ORIGIN_NO
     _FldNameList[45]   = data.joweRECEIPT.OFFLINE_NO
     _FldNameList[46]   = data.joweRECEIPT.CUSTGRP_ID
     _FldNameList[47]   = data.joweRECEIPT.SHOP_ID
     _FldNameList[48]   = data.joweRECEIPT.CASHREG_NO
     _FldNameList[49]   = data.joweRECEIPT.SIGNATURE
     _FldNameList[50]   = data.joweRECEIPT.SUPP_PRICE
     _FldNameList[51]   = data.joweRECEIPT.CARTYPEID
     _FldNameList[52]   = data.joweRECEIPT.CREDCARDID
     _FldNameList[53]   = data.joweRECEIPT.LEGETIMAT
     _FldNameList[54]   = data.joweRECEIPT.THEIR_ID
     _FldNameList[55]   = data.joweRECEIPT.EME_NAME
     _FldNameList[56]   = data.joweRECEIPT.IS_STAT
     _FldNameList[57]   = data.joweRECEIPT.IS_REPORT
     _FldNameList[58]   = data.joweRECEIPT.IS_STAFF
     _FldNameList[59]   = data.joweRECEIPT.IS_LOCSOLD
     _FldNameList[60]   = data.joweRECEIPT.IS_PRINTED
     _FldNameList[61]   = data.joweRECEIPT.IS_EXPORT
     _Query            is OPENED
*/  /* BROWSE BROWSE-RECEIPT */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-REC_PAY
/* Query rebuild information for BROWSE BROWSE-REC_PAY
     _TblList          = "data.joweREC_PAY"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > data.joweREC_PAY.RECEIPT_NO
"joweREC_PAY.RECEIPT_NO" ? ? "character" ? ? ? ? ? ? no ? no no "17.4" yes no no "U" "" ""
     _FldNameList[2]   = data.joweREC_PAY.PAYTYPE_ID
     _FldNameList[3]   = data.joweREC_PAY.PAYMENT_ID
     _FldNameList[4]   = data.joweREC_PAY.RATE_OUT
     _FldNameList[5]   = data.joweREC_PAY.AMOUNT
     _FldNameList[6]   = data.joweREC_PAY.AMOUNT_ORG
     _FldNameList[7]   = data.joweREC_PAY.PAIDAMOUNT
     _FldNameList[8]   = data.joweREC_PAY.AMOUNT_RET
     _Query            is OPENED
*/  /* BROWSE BROWSE-REC_PAY */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-REC_ROW
/* Query rebuild information for BROWSE BROWSE-REC_ROW
     _TblList          = "data.joweREC_ROW"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > data.joweREC_ROW.RECEIPT_NO
"joweREC_ROW.RECEIPT_NO" ? ? "character" ? ? ? ? ? ? no ? no no "17.4" yes no no "U" "" ""
     _FldNameList[2]   = data.joweREC_ROW.INSERTED
     _FldNameList[3]   = data.joweREC_ROW.ROW_NO
     _FldNameList[4]   = data.joweREC_ROW.SEQNO
     _FldNameList[5]   = data.joweREC_ROW.PRODUCT_NO
     _FldNameList[6]   = data.joweREC_ROW.UNIT_ID
     _FldNameList[7]   = data.joweREC_ROW.PriceTypeId
     _FldNameList[8]   = data.joweREC_ROW.QUANTITY
     _FldNameList[9]   = data.joweREC_ROW.STK_CONVF
     _FldNameList[10]   = data.joweREC_ROW.SUPP_PRICE
     _FldNameList[11]   = data.joweREC_ROW.UNIT_PRICE
     _FldNameList[12]   = data.joweREC_ROW.AMOUNT
     _FldNameList[13]   = data.joweREC_ROW.DISCOUNT_P
     _FldNameList[14]   = data.joweREC_ROW.DISCOUNT
     _FldNameList[15]   = data.joweREC_ROW.VAT_P
     _FldNameList[16]   = data.joweREC_ROW.VAT
     _FldNameList[17]   = data.joweREC_ROW.CONTRACTNO
     _FldNameList[18]   = data.joweREC_ROW.GROUP_NO
     _FldNameList[19]   = data.joweREC_ROW.ACCOUNT_4
     _FldNameList[20]   = data.joweREC_ROW.ACCOUNT_5
     _FldNameList[21]   = data.joweREC_ROW.IS_MAIN_ROW
     _FldNameList[22]   = data.joweREC_ROW.IS_TEXT
     _FldNameList[23]   = data.joweREC_ROW.IS_MANDISC
     _FldNameList[24]   = data.joweREC_ROW.IS_DISC_P
     _FldNameList[25]   = data.joweREC_ROW.IS_N_PRICE
     _FldNameList[26]   = data.joweREC_ROW.PRODUCT_ID
     _FldNameList[27]   = data.joweREC_ROW.DESCRIPT
     _FldNameList[28]   = data.joweREC_ROW.PRODGR_ID
     _FldNameList[29]   = data.joweREC_ROW.ORIGIN_ID
     _FldNameList[30]   = data.joweREC_ROW.PROD_CLASS
     _FldNameList[31]   = data.joweREC_ROW.DISC_CUST
     _FldNameList[32]   = data.joweREC_ROW.DISC_EMPL
     _FldNameList[33]   = data.joweREC_ROW.DISC_ACT
     _FldNameList[34]   = data.joweREC_ROW.CRED_QTY
     _FldNameList[35]   = data.joweREC_ROW.CUSTORD_NO
     _FldNameList[36]   = data.joweREC_ROW.CORDROW_NO
     _FldNameList[37]   = data.joweREC_ROW.REPORT_CODE
     _FldNameList[38]   = data.joweREC_ROW.STAT_CODE
     _FldNameList[39]   = data.joweREC_ROW.IS_PRESOLD
     _FldNameList[40]   = data.joweREC_ROW.IsTillUnitStock
     _FldNameList[41]   = data.joweREC_ROW.OUR_NO
     _FldNameList[42]   = data.joweREC_ROW.UnitPriceWoAct
     _FldNameList[43]   = data.joweREC_ROW.DiscTypeId
     _Query            is OPENED
*/  /* BROWSE BROWSE-REC_ROW */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <insert window title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Katalog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Katalog C-Win
ON CHOOSE OF B-Katalog IN FRAME DEFAULT-FRAME /* ... */
DO:
/* a test/demo program */
 DEFINE VARIABLE lCanceled AS LOGICAL    NO-UNDO.
 ASSIGN FI-Katalog.
 RUN BrowseForFolder.p ("Velg katalog hvor importfilene ligger",
                       OUTPUT FI-Katalog, 
                       OUTPUT lCanceled).
 IF lCanceled = TRUE THEN
    RETURN NO-APPLY.
 ASSIGN
/*     FI-Katalog              = cFolder */
    FI-Katalog:SCREEN-VALUE = FI-Katalog
    BUTTON-EXPORT:SENSITIVE       = TRUE
    .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 C-Win
ON CHOOSE OF BUTTON-1 IN FRAME DEFAULT-FRAME /* Fil */
DO:
    DEFINE VARIABLE OKpressed AS LOGICAL    NO-UNDO.
    ASSIGN BUTTON-2:SENSITIVE = FALSE.
    SYSTEM-DIALOG GET-FILE FI-RecRowFil
          TITLE      "Välj REC_ROW fil ..."
          FILTERS    "Datafil (*.dat)"   "*.dat"
          MUST-EXIST
          USE-FILENAME
          RETURN-TO-START-DIR
          UPDATE OKpressed.
      IF OKpressed = TRUE THEN DO:
          ASSIGN FI-RecRowFil:SCREEN-VALUE = FI-RecRowFil
                 BUTTON-2:SENSITIVE = TRUE.
      END.
      RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-10
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-10 C-Win
ON CHOOSE OF BUTTON-10 IN FRAME DEFAULT-FRAME /* Läs in */
DO:
    RUN LesInnRec_Pay (FI-RecPayFil:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
    {&OPEN-QUERY-BROWSE-REC_PAY}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-11
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-11 C-Win
ON CHOOSE OF BUTTON-11 IN FRAME DEFAULT-FRAME /* Ta bort */
DO:
    FOR EACH joweREC_PAY. 
        DELETE joweREC_PAY.
    END.
    {&OPEN-QUERY-BROWSE-REC_PAY}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-12
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-12 C-Win
ON CHOOSE OF BUTTON-12 IN FRAME DEFAULT-FRAME /* Läs in alle filer */
DO:
    STATUS DEFAULT "Leser inn RECEIPT".
    RUN LesInnReceipt (FI-ReceiptFil:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
    {&OPEN-QUERY-BROWSE-RECEIPT}
    STATUS DEFAULT "Leser inn REC_ROW".
    RUN LesInnRec_Row (FI-RecRowFil:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
    {&OPEN-QUERY-BROWSE-REC_ROW}
    STATUS DEFAULT "Leser inn REC_PAY".
    RUN LesInnRec_Pay (FI-RecPayFil:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
    {&OPEN-QUERY-BROWSE-REC_PAY}
    STATUS DEFAULT "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-13
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-13 C-Win
ON CHOOSE OF BUTTON-13 IN FRAME DEFAULT-FRAME /* Ta bort alle */
DO:
  STATUS DEFAULT "Sletter RECEIPT".
  DELETE FROM jowereceipt.
  {&OPEN-QUERY-BROWSE-RECEIPT}

  STATUS DEFAULT "Sletter REC_ROW".
  DELETE FROM jowerec_row.
  {&OPEN-QUERY-BROWSE-REC_ROW}

  STATUS DEFAULT "Sletter REC_PAY".
  DELETE FROM jowerec_pay.
  {&OPEN-QUERY-BROWSE-REC_PAY}

  STATUS DEFAULT "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 C-Win
ON CHOOSE OF BUTTON-2 IN FRAME DEFAULT-FRAME /* Läs in */
DO:
    RUN LesInnRec_Row (FI-RecRowFil:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
    {&OPEN-QUERY-BROWSE-REC_ROW}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-3 C-Win
ON CHOOSE OF BUTTON-3 IN FRAME DEFAULT-FRAME /* Ta bort */
DO:
    FOR EACH joweREC_ROW. 
        DELETE joweREC_ROW.
    END.
    {&OPEN-QUERY-BROWSE-REC_ROW}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-5 C-Win
ON CHOOSE OF BUTTON-5 IN FRAME DEFAULT-FRAME /* Fil */
DO:
    DEFINE VARIABLE OKpressed AS LOGICAL    NO-UNDO.
    ASSIGN BUTTON-2:SENSITIVE = FALSE.
    SYSTEM-DIALOG GET-FILE FI-ReceiptFil
          TITLE      "Välj RECEIPT fil ..."
          FILTERS    "Datafil (*.dat)"   "*.dat"
          MUST-EXIST
          USE-FILENAME
          RETURN-TO-START-DIR
          UPDATE OKpressed.
      IF OKpressed = TRUE THEN DO:
          ASSIGN FI-ReceiptFil:SCREEN-VALUE = FI-ReceiptFil
                 BUTTON-6:SENSITIVE = TRUE.
      END.
      RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-6 C-Win
ON CHOOSE OF BUTTON-6 IN FRAME DEFAULT-FRAME /* Läs in */
DO:
    RUN LesInnReceipt (FI-ReceiptFil:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
    {&OPEN-QUERY-BROWSE-RECEIPT}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-7 C-Win
ON CHOOSE OF BUTTON-7 IN FRAME DEFAULT-FRAME /* Ta bort */
DO:
    FOR EACH joweRECEIPT. 
        DELETE joweRECEIPT.
    END.
    {&OPEN-QUERY-BROWSE-RECEIPT}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-9 C-Win
ON CHOOSE OF BUTTON-9 IN FRAME DEFAULT-FRAME /* Fil */
DO:
    DEFINE VARIABLE OKpressed AS LOGICAL    NO-UNDO.
    ASSIGN BUTTON-2:SENSITIVE = FALSE.
    SYSTEM-DIALOG GET-FILE FI-RecPayFil
          TITLE      "Välj REC_PAY fil ..."
          FILTERS    "Datafil (*.dat)"   "*.dat"
          MUST-EXIST
          USE-FILENAME
          RETURN-TO-START-DIR
          UPDATE OKpressed.
      IF OKpressed = TRUE THEN DO:
          ASSIGN FI-RecPayFil:SCREEN-VALUE = FI-RecPayFil
                 BUTTON-10:SENSITIVE = TRUE.
      END.
      RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-EXPORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-EXPORT C-Win
ON CHOOSE OF BUTTON-EXPORT IN FRAME DEFAULT-FRAME /* EXPORTERA */
DO:
    DEFINE VARIABLE iRowNum    AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cOldButik  AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cButikDir  AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE iStatus    AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cKvittoFil AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cDateFormat      AS CHARACTER  NO-UNDO.
    ASSIGN cDateFormat            = SESSION:DATE-FORMAT
           SESSION:DATE-FORMAT    = "ymd".

    STATUS DEFAULT "Export av RECEIPT".
    FOR EACH joweRECEIPT NO-LOCK 
        BREAK 
        BY JoweRECEIPT.OUR_ID
        BY JoweRECEIPT.ACT_DATE
        BY JoweRECEIPT.RECEIPT_NO
        BY JoweRECEIPT.ACT_TIME:

        IF FIRST-OF(JoweRECEIPT.ACT_DATE)THEN 
        DO:
            OUTPUT CLOSE.
            ASSIGN cButikDir = FI-Katalog + "\" + SUBSTR(joweRECEIPT.RECEIPT_NO,1,5)
                   cOldButik = SUBSTR(joweRECEIPT.RECEIPT_NO,1,5).
            OS-CREATE-DIR VALUE(cButikDir).
            ASSIGN iStatus = OS-ERROR.
            IF iStatus NE 0 THEN DO:
                MESSAGE "Directory " cButikDir " kan inte skapes. "
                     VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                RETURN NO-APPLY.
            END.
            ASSIGN cKvittoFil = "MD" + STRING(JoweRECEIPT.ACT_DATE,"999999") + ".txt".
            OUTPUT TO VALUE(cButikDir + "\" + cKvittoFil).
        END.

        RUN Exportera ("H",0).
         ASSIGN iRowNum = 0.
         FOR EACH joweREC_ROW WHERE joweREC_ROW.RECEIPT_NO =  joweRECEIPT.RECEIPT_NO:
             ASSIGN iRowNum = joweREC_ROW.ROW_NO.
             RUN Exportera ("L",iRowNum).
         END.
         FOR EACH joweREC_PAY WHERE joweREC_PAY.RECEIPT_NO = joweRECEIPT.RECEIPT_NO.
             ASSIGN iRowNum = iRowNum + 1.
             RUN Exportera ("P",iRowNum).
         END.
     END.
     OUTPUT CLOSE.
     ASSIGN SESSION:DATE-FORMAT    = cDateFormat.
     STATUS DEFAULT "".

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-RECEIPT
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY FI-ReceiptFil CB-Receipt CB-Rec-Row FI-RecRowFil FI-RecPayFil 
          FI-Katalog 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE BUTTON-5 BUTTON-6 BUTTON-7 CB-Receipt BROWSE-RECEIPT BUTTON-1 BUTTON-2 
         BUTTON-3 CB-Rec-Row BROWSE-REC_ROW BUTTON-9 BUTTON-10 BUTTON-11 
         BROWSE-REC_PAY BUTTON-12 BUTTON-13 BUTTON-EXPORT B-Katalog 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Exportera C-Win 
PROCEDURE Exportera :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER cType AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER iRow        AS INTEGER    NO-UNDO.
    EXPORT 
    cType
    iRow
    joweRECEIPT.RECEIPT_NO 
    joweRECEIPT.ACT_NO     
    joweRECEIPT.ACT_DATE   
    joweRECEIPT.LogicDate  
    joweRECEIPT.ACT_TIME   
    IF cType = "H" THEN joweRECEIPT.OUR_ID     ELSE ?
    IF cType = "H" THEN joweRECEIPT.OUR_NO     ELSE ?
    IF cType = "H" THEN joweRECEIPT.INDIVID_NO ELSE ?
    IF cType = "H" THEN joweRECEIPT.INDIVID_ID ELSE ?
    IF cType = "H" THEN joweRECEIPT.NAME       ELSE ?
    IF cType = "H" THEN joweRECEIPT.SALESP_ID  ELSE ?
    IF cType = "H" THEN joweRECEIPT.USER_ID    ELSE ?
    IF cType = "H" THEN joweRECEIPT.EME_NO     ELSE ?
    IF cType = "H" THEN joweRECEIPT.ACCOUNT_2  ELSE ?
    IF cType = "H" THEN joweRECEIPT.PROJECT_ID ELSE ?
    IF cType = "H" THEN joweRECEIPT.NOTES      ELSE ?
    IF cType = "H" THEN joweRECEIPT.INSERTED   ELSE ?
    IF cType = "H" THEN joweRECEIPT.UPDATED    ELSE ?
    IF cType = "H" THEN joweRECEIPT.I_USER_NO  ELSE ?
    IF cType = "H" THEN joweRECEIPT.USER_NO    ELSE ?
    IF cType = "H" THEN joweRECEIPT.CAMP_NO    ELSE ?
    IF cType = "H" THEN joweRECEIPT.CMP_ACT_NO ELSE ?
    IF cType = "H" THEN joweRECEIPT.IS_INTERN  ELSE ?
    IF cType = "H" THEN joweRECEIPT.IS_DONE    ELSE ?
    IF cType = "H" THEN joweRECEIPT.STOCKVALUE ELSE ?
    IF cType = "H" THEN joweRECEIPT.FREIGHT    ELSE ?
    IF cType = "H" THEN joweRECEIPT.INV_FEE    ELSE ?
    IF cType = "H" THEN joweRECEIPT.VAT        ELSE ?
    IF cType = "H" THEN joweRECEIPT.ROWSUM     ELSE ?
    IF cType = "H" THEN joweRECEIPT.DISCOUNT   ELSE ?
    IF cType = "H" THEN joweRECEIPT.DISCOUNT_P ELSE ?
    IF cType = "H" THEN joweRECEIPT.ROUND_OFF  ELSE ?
    IF cType = "H" THEN joweRECEIPT.TOTAL      ELSE ?
    IF cType = "H" THEN joweRECEIPT.CURR_RATE  ELSE ?
    IF cType = "H" THEN joweRECEIPT.CURRENCYID ELSE ?
    IF cType = "H" THEN joweRECEIPT.IS_APPROVE ELSE ?
    IF cType = "H" THEN joweRECEIPT.IS_SENT    ELSE ?
    IF cType = "H" THEN joweRECEIPT.BUYER_TYPE ELSE ?
    IF cType = "H" THEN joweRECEIPT.BALANCE_NO ELSE ?
    IF cType = "H" THEN joweRECEIPT.TillType   ELSE ?
    IF cType = "H" THEN joweRECEIPT.TillUnitId ELSE ?
    IF cType = "H" THEN joweRECEIPT.S_CHANGE   ELSE ?
    IF cType = "H" THEN joweRECEIPT.PRINT_SAP  ELSE ?
    IF cType = "H" THEN joweRECEIPT.ORIGIN_NO  ELSE ?
    IF cType = "H" THEN joweRECEIPT.OFFLINE_NO ELSE ?
    IF cType = "H" THEN joweRECEIPT.CUSTGRP_ID ELSE ?
    IF cType = "H" THEN joweRECEIPT.SHOP_ID    ELSE ?
    IF cType = "H" THEN joweRECEIPT.CASHREG_NO ELSE ?
    IF cType = "H" THEN joweRECEIPT.SIGNATURE  ELSE ?
    IF cType = "H" THEN joweRECEIPT.SUPP_PRICE ELSE ?
    IF cType = "H" THEN joweRECEIPT.CARTYPEID  ELSE ?
    IF cType = "H" THEN joweRECEIPT.CREDCARDID ELSE ?
    IF cType = "H" THEN joweRECEIPT.LEGETIMAT  ELSE ?
    IF cType = "H" THEN joweRECEIPT.THEIR_ID   ELSE ?
    IF cType = "H" THEN joweRECEIPT.EME_NAME   ELSE ?
    IF cType = "H" THEN joweRECEIPT.IS_STAT    ELSE ?
    IF cType = "H" THEN joweRECEIPT.IS_REPORT  ELSE ?
    IF cType = "H" THEN joweRECEIPT.IS_STAFF   ELSE ?
    IF cType = "H" THEN joweRECEIPT.IS_LOCSOLD ELSE ?
/*     IF cType = "H" THEN joweRECEIPT.IS_PRINTED ELSE ? */
/*     IF cType = "H" THEN joweRECEIPT.IS_EXPORT  ELSE ? */
/*     IF cType = "L" THEN joweREC_ROW.SEQNO           ELSE ? */
/*         joweREC_ROW.ROW_NO */
    IF cType = "L" THEN joweREC_ROW.PRODUCT_NO      ELSE ?
    IF cType = "L" THEN joweREC_ROW.UNIT_ID         ELSE ?
    IF cType = "L" THEN joweREC_ROW.PriceTypeId     ELSE ?
    IF cType = "L" THEN joweREC_ROW.QUANTITY        ELSE ?
    IF cType = "L" THEN joweREC_ROW.STK_CONVF       ELSE ?
    IF cType = "L" THEN joweREC_ROW.SUPP_PRICE      ELSE ?
    IF cType = "L" THEN joweREC_ROW.UNIT_PRICE      ELSE ?
    IF cType = "L" THEN joweREC_ROW.AMOUNT          ELSE ?
    IF cType = "L" THEN joweREC_ROW.DISCOUNT_P      ELSE ?
    IF cType = "L" THEN joweREC_ROW.DISCOUNT        ELSE ?
    IF cType = "L" THEN joweREC_ROW.VAT_P           ELSE ?
    IF cType = "L" THEN joweREC_ROW.VAT             ELSE ?
    IF cType = "L" THEN joweREC_ROW.CONTRACTNO      ELSE ?
    IF cType = "L" THEN joweREC_ROW.GROUP_NO        ELSE ?
    IF cType = "L" THEN joweREC_ROW.ACCOUNT_4       ELSE ?
    IF cType = "L" THEN joweREC_ROW.ACCOUNT_5       ELSE ?
    IF cType = "L" THEN joweREC_ROW.IS_MAIN_ROW     ELSE ?
    IF cType = "L" THEN joweREC_ROW.IS_TEXT         ELSE ?
    IF cType = "L" THEN joweREC_ROW.IS_MANDISC      ELSE ?
    IF cType = "L" THEN joweREC_ROW.IS_DISC_P       ELSE ?
    IF cType = "L" THEN joweREC_ROW.IS_N_PRICE      ELSE ?
    IF cType = "L" THEN joweREC_ROW.PRODUCT_ID      ELSE ?
    IF cType = "L" THEN joweREC_ROW.DESCRIPT        ELSE ?
    IF cType = "L" THEN joweREC_ROW.PRODGR_ID       ELSE ?
/*     joweREC_ROW.RECEIPT_NO */
    IF cType = "L" THEN joweREC_ROW.ORIGIN_ID       ELSE ?
    IF cType = "L" THEN joweREC_ROW.PROD_CLASS      ELSE ?
    IF cType = "L" THEN joweREC_ROW.DISC_CUST       ELSE ?
    IF cType = "L" THEN joweREC_ROW.DISC_EMPL       ELSE ?
    IF cType = "L" THEN joweREC_ROW.DISC_ACT        ELSE ?
/*     IF cType = "L" THEN joweREC_ROW.INSERTED        ELSE ? */
    IF cType = "L" THEN joweREC_ROW.CRED_QTY        ELSE ?
    IF cType = "L" THEN joweREC_ROW.CUSTORD_NO      ELSE ?
    IF cType = "L" THEN joweREC_ROW.CORDROW_NO      ELSE ?
    IF cType = "L" THEN joweREC_ROW.REPORT_CODE     ELSE ?
    IF cType = "L" THEN joweREC_ROW.STAT_CODE       ELSE ?
    IF cType = "L" THEN joweREC_ROW.IS_PRESOLD      ELSE ?
/*     IF cType = "L" THEN joweREC_ROW.IsTillUnitStock ELSE ? */
    IF cType = "L" THEN joweREC_ROW.OUR_NO          ELSE ?
    IF cType = "L" THEN joweREC_ROW.UnitPriceWoAct  ELSE ?
    IF cType = "L" THEN joweREC_ROW.DiscTypeId      ELSE ?
/*     IF cType = "P" THEN joweREC_PAY.RECEIPT_NO      ELSE ? */
    IF cType = "P" THEN joweREC_PAY.PAYTYPE_ID      ELSE ?
    IF cType = "P" THEN joweREC_PAY.PAYMENT_ID      ELSE ?
    IF cType = "P" THEN joweREC_PAY.RATE_OUT        ELSE ?
    IF cType = "P" THEN joweREC_PAY.AMOUNT          ELSE ?
    IF cType = "P" THEN joweREC_PAY.AMOUNT_ORG      ELSE ?
    IF cType = "P" THEN joweREC_PAY.PAIDAMOUNT      ELSE ?
    IF cType = "P" THEN joweREC_PAY.AMOUNT_RET      ELSE ?
     .                                              
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LesInnReceipt C-Win 
PROCEDURE LesInnReceipt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER cFilNavn AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cDummy           AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE iButiks_nr       AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cNumericFormat   AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cDateFormat      AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE piLoop           AS DEC FORMAT ">>>>>>>>>9" NO-UNDO.
    DEFINE VARIABLE pi2Loop          AS INT        NO-UNDO.
    DEF    VAR      piVer            AS INT        NO-UNDO.
    ASSIGN FRAME default-frame
        CB-Receipt .

    IF AVAILABLE ttjowereceipt-pbntmegapb THEN DELETE ttjowereceipt-pbntmegapb.
    IF AVAILABLE ttjowereceipt-pbntkvtemp THEN DELETE ttjowereceipt-pbntkvtemp.
    IF AVAILABLE ttjowereceipt-s10001     THEN DELETE ttjowereceipt-s10001.
    CREATE ttjowereceipt-pbntmegapb.
    CREATE ttjowereceipt-pbntkvtemp.
    CREATE ttjowereceipt-s10001.

    STATUS default "Leser inn RECEIPT".

        .

/*     ASSIGN iAntNya     = 0                                                           */
/*            iAntUppdat  = 0                                                           */
/*            iAntNyaHg   = 0                                                           */
/*            iAntFel     = 0                                                           */
/*            cErrorFil   = ENTRY(NUM-ENTRIES(cFilNavn,".") - 1,cFilNavn,".") + ".fel". */
    ETIME(TRUE).
    ASSIGN cNumericFormat         = SESSION:NUMERIC-FORMAT
           cDateFormat            = SESSION:DATE-FORMAT
           SESSION:NUMERIC-FORMAT = "American"
           SESSION:DATE-FORMAT    = "ymd"
           piVer                  = CB-Receipt
           .
    IF SEARCH(cFilNavn) = ? THEN DO:
        MESSAGE "Filen finns inte!"
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    END.
    ASSIGN
        piLoop = 0
        .
    OUTPUT STREAM ErrLogg TO VALUE(cFilNavn + ".err").
    INPUT FROM VALUE(cFilNavn).
/*     IMPORT UNFORMATTED cDummy. /* vi förutsätter att filen har en headerrecord */ */
    DO WHILE TRUE ON ENDKEY UNDO, LEAVE
                  ON ERROR UNDO, NEXT:
        /*PROCESS EVENTS.*/
        ASSIGN 
            piLoop = piLoop + 1
            .
        IF piLoop MODULO piModulo = 0 THEN
            STATUS default "Leser inn RECEIPT post " + string(piLoop).

        CREATE joweRECEIPT.
        CASE piVer:
            WHEN 1 THEN DO:
                IMPORT DELIMITER ";" ttjowereceipt-pbntmegapb NO-ERROR.
                ASSIGN ttjoweRECEIPT-pbntmegapb.RECEIPT_NO = FILL("0",14 - LENGTH(ttjoweRECEIPT-pbntmegapb.RECEIPT_NO)) + ttjoweRECEIPT-pbntmegapb.RECEIPT_NO NO-ERROR.
                BUFFER-COPY ttjowereceipt-pbntmegapb TO joweRECEIPT NO-ERROR.
            END.
            WHEN 2 THEN DO:
                IMPORT DELIMITER ";" ttjowereceipt-pbntkvtemp NO-ERROR.
                ASSIGN ttjoweRECEIPT-pbntkvtemp.RECEIPT_NO = FILL("0",14 - LENGTH(ttjoweRECEIPT-pbntkvtemp.RECEIPT_NO)) + ttjoweRECEIPT-pbntkvtemp.RECEIPT_NO NO-ERROR.
                BUFFER-COPY ttjowereceipt-pbntkvtemp TO joweRECEIPT NO-ERROR.
            END.
            WHEN 3 THEN DO:
                IMPORT DELIMITER ";" ttjowereceipt-s10001 NO-ERROR.
                ASSIGN ttjoweRECEIPT-s10001.RECEIPT_NO = FILL("0",14 - LENGTH(ttjoweRECEIPT-s10001.RECEIPT_NO)) + ttjoweRECEIPT-s10001.RECEIPT_NO NO-ERROR.
                BUFFER-COPY ttjowereceipt-s10001 TO joweRECEIPT NO-ERROR.
            END.
        END CASE.
        IF ERROR-STATUS:ERROR THEN 
        DO:
            PUT STREAM ErrLogg UNFORMATTED "Feil på linje " + string(piLoop) SKIP. 
            DO pi2Loop = 1 TO ERROR-STATUS:NUM-MESSAGES:
                PUT STREAM ErrLogg UNFORMATTED ERROR-STATUS:GET-MESSAGE(pi2Loop) SKIP.
            END.
            IF AVAILABLE joweRECEIPT THEN
                DELETE joweRECEIPT.
            UNDO, NEXT.  
        END.
/*        ASSIGN joweRECEIPT.RECEIPT_NO = FILL("0",14 - LENGTH(joweRECEIPT.RECEIPT_NO)) + joweRECEIPT.RECEIPT_NO NO-ERROR.*/
/*                joweRECEIPT.ACT_NO     = TRIM(joweRECEIPT.ACT_NO,"'")     */
/*                joweRECEIPT.ACT_TIME   = TRIM(joweRECEIPT.ACT_TIME,"'")   */
/*                joweRECEIPT.OUR_ID     = TRIM(joweRECEIPT.OUR_ID,"'")     */
/*                joweRECEIPT.OUR_NO     = TRIM(joweRECEIPT.OUR_NO,"'")     */
/*                joweRECEIPT.INDIVID_NO = TRIM(joweRECEIPT.INDIVID_NO,"'") */
/*                joweRECEIPT.INDIVID_ID = TRIM(joweRECEIPT.INDIVID_ID,"'") */
/*                joweRECEIPT.NAME       = TRIM(joweRECEIPT.NAME,"'")       */
/*                joweRECEIPT.SALESP_ID  = TRIM(joweRECEIPT.SALESP_ID,"'")  */
/*                joweRECEIPT.USER_ID    = TRIM(joweRECEIPT.USER_ID,"'")    */
/*                joweRECEIPT.EME_NO     = TRIM(joweRECEIPT.EME_NO,"'")     */
/*                joweRECEIPT.ACCOUNT_2  = TRIM(joweRECEIPT.ACCOUNT_2,"'")  */
/*                joweRECEIPT.PROJECT_ID = TRIM(joweRECEIPT.PROJECT_ID,"'") */
/*                joweRECEIPT.NOTES      = TRIM(joweRECEIPT.NOTES,"'")      */
/*                joweRECEIPT.I_USER_NO  = TRIM(joweRECEIPT.I_USER_NO,"'")  */
/*                joweRECEIPT.USER_NO    = TRIM(joweRECEIPT.USER_NO,"'")    */
/*                joweRECEIPT.CAMP_NO    = TRIM(joweRECEIPT.CAMP_NO,"'")    */
/*                joweRECEIPT.CMP_ACT_NO = TRIM(joweRECEIPT.CMP_ACT_NO,"'") */
/*                joweRECEIPT.IS_INTERN  = TRIM(joweRECEIPT.IS_INTERN,"'")  */
/*                joweRECEIPT.IS_DONE    = TRIM(joweRECEIPT.IS_DONE,"'")    */
/*                joweRECEIPT.CURRENCYID = TRIM(joweRECEIPT.CURRENCYID,"'") */
/*                joweRECEIPT.IS_APPROVE = TRIM(joweRECEIPT.IS_APPROVE,"'") */
/*                joweRECEIPT.IS_SENT    = TRIM(joweRECEIPT.IS_SENT,"'")    */
/*                joweRECEIPT.BUYER_TYPE = TRIM(joweRECEIPT.BUYER_TYPE,"'") */
/*                joweRECEIPT.RECEIPT_NO = TRIM(joweRECEIPT.RECEIPT_NO,"'") */
/*                joweRECEIPT.BALANCE_NO = TRIM(joweRECEIPT.BALANCE_NO,"'") */
/*                joweRECEIPT.TillType   = TRIM(joweRECEIPT.TillType,"'")   */
/*                joweRECEIPT.TillUnitId = TRIM(joweRECEIPT.TillUnitId,"'") */
/*                joweRECEIPT.PRINT_SAP  = TRIM(joweRECEIPT.PRINT_SAP,"'")  */
/*                joweRECEIPT.ORIGIN_NO  = TRIM(joweRECEIPT.ORIGIN_NO,"'")  */
/*                joweRECEIPT.OFFLINE_NO = TRIM(joweRECEIPT.OFFLINE_NO,"'") */
/*                joweRECEIPT.CUSTGRP_ID = TRIM(joweRECEIPT.CUSTGRP_ID,"'") */
/*                joweRECEIPT.SHOP_ID    = TRIM(joweRECEIPT.SHOP_ID,"'")    */
/*                joweRECEIPT.SIGNATURE  = TRIM(joweRECEIPT.SIGNATURE,"'")  */
/*                joweRECEIPT.CARTYPEID  = TRIM(joweRECEIPT.CARTYPEID,"'")  */
/*                joweRECEIPT.CREDCARDID = TRIM(joweRECEIPT.CREDCARDID,"'") */
/*                joweRECEIPT.LEGETIMAT  = TRIM(joweRECEIPT.LEGETIMAT,"'")  */
/*                joweRECEIPT.THEIR_ID   = TRIM(joweRECEIPT.THEIR_ID,"'")   */
/*                joweRECEIPT.EME_NAME   = TRIM(joweRECEIPT.EME_NAME,"'")   */
/*                joweRECEIPT.IS_STAT    = TRIM(joweRECEIPT.IS_STAT,"'")    */
/*                joweRECEIPT.IS_REPORT  = TRIM(joweRECEIPT.IS_REPORT,"'")  */
/*                joweRECEIPT.IS_STAFF   = TRIM(joweRECEIPT.IS_STAFF,"'")   */
/*                joweRECEIPT.IS_LOCSOLD = TRIM(joweRECEIPT.IS_LOCSOLD,"'") */
/*                joweRECEIPT.IS_PRINTED = TRIM(joweRECEIPT.IS_PRINTED,"'") */
/*                joweRECEIPT.IS_EXPORT  = TRIM(joweRECEIPT.IS_EXPORT,"'"). */

        RELEASE joweRECEIPT.
    END.
/*     IF LASTKEY = -2 THEN    */
/*         DELETE joweRECEIPT. */
    INPUT CLOSE.
    OUTPUT STREAM ErrLogg CLOSE.
    ASSIGN SESSION:NUMERIC-FORMAT = cNumericFormat
           SESSION:DATE-FORMAT    = cDateFormat.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LesInnRec_Pay C-Win 
PROCEDURE LesInnRec_Pay :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER cFilNavn AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDummy           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iButiks_nr       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cNumericFormat   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDateFormat      AS CHARACTER NO-UNDO.
    DEF    VAR      piLoop           AS INT       NO-UNDO.
    DEFINE VARIABLE pi2Loop          AS INT        NO-UNDO.

    STATUS DEFAULT "Leser inn REC_PAY".

/*     ASSIGN iAntNya     = 0                                                           */
/*            iAntUppdat  = 0                                                           */
/*            iAntNyaHg   = 0                                                           */
/*            iAntFel     = 0                                                           */
/*            cErrorFil   = ENTRY(NUM-ENTRIES(cFilNavn,".") - 1,cFilNavn,".") + ".fel". */
    ETIME(TRUE).
    ASSIGN cNumericFormat         = SESSION:NUMERIC-FORMAT
           cDateFormat            = SESSION:DATE-FORMAT
           SESSION:NUMERIC-FORMAT = "American"
           SESSION:DATE-FORMAT    = "ymd".
    IF SEARCH(cFilNavn) = ? THEN DO:
        MESSAGE "Filen finns inte!"
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    END.
    OUTPUT STREAM ErrLogg TO VALUE(cFilNavn + ".err").
    INPUT FROM VALUE(cFilNavn).
/*     IMPORT UNFORMATTED cDummy. /* vi förutsätter att filen har en headerrecord */ */
    DO WHILE TRUE ON ENDKEY UNDO, LEAVE
                  ON ERROR UNDO, NEXT:
        /*PROCESS EVENTS.*/
        ASSIGN 
            piLoop = piLoop + 1
            .
        IF piLoop MODULO piModulo = 0 THEN
            STATUS DEFAULT "Leser inn REC_PAY post " + string(piLoop).
        CREATE joweREC_PAY.
        IMPORT DELIMITER ";" joweREC_PAY NO-ERROR.
        IF ERROR-STATUS:ERROR THEN 
        DO:
            PUT STREAM ErrLogg UNFORMATTED "Feil på linje " + string(piLoop) SKIP. 
            DO pi2Loop = 1 TO ERROR-STATUS:NUM-MESSAGES:
                PUT STREAM ErrLogg UNFORMATTED ERROR-STATUS:GET-MESSAGE(pi2Loop) SKIP.
            END.
            IF AVAILABLE joweREC_PAY THEN
                DELETE joweREC_PAY.
            UNDO, NEXT.  
        END.
/*        ASSIGN joweREC_PAY.RECEIPT_NO = FILL("0",14 - LENGTH(joweREC_PAY.RECEIPT_NO)) + joweREC_PAY.RECEIPT_NO. */
/*                joweREC_PAY.RECEIPT_NO = TRIM(joweREC_PAY.RECEIPT_NO,"'")  */
/*                joweREC_PAY.PAYTYPE_ID = TRIM(joweREC_PAY.PAYTYPE_ID,"'")  */
/*                joweREC_PAY.PAYMENT_ID = TRIM(joweREC_PAY.PAYMENT_ID,"'"). */
        RELEASE joweREC_PAY.
    END.
/*     IF LASTKEY = -2 THEN    */
/*         DELETE joweREC_PAY. */
    INPUT CLOSE.
    OUTPUT STREAM ErrLogg CLOSE.
    ASSIGN SESSION:NUMERIC-FORMAT = cNumericFormat
           SESSION:DATE-FORMAT    = cDateFormat.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LesInnRec_Row C-Win 
PROCEDURE LesInnRec_Row PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  {jowerec_row-pbntmegapb.i}
  {jowerec_row-pbntxlent.i}
  {jowerec_row-s10001.i}

------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER cFilNavn AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cDummy           AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE iButiks_nr       AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cNumericFormat   AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cDateFormat      AS CHARACTER  NO-UNDO.
    DEF    VAR      piLoop           AS INT       NO-UNDO.
    DEFINE VARIABLE pi2Loop          AS INT        NO-UNDO.
    DEF    VAR      piVer            AS INT        NO-UNDO.
    ASSIGN FRAME default-frame
        CB-Rec-Row .

    IF AVAILABLE ttjowerec_row-pbntmegapb THEN DELETE ttjowerec_row-pbntmegapb.
    IF AVAILABLE ttjowerec_row-pbntxlent  THEN DELETE ttjowerec_row-pbntxlent.
    IF AVAILABLE ttjowerec_row-s10001     THEN DELETE ttjowerec_row-s10001.
    CREATE ttjowerec_row-pbntmegapb.
    CREATE ttjowerec_row-pbntxlent.
    CREATE ttjowerec_row-s10001.


    STATUS DEFAULT "Leser inn REC_ROW".

/*     ASSIGN iAntNya     = 0                                                           */
/*            iAntUppdat  = 0                                                           */
/*            iAntNyaHg   = 0                                                           */
/*            iAntFel     = 0                                                           */
/*            cErrorFil   = ENTRY(NUM-ENTRIES(cFilNavn,".") - 1,cFilNavn,".") + ".fel". */
    ETIME(TRUE).
    ASSIGN cNumericFormat         = SESSION:NUMERIC-FORMAT
           cDateFormat            = SESSION:DATE-FORMAT
           SESSION:NUMERIC-FORMAT = "American"
           SESSION:DATE-FORMAT    = "ymd"
           piVer                  = CB-Rec-Row.
    IF SEARCH(cFilNavn) = ? THEN DO:
        MESSAGE "Filen finns inte!"
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    END.
    OUTPUT STREAM ErrLogg TO VALUE(cFilNavn + ".err").
    INPUT FROM VALUE(cFilNavn).
/*     IMPORT UNFORMATTED cDummy. /* vi förutsätter att filen har en headerrecord */ */
    DO WHILE TRUE ON ENDKEY UNDO, LEAVE
                  ON ERROR UNDO, NEXT:
        /*PROCESS EVENTS.*/
        ASSIGN 
            piLoop = piLoop + 1
            .
        IF piLoop MODULO piModulo = 0 THEN
            STATUS DEFAULT "Leser inn REC_ROW post " + string(piLoop).
        CREATE joweREC_ROW.
        CASE piVer:
            WHEN 1 THEN DO:
                IMPORT DELIMITER ";" ttjowerec_row-pbntmegapb NO-ERROR.
                ASSIGN ttjowerec_row-pbntmegapb.RECEIPT_NO = FILL("0",14 - LENGTH(ttjowerec_row-pbntmegapb.RECEIPT_NO)) + ttjowerec_row-pbntmegapb.RECEIPT_NO
                       ttjowerec_row-pbntmegapb.SEQNO      = TRIM(ttjowerec_row-pbntmegapb.SEQNO,"'")
                       ttjowerec_row-pbntmegapb.SEQNO      = FILL("0",14 - LENGTH(ttjowerec_row-pbntmegapb.SEQNO)) + ttjowerec_row-pbntmegapb.SEQNO.
                BUFFER-COPY ttjowerec_row-pbntmegapb TO joweREC_ROW NO-ERROR.
            END.
            WHEN 2 THEN DO:
                IMPORT DELIMITER ";" ttjowerec_row-pbntxlent NO-ERROR.
                ASSIGN ttjoweREC_ROW-pbntxlent.RECEIPT_NO = FILL("0",14 - LENGTH(ttjoweREC_ROW-pbntxlent.RECEIPT_NO)) + ttjoweREC_ROW-pbntxlent.RECEIPT_NO
                       ttjoweREC_ROW-pbntxlent.SEQNO      = TRIM(ttjoweREC_ROW-pbntxlent.SEQNO,"'")
                       ttjoweREC_ROW-pbntxlent.SEQNO      = FILL("0",14 - LENGTH(ttjoweREC_ROW-pbntxlent.SEQNO)) + ttjoweREC_ROW-pbntxlent.SEQNO.
                BUFFER-COPY ttjowerec_row-pbntxlent TO joweREC_ROW NO-ERROR.
            END.
            WHEN 3 THEN DO:
                IMPORT DELIMITER ";" ttjowerec_row-s10001 NO-ERROR.
                ASSIGN ttjoweREC_ROW-s10001.RECEIPT_NO = FILL("0",14 - LENGTH(ttjoweREC_ROW-s10001.RECEIPT_NO)) + ttjoweREC_ROW-s10001.RECEIPT_NO
                       ttjoweREC_ROW-s10001.SEQNO      = TRIM(ttjoweREC_ROW-s10001.SEQNO,"'")
                       ttjoweREC_ROW-s10001.SEQNO      = FILL("0",14 - LENGTH(ttjoweREC_ROW-s10001.SEQNO)) + ttjoweREC_ROW-s10001.SEQNO.
                BUFFER-COPY ttjowerec_row-s10001 TO joweREC_ROW NO-ERROR.
            END.
        END CASE.
        IF ERROR-STATUS:ERROR THEN 
        DO:
            PUT STREAM ErrLogg UNFORMATTED "Feil på linje " + string(piLoop) SKIP. 
            DO pi2Loop = 1 TO ERROR-STATUS:NUM-MESSAGES:
                PUT STREAM ErrLogg UNFORMATTED ERROR-STATUS:GET-MESSAGE(pi2Loop) SKIP.
            END.
            IF AVAILABLE joweREC_ROW THEN
                DELETE joweREC_ROW.
            UNDO, NEXT.  
        END.
 
/*       ASSIGN joweREC_ROW.SEQNO           = TRIM(joweREC_ROW.SEQNO,"'") */
/*               joweREC_ROW.SEQNO           = FILL("0",14 - LENGTH(joweREC_ROW.SEQNO)) + joweREC_ROW.SEQNO */
/*                joweREC_ROW.RECEIPT_NO      = FILL("0",14 - LENGTH(joweREC_ROW.RECEIPT_NO)) + joweREC_ROW.RECEIPT_NO */
            
/*                joweREC_ROW.PRODUCT_NO      = TRIM(joweREC_ROW.PRODUCT_NO,"'")      */
/*                joweREC_ROW.UNIT_ID         = TRIM(joweREC_ROW.UNIT_ID,"'")         */
/*                joweREC_ROW.PriceTypeId     = TRIM(joweREC_ROW.PriceTypeId,"'")     */
/*                joweREC_ROW.CONTRACTNO      = TRIM(joweREC_ROW.CONTRACTNO,"'")      */
/*                joweREC_ROW.GROUP_NO        = TRIM(joweREC_ROW.GROUP_NO,"'")        */
/*                joweREC_ROW.ACCOUNT_4       = TRIM(joweREC_ROW.ACCOUNT_4,"'")       */
/*                joweREC_ROW.ACCOUNT_5       = TRIM(joweREC_ROW.ACCOUNT_5,"'")       */
/*                joweREC_ROW.IS_MAIN_ROW     = TRIM(joweREC_ROW.IS_MAIN_ROW,"'")     */
/*                joweREC_ROW.IS_TEXT         = TRIM(joweREC_ROW.IS_TEXT,"'")         */
/*                joweREC_ROW.IS_MANDISC      = TRIM(joweREC_ROW.IS_MANDISC,"'")      */
/*                joweREC_ROW.IS_DISC_P       = TRIM(joweREC_ROW.IS_DISC_P,"'")       */
/*                joweREC_ROW.IS_N_PRICE      = TRIM(joweREC_ROW.IS_N_PRICE,"'")      */
/*                joweREC_ROW.PRODUCT_ID      = TRIM(joweREC_ROW.PRODUCT_ID,"'")      */
/*                joweREC_ROW.DESCRIPT        = TRIM(joweREC_ROW.DESCRIPT,"'")        */
/*                joweREC_ROW.PRODGR_ID       = TRIM(joweREC_ROW.PRODGR_ID,"'")       */
/*                joweREC_ROW.RECEIPT_NO      = TRIM(joweREC_ROW.RECEIPT_NO,"'")      */
/*                joweREC_ROW.ORIGIN_ID       = TRIM(joweREC_ROW.ORIGIN_ID ,"'")      */
/*                joweREC_ROW.PROD_CLASS      = TRIM(joweREC_ROW.PROD_CLASS,"'")      */
/*                joweREC_ROW.CUSTORD_NO      = TRIM(joweREC_ROW.CUSTORD_NO,"'")      */
/*                joweREC_ROW.CORDROW_NO      = TRIM(joweREC_ROW.CORDROW_NO,"'")      */
/*                joweREC_ROW.REPORT_CODE     = TRIM(joweREC_ROW.REPORT_CODE,"'")     */
/*                joweREC_ROW.STAT_CODE       = TRIM(joweREC_ROW.STAT_CODE,"'")       */
/*                joweREC_ROW.IS_PRESOLD      = TRIM(joweREC_ROW.IS_PRESOLD,"'")      */
/*                joweREC_ROW.IsTillUnitStock = TRIM(joweREC_ROW.IsTillUnitStock,"'") */
/*                joweREC_ROW.OUR_NO          = TRIM(joweREC_ROW.OUR_NO,"'")          */
/*                joweREC_ROW.DiscTypeId      = TRIM(joweREC_ROW.DiscTypeId,"'").     */

        RELEASE joweREC_ROW.
    END.
/*     IF LASTKEY = -2 THEN    */
/*         DELETE joweREC_ROW. */
    INPUT CLOSE.
    OUTPUT STREAM ErrLogg CLOSE.
    ASSIGN SESSION:NUMERIC-FORMAT = cNumericFormat
           SESSION:DATE-FORMAT    = cDateFormat.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

