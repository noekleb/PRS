DEFINE NEW SHARED TEMP-TABLE TT_Rader  NO-UNDO
/*  1  A */ FIELD cType          AS CHARACTER
/*  2  B */ FIELD iRow           AS INTEGER
/*  3  C */ FIELD RECEIPT_NO     LIKE joweRECEIPT.RECEIPT_NO 
/*  4  D */ FIELD ACT_NO         LIKE joweRECEIPT.ACT_NO     
/*  5  E */ FIELD ACT_DATE       LIKE joweRECEIPT.ACT_DATE    
/*  6  F */ FIELD LogicDate      LIKE joweRECEIPT.LogicDate  
/*  7  G */ FIELD ACT_TIME       LIKE joweRECEIPT.ACT_TIME   
/*  8  H */ FIELD OUR_ID         LIKE joweRECEIPT.OUR_ID     
/*  9  I */ FIELD OUR_NO         LIKE joweRECEIPT.OUR_NO     
/* 10  J */ FIELD INDIVID_NO     LIKE joweRECEIPT.INDIVID_NO 
/* 11  K */ FIELD INDIVID_ID     LIKE joweRECEIPT.INDIVID_ID 
/* 12  L */ FIELD NAME           LIKE joweRECEIPT.NAME       
/* 13  M */ FIELD SALESP_ID      LIKE joweRECEIPT.SALESP_ID  
/* 14  N */ FIELD USER_ID        LIKE joweRECEIPT.USER_ID    
/* 15  O */ FIELD EME_NO         LIKE joweRECEIPT.EME_NO     
/* 16  P */ FIELD ACCOUNT_2      LIKE joweRECEIPT.ACCOUNT_2  
/* 17  Q */ FIELD PROJECT_ID     LIKE joweRECEIPT.PROJECT_ID 
/* 18  R */ FIELD NOTES          LIKE joweRECEIPT.NOTES      
/* 19  S */ FIELD INSERTED       LIKE joweRECEIPT.INSERTED   
/* 20  T */ FIELD UPDATED        LIKE joweRECEIPT.UPDATED    
/* 21  U */ FIELD I_USER_NO      LIKE joweRECEIPT.I_USER_NO  
/* 22  V */ FIELD USER_NO        LIKE joweRECEIPT.USER_NO    
/* 23  W */ FIELD CAMP_NO        LIKE joweRECEIPT.CAMP_NO    
/* 24  X */ FIELD CMP_ACT_NO     LIKE joweRECEIPT.CMP_ACT_NO 
/* 25  Y */ FIELD IS_INTERN      LIKE joweRECEIPT.IS_INTERN  
/* 26  Z */ FIELD IS_DONE        LIKE joweRECEIPT.IS_DONE    
/* 27 AA */ FIELD STOCKVALUE     LIKE joweRECEIPT.STOCKVALUE 
/* 28 AB */ FIELD FREIGHT        LIKE joweRECEIPT.FREIGHT    
/* 29 AC */ FIELD INV_FEE        LIKE joweRECEIPT.INV_FEE    
/* 30 AD */ FIELD VAT            LIKE joweRECEIPT.VAT        
/* 31 AE */ FIELD ROWSUM         LIKE joweRECEIPT.ROWSUM     
/* 32 AF */ FIELD DISCOUNT       LIKE joweRECEIPT.DISCOUNT   
/* 33 AG */ FIELD DISCOUNT_P     LIKE joweRECEIPT.DISCOUNT_P 
/* 34 AH */ FIELD ROUND_OFF      LIKE joweRECEIPT.ROUND_OFF 
/* 35 AI */ FIELD TOTAL          LIKE joweRECEIPT.TOTAL      
/* 36 AJ */ FIELD CURR_RATE      LIKE joweRECEIPT.CURR_RATE  
/* 37 AK */ FIELD CURRENCYID     LIKE joweRECEIPT.CURRENCYID 
/* 38 AL */ FIELD IS_APPROVE     LIKE joweRECEIPT.IS_APPROVE 
/* 39 AM */ FIELD IS_SENT        LIKE joweRECEIPT.IS_SENT    
/* 40 AN */ FIELD BUYER_TYPE     LIKE joweRECEIPT.BUYER_TYPE 
/* 41 AO */ FIELD BALANCE_NO     LIKE joweRECEIPT.BALANCE_NO 
/* 42 AP */ FIELD TillType       LIKE joweRECEIPT.TillType   
/* 43 AQ */ FIELD TillUnitId     LIKE joweRECEIPT.TillUnitId 
/* 44 AR */ FIELD S_CHANGE       LIKE joweRECEIPT.S_CHANGE   
/* 45 AS */ FIELD PRINT_SAP      LIKE joweRECEIPT.PRINT_SAP  
/* 46 AT */ FIELD ORIGIN_NO      LIKE joweRECEIPT.ORIGIN_NO  
/* 47 AU */ FIELD OFFLINE_NO     LIKE joweRECEIPT.OFFLINE_NO 
/* 48 AV */ FIELD CUSTGRP_ID     LIKE joweRECEIPT.CUSTGRP_ID 
/* 49 AW */ FIELD SHOP_ID        LIKE joweRECEIPT.SHOP_ID    
/* 50 AX */ FIELD CASHREG_NO     LIKE joweRECEIPT.CASHREG_NO 
/* 51 AY */ FIELD SIGNATURE      LIKE joweRECEIPT.SIGNATURE  
/* 52 AZ */ FIELD SUPP_PRICE     LIKE joweRECEIPT.SUPP_PRICE
/* 53 BA */ FIELD CARTYPEID      LIKE joweRECEIPT.CARTYPEID  
/* 54 BB */ FIELD CREDCARDID     LIKE joweRECEIPT.CREDCARDID 
/* 55 BC */ FIELD LEGETIMAT      LIKE joweRECEIPT.LEGETIMAT  
/* 56 BD */ FIELD THEIR_ID       LIKE joweRECEIPT.THEIR_ID   
/* 57 BE */ FIELD EME_NAME       LIKE joweRECEIPT.EME_NAME   
/* 58 BF */ FIELD IS_STAT        LIKE joweRECEIPT.IS_STAT    
/* 59 BG */ FIELD IS_REPORT      LIKE joweRECEIPT.IS_REPORT  
/* 60 BH */ FIELD IS_STAFF       LIKE joweRECEIPT.IS_STAFF   
/* 61 BI */ FIELD IS_LOCSOLD     LIKE joweRECEIPT.IS_LOCSOLD 
/* 62 BJ */ FIELD PRODUCT_NO     LIKE joweREC_ROW.PRODUCT_NO      
/* 63 BK */ FIELD UNIT_ID        LIKE joweREC_ROW.UNIT_ID         
/* 64 BL */ FIELD PriceTypeId    LIKE joweREC_ROW.PriceTypeId     
/* 65 BM */ FIELD QUANTITY       LIKE joweREC_ROW.QUANTITY        
/* 66 BN */ FIELD STK_CONVF      LIKE joweREC_ROW.STK_CONVF       
/* 67 BO */ FIELD L_SUPP_PRICE     LIKE joweREC_ROW.SUPP_PRICE      
/* 68 BP */ FIELD UNIT_PRICE     LIKE joweREC_ROW.UNIT_PRICE      
/* 69 BQ */ FIELD AMOUNT         LIKE joweREC_ROW.AMOUNT          
/* 70 BR */ FIELD L_DISCOUNT_P     LIKE joweREC_ROW.DISCOUNT_P      
/* 71 BS */ FIELD L_DISCOUNT       LIKE joweREC_ROW.DISCOUNT        
/* 72 BT */ FIELD VAT_P          LIKE joweREC_ROW.VAT_P           
/* 73 BU */ FIELD L_VAT            LIKE joweREC_ROW.VAT             
/* 74 BV */ FIELD CONTRACTNO     LIKE joweREC_ROW.CONTRACTNO      
/* 75 BW */ FIELD GROUP_NO       LIKE joweREC_ROW.GROUP_NO        
/* 76 BX */ FIELD ACCOUNT_4      LIKE joweREC_ROW.ACCOUNT_4       
/* 77 BY */ FIELD ACCOUNT_5      LIKE joweREC_ROW.ACCOUNT_5       
/* 78 BZ */ FIELD IS_MAIN_ROW    LIKE joweREC_ROW.IS_MAIN_ROW     
/* 79 CA */ FIELD IS_TEXT        LIKE joweREC_ROW.IS_TEXT         
/* 80 CB */ FIELD IS_MANDISC     LIKE joweREC_ROW.IS_MANDISC      
/* 81 CC */ FIELD IS_DISC_P      LIKE joweREC_ROW.IS_DISC_P       
/* 82 CD */ FIELD IS_N_PRICE     LIKE joweREC_ROW.IS_N_PRICE      
/* 83 CE */ FIELD PRODUCT_ID     LIKE joweREC_ROW.PRODUCT_ID      
/* 84 CF */ FIELD DESCRIPT       LIKE joweREC_ROW.DESCRIPT        
/* 85 CG */ FIELD PRODGR_ID      LIKE joweREC_ROW.PRODGR_ID       
/* 86 CH */ FIELD ORIGIN_ID      LIKE joweREC_ROW.ORIGIN_ID       
/* 87 CI */ FIELD PROD_CLASS     LIKE joweREC_ROW.PROD_CLASS      
/* 88 CJ */ FIELD DISC_CUST      LIKE joweREC_ROW.DISC_CUST       
/* 89 CK */ FIELD DISC_EMPL      LIKE joweREC_ROW.DISC_EMPL       
/* 90 CL */ FIELD DISC_ACT       LIKE joweREC_ROW.DISC_ACT        
/* 91 CM */ FIELD CRED_QTY       LIKE joweREC_ROW.CRED_QTY        
/* 92 CN */ FIELD CUSTORD_NO     LIKE joweREC_ROW.CUSTORD_NO      
/* 93 CO */ FIELD CORDROW_NO     LIKE joweREC_ROW.CORDROW_NO      
/* 94 CP */ FIELD REPORT_CODE    LIKE joweREC_ROW.REPORT_CODE     
/* 95 CQ */ FIELD STAT_CODE      LIKE joweREC_ROW.STAT_CODE       
/* 96 CR */ FIELD IS_PRESOLD     LIKE joweREC_ROW.IS_PRESOLD      
/* 97 CS */ FIELD L_OUR_NO         LIKE joweREC_ROW.OUR_NO          
/* 98 CT */ FIELD UnitPriceWoAct LIKE joweREC_ROW.UnitPriceWoAct  
/* 99 CU */ FIELD DiscTypeId     LIKE joweREC_ROW.DiscTypeId      
/*100 CV */ FIELD PAYTYPE_ID     LIKE joweREC_PAY.PAYTYPE_ID      
/*101 CW */ FIELD PAYMENT_ID     LIKE joweREC_PAY.PAYMENT_ID      
/*102 CX */ FIELD RATE_OUT       LIKE joweREC_PAY.RATE_OUT        
/*103 CY */ FIELD P_AMOUNT         LIKE joweREC_PAY.AMOUNT          
/*104 CZ */ FIELD AMOUNT_ORG     LIKE joweREC_PAY.AMOUNT_ORG      
/*105 DA */ FIELD PAIDAMOUNT     LIKE joweREC_PAY.PAIDAMOUNT      
/*106 DB */ FIELD AMOUNT_RET     LIKE joweREC_PAY.AMOUNT_RET
/*107 DC */ FIELD OriginalData   AS   CHAR 
/*108 DD */ FIELD TTId           AS   INT
/*109 DE */ FIELD TbId           AS   INT
/*110 DF */ FIELD POS            AS   CHAR
/*111 DG */ FIELD BongRecid      AS   RECID
INDEX Receipt_no IS PRIMARY ACT_DATE ACT_TIME RECEIPT_NO.

DEF NEW SHARED TEMP-TABLE tt_Hode NO-UNDO
   /* 10    J */ FIELD ACT_DATE       LIKE joweRECEIPT.ACT_DATE   
   /* 10    J */ FIELD ACT_TIME       LIKE joweRECEIPT.ACT_TIME   
   /* 10    J */ FIELD RECEIPT_NO     LIKE joweRECEIPT.RECEIPT_NO 
   /* 10    J */ FIELD iSlKort        AS   INT
   /* 10    J */ FIELD iKvitto_seq    AS   DEC
    INDEX Receipt_no IS PRIMARY ACT_DATE ACT_TIME RECEIPT_NO.

/* ** DET UNDER HER SKAL SLETTES ** */
DEF NEW SHARED TEMP-TABLE ldPost NO-UNDO
   /* 10    J */ FIELD recno        AS CHAR  FORMAT "X(6)" 
   /* 10    J */ FIELD logid        AS CHAR  FORMAT "X(3)" 
   /* 10    J */ FIELD tno          AS CHAR  FORMAT "X(3)" 
   /* 10    J */ FIELD oprno        AS CHAR  FORMAT "X(4)" 
   /* 10    J */ FIELD receipt      AS CHAR  FORMAT "X(7)" 
   /* 10    J */ FIELD transDate    AS CHAR  FORMAT "X(8)" 
   /* 10    J */ FIELD transTime    AS CHAR  FORMAT "X(6)" 
   /* 10    J */ FIELD n1           AS CHAR  FORMAT "X(20)"
   /* 10    J */ FIELD t1           AS CHAR  FORMAT "X(20)"
   /* 10    J */ FIELD n2           AS CHAR  FORMAT "X(3)" 
   /* 10    J */ FIELD n3           AS CHAR  FORMAT "X(6)"
   /* 10    J */ FIELD amount       AS DECIMAL
   /* 10    J */ FIELD amount2      AS DECIMAL
   /* 10    J */ FIELD quantity     AS DECIMAL
   /* 10    J */ FIELD t2           AS CHAR  FORMAT "X(7)" 
   /* 10    J */ FIELD tag          AS CHAR  FORMAT "X(1)"
   /* 10    J */ FIELD TTId         AS INT   FORMAT "999"
   /* 10    J */ FIELD TBId         AS INT   FORMAT "99"
   /* 10    J */ FIELD Tid          AS INT   
   /* 10    J */ FIELD OriginalData AS CHAR 
    INDEX tnoRec    IS PRIMARY tno receipt 
    INDEX Kvitto    tno receipt recno.

DEF NEW SHARED TEMP-TABLE ttKvittonr NO-UNDO
   /* 10    J */ FIELD tno        AS CHAR
   /* 10    J */ FIELD knr        AS CHAR
   /* 10    J */ FIELD kvitto_seq AS INTEGER
    INDEX tnoRec IS PRIMARY tno knr.

DEF NEW SHARED TEMP-TABLE ttStarReq NO-UNDO
   /* 10    J */ FIELD ean      AS CHAR
   /* 10    J */ FIELD nyttEan  AS CHAR
   /* 10    J */ FIELD kp       AS DECIMAL
   /* 10    J */ FIELD fp       AS DECIMAL
   /* 10    J */ FIELD mediakod AS CHAR
   /* 10    J */ FIELD datum    AS DATE
    INDEX ean IS PRIMARY ean.

/* TEMP-TABLE ttKvitto */.
{kvitto.i "new shared" "no-undo"}
/* TEMP-TABLE ttKvittoRad */
{kvittorad.i "new shared" "no-undo"}
