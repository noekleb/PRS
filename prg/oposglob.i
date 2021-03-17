&GLOBAL-DEFINE OPOS                       100
&GLOBAL-DEFINE UPOS                       101


/*OPOS "State" Property Constants*/
&GLOBAL-DEFINE OPOS_S_CLOSED 		      1
&GLOBAL-DEFINE OPOS_S_IDLE   		      2
&GLOBAL-DEFINE OPOS_S_BUSY   		      3
&GLOBAL-DEFINE OPOS_S_ERROR  		      4

/* OPOS "ResultCode" Property Constants */ 
&GLOBAL-DEFINE OPOSERR    		          100
&GLOBAL-DEFINE OPOSERREXT 		          200

&GLOBAL-DEFINE OPOS_SUCCESS     	      0
&GLOBAL-DEFINE OPOS_E_CLOSED     	      1 + {&OPOSERR}
&GLOBAL-DEFINE OPOS_E_CLAIMED    	      2 + {&OPOSERR}
&GLOBAL-DEFINE OPOS_E_NOTCLAIMED 	      3 + {&OPOSERR}
&GLOBAL-DEFINE OPOS_E_NOSERVICE  	      4 + {&OPOSERR}
&GLOBAL-DEFINE OPOS_E_DISABLED      	  5 + {&OPOSERR}
&GLOBAL-DEFINE OPOS_E_ILLEGAL       	  6 + {&OPOSERR}
&GLOBAL-DEFINE OPOS_E_NOHARDWARE    	  7 + {&OPOSERR}
&GLOBAL-DEFINE OPOS_E_OFFLINE       	  8 + {&OPOSERR}
&GLOBAL-DEFINE OPOS_E_NOEXIST       	  9 + {&OPOSERR}
&GLOBAL-DEFINE OPOS_E_EXISTS        	  10 + {&OPOSERR}
&GLOBAL-DEFINE OPOS_E_FAILURE       	  11 + {&OPOSERR}
&GLOBAL-DEFINE OPOS_E_TIMEOUT       	  12 + {&OPOSERR}
&GLOBAL-DEFINE OPOS_E_BUSY          	  13 + {&OPOSERR}
&GLOBAL-DEFINE OPOS_E_EXTENDED      	  14 + {&OPOSERR}

/* OPOS "BinaryConversion" Property Constants */
&GLOBAL-DEFINE OPOS_BC_NONE         	  0
&GLOBAL-DEFINE OPOS_BC_NIBBLE       	  1
&GLOBAL-DEFINE OPOS_BC_DECIMAL      	  2

/* "CheckHealth" Method: "Level" Parameter Constants */
&GLOBAL-DEFINE OPOS_CH_INTERNAL     	  1
&GLOBAL-DEFINE OPOS_CH_EXTERNAL     	  2
&GLOBAL-DEFINE OPOS_CH_INTERACTIVE  	  3


/* "ErrorEvent" Event: "ErrorLocus" Parameter Constants */
&GLOBAL-DEFINE OPOS_EL_OUTPUT       	  1
&GLOBAL-DEFINE OPOS_EL_INPUT        	  2
&GLOBAL-DEFINE OPOS_EL_INPUT_DATA   	  3

/* "ErrorEvent" Event: "ErrorResponse" Constants */
&GLOBAL-DEFINE OPOS_ER_RETRY        	  11
&GLOBAL-DEFINE OPOS_ER_CLEAR        	  12
&GLOBAL-DEFINE OPOS_ER_CONTINUEINPUT	  13

/* General Constants */
&GLOBAL-DEFINE OPOS_FOREVER         	  -1

/***********************
* CASHDRAWER CONSTANTS *
***********************/ 

/* "StatusUpdateEvent" Event Constants */
&GLOBAL-DEFINE OPOS_CASH_SUE_DRAWERCLOSED 0
&GLOBAL-DEFINE OPOS_CASH_SUE_DRAWEROPEN   1


/********************
* KEYLOCK CONSTANTS *
********************/ 
/* "KeyPosition" Property Constants */
/* "WaitForKeylockChange" Method: "KeyPosition" Parameter */
/* "StatusUpdateEvent" Event: "Data" Parameter */
&GLOBAL-DEFINE OPOS_LOCK_KP_ANY           0    /* WaitForKeylockChange Only */
&GLOBAL-DEFINE OPOS_LOCK_KP_LOCK          1
&GLOBAL-DEFINE OPOS_LOCK_KP_NORM          2
&GLOBAL-DEFINE OPOS_LOCK_KP_SUPR          3

/*****************************
* CUSTOMER DISPLAY CONSTANTS *
*****************************/ 

/* "CapBlink" Property Constants */
&GLOBAL-DEFINE OPOS_DISP_CB_NOBLINK       0
&GLOBAL-DEFINE OPOS_DISP_CB_BLINKALL      1
&GLOBAL-DEFINE OPOS_DISP_CB_BLINKEACH     2

/* "CapCharacterSet" Property Constants */
&GLOBAL-DEFINE OPOS_DISP_CCS_NUMERIC      0
&GLOBAL-DEFINE OPOS_DISP_CCS_ALPHA        1
&GLOBAL-DEFINE OPOS_DISP_CCS_ASCII        998
&GLOBAL-DEFINE OPOS_DISP_CCS_KANA         10
&GLOBAL-DEFINE OPOS_DISP_CCS_KANJI        11

/* "CharacterSet" Property Constants */
&GLOBAL-DEFINE OPOS_DISP_CS_ASCII         998
&GLOBAL-DEFINE OPOS_DISP_CS_WINDOWS       999

/* "MarqueeType" Property Constants */
&GLOBAL-DEFINE OPOS_DISP_MT_NONE          0
&GLOBAL-DEFINE OPOS_DISP_MT_UP            1
&GLOBAL-DEFINE OPOS_DISP_MT_DOWN          2
&GLOBAL-DEFINE OPOS_DISP_MT_LEFT          3
&GLOBAL-DEFINE OPOS_DISP_MT_RIGHT         4
&GLOBAL-DEFINE OPOS_DISP_MT_INIT          5

/* "MarqueeFormat" Property Constants */
&GLOBAL-DEFINE OPOS_DISP_MF_WALK          0
&GLOBAL-DEFINE OPOS_DISP_MF_PLACE         1

/* "DisplayText" Method: "Attribute" Property Constants */
/* "DisplayTextAt" Method: "Attribute" Property Constants */
&GLOBAL-DEFINE OPOS_DISP_DT_NORMAL        0
&GLOBAL-DEFINE OPOS_DISP_DT_BLINK         1

/* "ScrollText" Method: "Direction" Parameter Constants */
&GLOBAL-DEFINE OPOS_DISP_ST_UP            1
&GLOBAL-DEFINE OPOS_DISP_ST_DOWN          2
&GLOBAL-DEFINE OPOS_DISP_ST_LEFT          3
&GLOBAL-DEFINE OPOS_DISP_ST_RIGHT         4

/* "SetDescriptor" Method: "Attribute" Parameter Constants */
&GLOBAL-DEFINE OPOS_DISP_SD_OFF           0
&GLOBAL-DEFINE OPOS_DISP_SD_ON            1
&GLOBAL-DEFINE OPOS_DISP_SD_BLINK         2

/***************************************************
* MICR - Magnetic Ink Character Recognition Reader *
***************************************************/
/* "CheckType" Property Constants */
&GLOBAL-DEFINE OPOS_MICR_CT_PERSONAL       1
&GLOBAL-DEFINE OPOS_MICR_CT_BUSINESS       2
&GLOBAL-DEFINE OPOS_MICR_CT_UNKNOWN       99

/* "CountryCode" Property Constants */
&GLOBAL-DEFINE OPOS_MICR_CC_USA            1
&GLOBAL-DEFINE OPOS_MICR_CC_CANADA         2
&GLOBAL-DEFINE OPOS_MICR_CC_MEXICO         3
&GLOBAL-DEFINE OPOS_MICR_CC_UNKNOWN       99

/* "ResultCodeExtended" Property Constants for MICR */
&GLOBAL-DEFINE OPOS_OPOS_EMICR_NOCHECK    1 + OPOSERREXT /* EndInsertion */
&GLOBAL-DEFINE OPOS_OPOS_EMICR_CHECK      2 + OPOSERREXT /* EndRemoval */

/********************************
* MSR -  Magnetic Stripe Reader *
********************************/
/* "TracksToRead" Property Constants */
&GLOBAL-DEFINE OPOS_MSR_TR_1              1
&GLOBAL-DEFINE OPOS_MSR_TR_2              2
&GLOBAL-DEFINE OPOS_MSR_TR_3              4
&GLOBAL-DEFINE OPOS_MSR_TR_1_2            MSR_TR_1 + MSR_TR_2
&GLOBAL-DEFINE OPOS_MSR_TR_1_3            MSR_TR_1 + MSR_TR_3
&GLOBAL-DEFINE OPOS_MSR_TR_2_3            MSR_TR_2 + MSR_TR_3
&GLOBAL-DEFINE OPOS_MSR_TR_1_2_3          MSR_TR_1 + MSR_TR_2 + MSR_TR_3

/* "ErrorReportingType" Property Constants */
&GLOBAL-DEFINE OPOS_MSR_ERT_CARD          0
&GLOBAL-DEFINE OPOS_MSR_ERT_TRACK         1

/* "ErrorEvent" Event: "ResultCodeExtended" Parameter Constants */
&GLOBAL-DEFINE OPOS_OPOS_EMSR_START       1 + OPOSERREXT
&GLOBAL-DEFINE OPOS_OPOS_EMSR_END         2 + OPOSERREXT
&GLOBAL-DEFINE OPOS_OPOS_EMSR_PARITY      3 + OPOSERREXT
&GLOBAL-DEFINE OPOS_OPOS_EMSR_LRC         4 + OPOSERREXT


/********************
* PR -  POS Printer *
********************/
/* Printer Station Constants */
&GLOBAL-DEFINE OPOS_PTR_S_JOURNAL           1
&GLOBAL-DEFINE OPOS_PTR_S_RECEIPT           2
&GLOBAL-DEFINE OPOS_PTR_S_SLIP              4

&GLOBAL-DEFINE OPOS_PTR_S_JOURNAL_RECEIPT   PTR_S_JOURNAL + PTR_S_RECEIPT 
&GLOBAL-DEFINE OPOS_PTR_S_JOURNAL_SLIP      PTR_S_JOURNAL + PTR_S_SLIP    
&GLOBAL-DEFINE OPOS_PTR_S_RECEIPT_SLIP      PTR_S_RECEIPT + PTR_S_SLIP    

&GLOBAL-DEFINE OPOS_PTR_TWO_RECEIPT_JOURNAL 32768 + PTR_S_JOURNAL_RECEIPT
&GLOBAL-DEFINE OPOS_PTR_TWO_SLIP_JOURNAL    32768 + PTR_S_JOURNAL_SLIP   
&GLOBAL-DEFINE OPOS_PTR_TWO_SLIP_RECEIPT    32768 + PTR_S_RECEIPT_SLIP   

/* "CapCharacterSet" Property Constants */
&GLOBAL-DEFINE OPOS_PTR_CCS_ALPHA           1
&GLOBAL-DEFINE OPOS_PTR_CCS_ASCII           998
&GLOBAL-DEFINE OPOS_PTR_CCS_KANA            10
&GLOBAL-DEFINE OPOS_PTR_CCS_KANJI           11

/* "CharacterSet" Property Constants */
&GLOBAL-DEFINE OPOS_PTR_CS_ASCII            998
&GLOBAL-DEFINE OPOS_PTR_CS_WINDOWS          999

/* "ErrorLevel" Property Constants */
&GLOBAL-DEFINE OPOS_PTR_EL_NONE             1
&GLOBAL-DEFINE OPOS_PTR_EL_RECOVERABLE      2
&GLOBAL-DEFINE OPOS_PTR_EL_FATAL            3

/* "MapMode" Property Constants */
&GLOBAL-DEFINE OPOS_PTR_MM_DOTS             1
&GLOBAL-DEFINE OPOS_PTR_MM_TWIPS            2
&GLOBAL-DEFINE OPOS_PTR_MM_ENGLISH          3
&GLOBAL-DEFINE OPOS_PTR_MM_METRIC           4

/* "CutPaper" Method Constant */
&GLOBAL-DEFINE OPOS_PTR_CP_FULLCUT          100

/* "PrintBarCode" Method Constants: */
/*   "Alignment" Parameter */
/*     Either the distance from the left-most print column to the start */
/*     of the bar code, or one of the following: */
&GLOBAL-DEFINE OPOS_PTR_BC_LEFT             -1
&GLOBAL-DEFINE OPOS_PTR_BC_CENTER           -2
&GLOBAL-DEFINE OPOS_PTR_BC_RIGHT            -3

/*   "TextPosition" Parameter */
&GLOBAL-DEFINE OPOS_PTR_BC_TEXT_NONE        -11
&GLOBAL-DEFINE OPOS_PTR_BC_TEXT_ABOVE       -12
&GLOBAL-DEFINE OPOS_PTR_BC_TEXT_BELOW       -13

/*   "Symbology" Parameter: */
/*    One dimensional symbologies */
&GLOBAL-DEFINE OPOS_PTR_BCS_UPCA            101  /* Digits */
&GLOBAL-DEFINE OPOS_PTR_BCS_UPCE            102  /* Digits */
&GLOBAL-DEFINE OPOS_PTR_BCS_JAN8            103  /* = EAN 8 */
&GLOBAL-DEFINE OPOS_PTR_BCS_EAN8            103  /* = JAN 8 (added in 1.2) */
&GLOBAL-DEFINE OPOS_PTR_BCS_JAN13           104  /* = EAN 13 */
&GLOBAL-DEFINE OPOS_PTR_BCS_EAN13           104  /* = JAN 13 (added in 1.2) */
&GLOBAL-DEFINE OPOS_PTR_BCS_TF              105  /* (Discrete 2 of 5) Digits */
&GLOBAL-DEFINE OPOS_PTR_BCS_ITF             106  /* (Interleaved 2 of 5) Digits */
&GLOBAL-DEFINE OPOS_PTR_BCS_Codabar         107  /* Digits, -, $, :, /, ., +; */
                                                 /*   4 start/stop characters */
                                                 /*   (a, b, c, d) */
&GLOBAL-DEFINE OPOS_PTR_BCS_Code39          108  /* Alpha, Digits, Space, -, ., */
                                                 /*   $, /, +, %; start/stop (*) */
                                                 /* Also has Full ASCII feature */
&GLOBAL-DEFINE OPOS_PTR_BCS_Code93          109  /* Same characters as Code 39 */
&GLOBAL-DEFINE OPOS_PTR_BCS_Code128         110  /* 128 data characters */
/*        (The following were added in Release 1.2) */
&GLOBAL-DEFINE OPOS_PTR_BCS_UPCA_S          111  /* UPC-A with supplemental barcode */
&GLOBAL-DEFINE OPOS_PTR_BCS_UPCE_S          112  /* UPC-E with supplemental barcode */
&GLOBAL-DEFINE OPOS_PTR_BCS_UPCD1           113  /* UPC-D1 */
&GLOBAL-DEFINE OPOS_PTR_BCS_UPCD2           114  /* UPC-D2 */
&GLOBAL-DEFINE OPOS_PTR_BCS_UPCD3           115  /* UPC-D3 */
&GLOBAL-DEFINE OPOS_PTR_BCS_UPCD4           116  /* UPC-D4 */
&GLOBAL-DEFINE OPOS_PTR_BCS_UPCD5           117  /* UPC-D5 */
&GLOBAL-DEFINE OPOS_PTR_BCS_EAN8_S          118  /* EAN 8 with supplemental barcode */
&GLOBAL-DEFINE OPOS_PTR_BCS_EAN13_S         119  /* EAN 13 with supplemental barcode */
                                        
&GLOBAL-DEFINE OPOS_PTR_BCS_EAN128          120  /* EAN 128 */
&GLOBAL-DEFINE OPOS_PTR_BCS_OCRA            121  /* OCR "A" */
&GLOBAL-DEFINE OPOS_PTR_BCS_OCRB            122  /* OCR "B" */


/*     Two dimensional symbologies */
&GLOBAL-DEFINE OPOS_PTR_BCS_PDF417          201
&GLOBAL-DEFINE OPOS_PTR_BCS_MAXICODE        202

/*     Start of Printer-Specific bar code symbologies */
&GLOBAL-DEFINE OPOS_PTR_BCS_OTHER           501


/* "PrintBitmap" Method Constants: */
/*   "Width" Parameter */
/*     Either bitmap width or: */
&GLOBAL-DEFINE OPOS_PTR_BM_ASIS             -11  /* One pixel per printer dot */

/*   "Alignment" Parameter */
/*     Either the distance from the left-most print column to the start */
/*     of the bitmap, or one of the following: */
&GLOBAL-DEFINE OPOS_PTR_BM_LEFT             -1
&GLOBAL-DEFINE OPOS_PTR_BM_CENTER           -2
&GLOBAL-DEFINE OPOS_PTR_BM_RIGHT            -3

/* "RotatePrint" Method: "Rotation" Parameter Constants */
/* "RotateSpecial" Property Constants */
&GLOBAL-DEFINE OPOS_PTR_RP_NORMAL           1
&GLOBAL-DEFINE OPOS_PTR_RP_RIGHT90          257  /* 0x0101 */
&GLOBAL-DEFINE OPOS_PTR_RP_LEFT90           258  /* 0x0102; */
&GLOBAL-DEFINE OPOS_PTR_RP_ROTATE180        259  /* 0x0103; */

/* "SetLogo" Method: "Location" Parameter Constants */
&GLOBAL-DEFINE OPOS_PTR_L_TOP               1
&GLOBAL-DEFINE OPOS_PTR_L_BOTTOM            2

/* "TransactionPrint" Method: "Control" Parameter Constants */
&GLOBAL-DEFINE OPOS_PTR_TP_TRANSACTION      11
&GLOBAL-DEFINE OPOS_PTR_TP_NORMAL           12

/* "StatusUpdateEvent" Event: "Data" Parameter Constants */
&GLOBAL-DEFINE OPOS_PTR_SUE_COVER_OPEN      11
&GLOBAL-DEFINE OPOS_PTR_SUE_COVER_OK        12
&GLOBAL-DEFINE OPOS_PTR_SUE_JRN_EMPTY       21
&GLOBAL-DEFINE OPOS_PTR_SUE_JRN_NEAREMPTY   22
&GLOBAL-DEFINE OPOS_PTR_SUE_JRN_PAPEROK     23
&GLOBAL-DEFINE OPOS_PTR_SUE_REC_EMPTY       24
&GLOBAL-DEFINE OPOS_PTR_SUE_REC_NEAREMPTY   25
&GLOBAL-DEFINE OPOS_PTR_SUE_REC_PAPEROK     26
&GLOBAL-DEFINE OPOS_PTR_SUE_SLP_EMPTY       27
&GLOBAL-DEFINE OPOS_PTR_SUE_SLP_NEAREMPTY   28
&GLOBAL-DEFINE OPOS_PTR_SUE_SLP_PAPEROK     29
&GLOBAL-DEFINE OPOS_PTR_SUE_IDLE            1001

/* "ResultCodeExtended" Property Constants for Printer */
&GLOBAL-DEFINE OPOS_OPOS_EPTR_COVER_OPEN    1 + OPOSERREXT /* (Several) */
&GLOBAL-DEFINE OPOS_OPOS_EPTR_JRN_EMPTY     2 + OPOSERREXT /* (Several) */
&GLOBAL-DEFINE OPOS_OPOS_EPTR_REC_EMPTY     3 + OPOSERREXT /* (Several) */
&GLOBAL-DEFINE OPOS_OPOS_EPTR_SLP_EMPTY     4 + OPOSERREXT /* (Several) */
&GLOBAL-DEFINE OPOS_OPOS_EPTR_SLP_FORM      5 + OPOSERREXT /* EndRemoval */
&GLOBAL-DEFINE OPOS_OPOS_EPTR_TOOBIG        6 + OPOSERREXT /* PrintBitmap */
&GLOBAL-DEFINE OPOS_OPOS_EPTR_BADFORMAT     7 + OPOSERREXT /* PrintBitmap */

/**************
* Hard Totals *
**************/
/* "ResultCodeExtended" Property Constants for Hard Totals */
&GLOBAL-DEFINE OPOS_OPOS_ETOT_NOROOM        1 + OPOSERREXT /* Create, Write */
&GLOBAL-DEFINE OPOS_OPOS_ETOT_VALIDATION    2 + OPOSERREXT /* Read, Write */

