/*
Procedure:  BarCode128AOr128BStringConvert.p
Purpose:  Converts a character string to a 128A or a 128B barcode string.
Input  :  ipcBarCodeType = "A" for barcode type 128A and "B" barcode type 128B.
          ipcInputString = The character string to be converted to barcode string.
Output :  
Syntax:  RUN BarCode128AOr128BStringConvert.p(INPUT ipcBarCodeType, INPUT ipcInputString, OUTPUT opcReturnedString).
*/

DEFINE INPUT PARAMETER  cBarCodeType     AS CHARACTER   NO-UNDO.
DEFINE INPUT PARAMETER  cInputString     AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER cReturnedString  AS CHARACTER NO-UNDO.

DEFINE VARIABLE cCurrentPair AS CHARACTER NO-UNDO.
DEFINE VARIABLE iCurrentAsciiValue AS INTEGER     NO-UNDO.
DEFINE VARIABLE cStartString    AS CHARACTER NO-UNDO.
DEFINE VARIABLE iCheckSumValue       AS INTEGER   NO-UNDO.
DEFINE VARIABLE iRunningTotal       AS INTEGER   NO-UNDO.
DEFINE VARIABLE cCheckSumCharacter AS CHARACTER NO-UNDO.
DEFINE VARIABLE iCounter        AS INTEGER   NO-UNDO.
DEFINE VARIABLE cNewString AS CHARACTER   NO-UNDO.
DEFINE VARIABLE ii AS INTEGER     NO-UNDO.
DEFINE VARIABLE cChr AS CHARACTER   NO-UNDO.
/* Initialize source and target strings */
ASSIGN
    cReturnedString = ""
    cInputString  = TRIM(cInputString).

/* Barcode type is assumed to be 128B if not 128A */
/* Initialize the starting value and start string */
IF cBarCodeType = "C" THEN
    ASSIGN
        iRunningTotal  = 105
        cStartString = CHR(205).
ELSE
    RETURN.
IF LENGTH(cInputString) <> 22 THEN
    RETURN.

/* Calculate the checksum, mod 103 and build output string */
DO iCounter = 1 TO 11:
    cCurrentPair = SUBSTRING(cInputString, iCounter * 2 - 1 , 2).
    /* Update the checksum running total */
    cChr = CHR(INT(cCurrentPair) + 32 + (IF INT(cCurrentPair) > 94 THEN 145 ELSE 0)).
    iRunningTotal = iRunningTotal + (INT(cCurrentPair) * iCounter).
    cNewString = cNewString + cChr.
    
END.

iCheckSumValue = iRunningTotal MODULO 103.

/* IF iCheckSumValue GT 90 THEN                           */
/*     cCheckSumCharacter = CHR(iCheckSumValue + 103).    */
/* ELSE                                                   */
/*     IF iCheckSumValue GT 0 THEN                        */
/*         cCheckSumCharacter = CHR(iCheckSumValue + 32). */
/*     ELSE                                               */
/*         cCheckSumCharacter = CHR(228).                 */
cCheckSumCharacter = CHR(iCheckSumValue + 32 + (IF iCheckSumValue > 94 THEN 145 ELSE 0)) .
ASSIGN
    cReturnedString = cStartString + cNewString + cCheckSumCharacter  + CHR(206).
/*     cReturnedString = cStartString + cInputString + cCheckSumCharacter  + CHR(126) + CHR(32). */
/* cReturnedString = "Í,BXnOÎ". */

OUTPUT TO "CLIPBOARD".
PUT UNFORMATTED cReturnedString SKIP.
OUTPUT CLOSE.

MESSAGE cReturnedString SKIP iCheckSumValue
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
