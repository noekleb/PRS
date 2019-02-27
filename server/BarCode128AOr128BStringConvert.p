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

DEFINE VARIABLE cCurrentCharacter AS CHARACTER NO-UNDO.
DEFINE VARIABLE iCurrentAsciiValue AS INTEGER     NO-UNDO.
DEFINE VARIABLE cStartString    AS CHARACTER NO-UNDO.
DEFINE VARIABLE iCheckSumValue       AS INTEGER   NO-UNDO.
DEFINE VARIABLE iRunningTotal       AS INTEGER   NO-UNDO.
DEFINE VARIABLE cCheckSumCharacter AS CHARACTER NO-UNDO.
DEFINE VARIABLE iCounter        AS INTEGER   NO-UNDO.

/* Initialize source and target strings */
ASSIGN
    cReturnedString = ""
    cInputString  = TRIM(cInputString).

/* Barcode type is assumed to be 128B if not 128A */
/* Initialize the starting value and start string */
IF cBarCodeType = "A" THEN
    ASSIGN
        iRunningTotal  = 103
        cStartString = CHR(123).
ELSE
    ASSIGN
        iRunningTotal  = 104
        cStartString = CHR(124).

/* Calculate the checksum, mod 103 and build output string */
DO iCounter = 1 TO LENGTH(cInputString):
    cCurrentCharacter = SUBSTRING(cInputString, iCounter, 1).
    /* get the ASCiCounter value of the current character */
    iCurrentAsciiValue = ASC(cCurrentCharacter).
    /* get the barcode 128 value of the current character */
    IF iCurrentAsciiValue < 127 THEN
        iCurrentAsciiValue = iCurrentAsciiValue - 32.
    ELSE
        iCurrentAsciiValue = iCurrentAsciiValue - 103.
    /* Update the checksum running total */
    iRunningTotal = iRunningTotal + iCurrentAsciiValue * iCounter.
    
    /*Compute output string, no spaces in TrueType fonts, quotes replaced for Word mailmerge bug */
    CASE cCurrentCharacter:
        WHEN CHR(32) THEN
            cReturnedString = cReturnedString + CHR(228).
        WHEN CHR(34) THEN
            cReturnedString = cReturnedString + CHR(226).
        WHEN CHR(123) THEN
            cReturnedString = cReturnedString + CHR(194).
        WHEN CHR(124) THEN
            cReturnedString = cReturnedString + CHR(195).
        WHEN CHR(125) THEN
            cReturnedString = cReturnedString + CHR(196).
        WHEN CHR(126) THEN
            cReturnedString = cReturnedString + CHR(197).
        OTHERWISE
            cReturnedString = cReturnedString + cCurrentCharacter.
    END CASE.
END.

iCheckSumValue = iRunningTotal MODULO 103.
IF iCheckSumValue GT 90 THEN
    cCheckSumCharacter = CHR(iCheckSumValue + 103).
ELSE
    IF iCheckSumValue GT 0 THEN
        cCheckSumCharacter = CHR(iCheckSumValue + 32).
    ELSE
        cCheckSumCharacter = CHR(228).
ASSIGN
    cReturnedString = cStartString + cReturnedString + cCheckSumCharacter  + CHR(126) + CHR(32).
