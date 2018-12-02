FUNCTION EAN13BC RETURN CHARACTER
  ( INPUT icStrekKode AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEFINE VARIABLE cBarCode AS CHARACTER EXTENT 14 NO-UNDO.
DEFINE VARIABLE cBCString AS CHARACTER NO-UNDO.
DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.

IF LENGTH(icStrekKode) < 13 THEN
  icStrekKode = FILL("0",13 - LENGTH(icStrekKode)) + icStrekKode.
ELSE IF LENGTH(icStrekKode) > 13 THEN
  RETURN "".

ASSIGN cBarCode[1] = CHR(ASC(SUBSTR(icStrekKode,1,1)) - 15) 
       cBarCode[2] = CHR(ASC(SUBSTR(icStrekKode,2,1)) + 48)
       cBarCode[8] = CHR(124).

ASSIGN cBarcode[3] = SUBSTR(icStrekKode,3,1)
       cBarcode[4] = SUBSTR(icStrekKode,4,1)
       cBarcode[5] = SUBSTR(icStrekKode,5,1)
       cBarcode[6] = SUBSTR(icStrekKode,6,1)
       cBarcode[7] = SUBSTR(icStrekKode,7,1).

CASE SUBSTR(icStrekKode,1,1):
    WHEN "1" THEN
        ASSIGN cBarcode[4] = CHR(ASC(cBarcode[4]) + 16)
               cBarcode[6] = CHR(ASC(cBarcode[6]) + 16)
               cBarcode[7] = CHR(ASC(cBarcode[7]) + 16).
    WHEN "2" THEN
        ASSIGN cBarcode[4] = CHR(ASC(cBarcode[4]) + 16)
               cBarcode[5] = CHR(ASC(cBarcode[5]) + 16)
               cBarcode[7] = CHR(ASC(cBarcode[7]) + 16).
    WHEN "3" THEN
        ASSIGN cBarcode[4] = CHR(ASC(cBarcode[4]) + 16)
               cBarcode[5] = CHR(ASC(cBarcode[5]) + 16)
               cBarcode[6] = CHR(ASC(cBarcode[6]) + 16).
    WHEN "4" THEN
        ASSIGN cBarcode[3] = CHR(ASC(cBarcode[3]) + 16)
               cBarcode[6] = CHR(ASC(cBarcode[6]) + 16)
               cBarcode[7] = CHR(ASC(cBarcode[7]) + 16).
    WHEN "5" THEN
        ASSIGN cBarcode[3] = CHR(ASC(cBarcode[3]) + 16)
               cBarcode[4] = CHR(ASC(cBarcode[4]) + 16)
               cBarcode[7] = CHR(ASC(cBarcode[7]) + 16).
    WHEN "6" THEN
        ASSIGN cBarcode[3] = CHR(ASC(cBarcode[3]) + 16)
               cBarcode[4] = CHR(ASC(cBarcode[4]) + 16)
               cBarcode[5] = CHR(ASC(cBarcode[5]) + 16).
    WHEN "7" THEN
        ASSIGN cBarcode[3] = CHR(ASC(cBarcode[3]) + 16)
               cBarcode[5] = CHR(ASC(cBarcode[5]) + 16)
               cBarcode[7] = CHR(ASC(cBarcode[7]) + 16).
    WHEN "8" THEN
        ASSIGN cBarcode[3] = CHR(ASC(cBarcode[3]) + 16)
               cBarcode[5] = CHR(ASC(cBarcode[5]) + 16)
               cBarcode[6] = CHR(ASC(cBarcode[6]) + 16).
    WHEN "9" THEN
        ASSIGN cBarcode[3] = CHR(ASC(cBarcode[3]) + 16)
               cBarcode[4] = CHR(ASC(cBarcode[4]) + 16)
               cBarcode[6] = CHR(ASC(cBarcode[6]) + 16).
END CASE.
ASSIGN cBarcode[9] = CHR(ASC(SUBSTR(icStrekKode,8,1)) + 32) 
       cBarcode[10] = CHR(ASC(SUBSTR(icStrekKode,9,1)) + 32) 
       cBarcode[11] = CHR(ASC(SUBSTR(icStrekKode,10,1)) + 32) 
       cBarcode[12] = CHR(ASC(SUBSTR(icStrekKode,11,1)) + 32) 
       cBarcode[13] = CHR(ASC(SUBSTR(icStrekKode,12,1)) + 32).
       

cBarcode[14] = CHR(ASC(SUBSTR(icStrekKode,13,1)) + 64).
DO iCount = 1 TO 14:
    cBCString = cBCString + cBarCode[iCount] .
END.

IF cBCstring = ? THEN cBCstring = "".

RETURN cBCstring. 

END FUNCTION.
