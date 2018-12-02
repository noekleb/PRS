/* ChkEAN.p */
/* Denne rutinen benyttes når man bare skal kontrollere sjekksifferet. Ikke for å kontrolelre 20 koder. */

DEF INPUT PARAMETER cKode AS CHAR NO-UNDO.

DEF VAR iCount1 AS INTE NO-UNDO.
DEF VAR iMulti  AS INTE INIT 1 NO-UNDO.
DEF VAR iSum AS INTE NO-UNDO.

DO iCount1 = LENGTH(cKode) TO 1 BY -1:  
    ASSIGN iMulti = IF iMulti = 1 THEN 3 ELSE 1
           iSum = iSum + INT(SUBSTR(cKode,iCount1,1)) * iMulti.
END.
RETURN cKode + string((10 - iSum MODULO 10) MODULO 10).

