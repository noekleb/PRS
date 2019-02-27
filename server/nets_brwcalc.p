/*
PROCEDURE kundekort_kortnr:
  DEF INPUT PARAM  icKortNr      AS CHAR  NO-UNDO.
  DEF INPUT PARAM  icSessionId   AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocReturn      AS CHAR  NO-UNDO.

  ocReturn = icKortNr.
END PROCEDURE.
*/

PROCEDURE SendtTid_Kl:
  DEF INPUT PARAM  iiSendtTid    AS INTEGER NO-UNDO.
  DEF INPUT PARAM  icSessionId   AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocReturn      AS CHAR  NO-UNDO.

  ocReturn = STRING(iiSendtTid,"HH:MM:SS").

END PROCEDURE.

/*
PROCEDURE medlemskort_ean:
  DEF INPUT PARAM  icKortNr      AS CHAR  NO-UNDO.
  DEF INPUT PARAM  icSessionId   AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocReturn      AS CHAR  NO-UNDO.
  DEFINE VAR cEAN         AS CHAR FORMAT "x(13)" NO-UNDO. 
  DEF VAR iCount1 AS INTE NO-UNDO.
  DEF VAR iMulti  AS INTE INIT 1 NO-UNDO.
  DEF VAR iSum AS INTE NO-UNDO.

  IF DECI(icKortnr) <> 0 THEN 
  BLOKKEN:
  DO:      
      FIND FIRST MedlemsKort NO-LOCK WHERE
        MedlemsKort.InterntKKortId = dec(icKortNr) NO-ERROR.
      IF AVAILABLE MedlemsKort THEN
          icKortNr = MedlemsKort.KortNr.
      ELSE DO: 
          icKortNr = ''.
          LEAVE BLOKKEN.
      END.
      
      icKortNr = "29" + FILL("0",10 - LENGTH(icKortNr)) + icKortNr.

      DO iCount1 = LENGTH(icKortNr) TO 1 BY -1:  
          ASSIGN iMulti = IF iMulti = 1 THEN 3 ELSE 1
                 iSum = iSum + INT(SUBSTR(icKortNr,iCount1,1)) * iMulti.
      END.
      icKortNr = icKortNr + string((10 - iSum MODULO 10) MODULO 10).
  END. /* BLOKKEN */
  ELSE icKortNr = ''.

  ocReturn = icKortNr.
END PROCEDURE.
*/
