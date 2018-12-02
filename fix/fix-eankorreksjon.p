DEF VAR iCount1 AS INTE NO-UNDO.
DEF VAR iMulti  AS INTE INIT 1 NO-UNDO.
DEF VAR iSum    AS INTE NO-UNDO.
DEF VAR cKode   AS CHAR NO-UNDO.
DEFINE VARIABLE lByt AS LOGICAL    NO-UNDO.
/* lByt = TRUE. */
FOR each strekkode where length(kode) > 7 and length(kode) < 13.
    find artbas of strekkode no-lock.
    find levbas of artbas no-lock.
      iMulti = 1.
      iSum   = 0.
      cKode = fill("0",13 - LENGTH(Strekkode.kode)) + strekkode.kode.
      cKode = SUBSTR(ckode,1,12).
      DO iCount1 = LENGTH(cKode) TO 1 BY -1:  
          ASSIGN iMulti = IF iMulti = 1 THEN 3 ELSE 1
                 iSum = iSum + INT(SUBSTR(cKode,iCount1,1)) * iMulti.
      END.
      ckode =  cKode + string((10 - iSum MODULO 10) MODULO 10).
      IF lByt = FALSE THEN
          disp strekkode.artikkelnr kode ckode format "x(15)" 
          fill("0",13 - length(kode)) + kode = cKode.
      ELSE IF fill("0",13 - length(kode)) + kode = cKode THEN
          strekkode.kode = cKode no-error.
          if error-status:error then
          delete strekkode.
/*           DISP fill("0",13 - length(kode)) + kode = cKode. */
end.
FOR EACH strekkode WHERE LENGTH(kode) > 5 AND LENGTH(kode) <> 13.
    DISP strekkode.artikkelnr strekkode.kode length(kode).
/* delete strekkode. */
END.
