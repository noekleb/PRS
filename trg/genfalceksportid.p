DEFINE OUTPUT PARAMETER dEksportId LIKE FalckEksport.EksportId  NO-UNDO.
DEFINE VARIABLE lLokaltHk    AS LOGICAL               NO-UNDO.
DEFINE VARIABLE strLokaltHk  AS CHARACTER             NO-UNDO.
DEFINE VARIABLE cHkNumSerier AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iCount       AS INTEGER    NO-UNDO.
DEFINE VARIABLE dFraNr       LIKE FalckEksport.EksportId NO-UNDO.
DEFINE VARIABLE dTilNr       LIKE FalckEksport.EksportId NO-UNDO.
DEFINE VARIABLE iCL          AS INTEGER NO-UNDO.

{syspara.i 5 1 1 iCL INT}

/* Ta reda på om vi har lokalt HK  */
/* Om lokalt HK skall vi gerera    */
/* artikkelnr mha systemparameter  */
{syspara.i 1 1 18 strLokaltHk}
ASSIGN lLokaltHk = strLokaltHk = "yes".
IF lLokaltHk THEN
    {syspara.i 1 1 29 cHkNumSerier}
   /*{syspara.i 1 1 23 cHkNumSerier}*/

IF lLokaltHK THEN HKLOOP: DO:
    DO iCount = 1 TO NUM-ENTRIES(cHkNumSerier):
        ASSIGN dFraNr = DECI(ENTRY(1,ENTRY(iCount,cHkNumSerier),"-"))
               dTilNr = DECI(ENTRY(2,ENTRY(iCount,cHkNumSerier),"-")).
        FIND LAST FalckEksport WHERE 
          FalckEksport.ButikkNr  = iCL AND 
          FalckEksport.EksportId >= dFraNr AND
          FalckEksport.EksportId <= dTilNr USE-INDEX EksportId NO-LOCK NO-ERROR.
        IF NOT AVAIL FalckEksport OR (AVAIL FalckEksport AND FalckEksport.EksportId < dTilNr) THEN DO:
            ASSIGN dEksportId = IF AVAIL FalckEksport THEN FalckEksport.EksportId + 1 ELSE dFraNr.
            LEAVE HKLOOP.
        END.
    END.
END.
ELSE BUTIKKLOOP: DO:
    FOR EACH Butiker NO-LOCK:
        ASSIGN dFraNr = Butiker.Butik * 100000 + 1
               dTilNr = Butiker.Butik * 100000 + 99999.
        FIND LAST FalckEksport WHERE 
          FalckEksport.ButikkNr  = iCL AND 
          FalckEksport.EksportId >= dFraNr AND
          FalckEksport.EksportId <= dTilNr USE-INDEX EksportId NO-LOCK NO-ERROR.

        IF NOT AVAIL FalckEksport OR (AVAIL FalckEksport AND FalckEksport.EksportId < dTilNr) THEN DO:
            ASSIGN dEksportId = IF AVAIL FalckEksport THEN FalckEksport.EksportId + 1 ELSE dFraNr.
            LEAVE BUTIKKLOOP.
        END.
    END.
END.
