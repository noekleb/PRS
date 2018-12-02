DEFINE OUTPUT PARAMETER dFaktura_Id AS DEC NO-UNDO.
DEFINE VARIABLE lLokaltHk    AS LOGICAL              NO-UNDO.
DEFINE VARIABLE strLokaltHk  AS CHARACTER            NO-UNDO.
DEFINE VARIABLE cHkNumSerier AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iCount       AS INTEGER    NO-UNDO.
DEFINE VARIABLE dFraNr       LIKE FakturaHode.Faktura_Id NO-UNDO.
DEFINE VARIABLE dTilNr       LIKE FakturaHode.Faktura_Id NO-UNDO.
/* Ta reda på om vi har lokalt HK  */
/* Om lokalt HK skall vi gerera    */
/* mha systemparameter  */
{syspara.i 1 1 18 strLokaltHk}
ASSIGN lLokaltHk = strLokaltHk = "yes".
IF lLokaltHk THEN
   {syspara.i 1 1 23 cHkNumSerier}

IF lLokaltHK THEN HKLOOP: DO:
    DO iCount = 1 TO NUM-ENTRIES(cHkNumSerier):
        ASSIGN dFraNr = DECI(ENTRY(1,ENTRY(iCount,cHkNumSerier),"-"))
               dTilNr = DECI(ENTRY(2,ENTRY(iCount,cHkNumSerier),"-")).
        FIND LAST FakturaHode WHERE FakturaHode.Faktura_Id >= dFraNr AND
                              FakturaHode.Faktura_Id <= dTilNr USE-INDEX FaktHode NO-LOCK NO-ERROR.
        IF NOT AVAIL FakturaHode OR (AVAIL FakturaHode AND FakturaHode.Faktura_Id < dTilNr) THEN DO:
            ASSIGN dFaktura_Id = IF AVAIL FakturaHode THEN FakturaHode.Faktura_Id + 1 ELSE dFraNr.
            LEAVE HKLOOP.
        END.
    END.
END.
ELSE BUTIKKLOOP: DO:
    FOR EACH Butiker NO-LOCK:
        ASSIGN dFraNr = DEC(string(Butiker.Butik) + SUBstring(STRING(YEAR(TODAY)),3,2)) * 10000000 + 1
               dTilNr = DEC(string(Butiker.Butik) + SUBstring(STRING(YEAR(TODAY)),3,2)) * 10000000 + 9999999.
        FIND LAST FakturaHode WHERE FakturaHode.Faktura_Id >= dFraNr AND
                              FakturaHode.Faktura_Id <= dTilNr USE-INDEX FaktHode NO-LOCK NO-ERROR.

        IF NOT AVAIL FakturaHode OR (AVAIL FakturaHode AND FakturaHode.Faktura_Id < dTilNr) THEN DO:
            ASSIGN dFaktura_Id = IF AVAIL FakturaHode THEN FakturaHode.Faktura_Id + 1 ELSE dFraNr.
            LEAVE BUTIKKLOOP.
        END.
    END.
END.
