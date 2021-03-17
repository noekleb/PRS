DEFINE OUTPUT PARAMETER dSIEEksportNr LIKE SIEEksport.SIEEksportNr  NO-UNDO.


FIND LAST SIEEksport NO-LOCK USE-INDEX SIEEksportNr NO-ERROR.

IF AVAILABLE SIEEksport
  THEN dSIEEksportNr = SIEEksport.SIEEksportNr + 1.
  ELSE dSIEEksportNr = 1.





/*DEFINE VARIABLE lLokaltHk    AS LOGICAL               NO-UNDO.
DEFINE VARIABLE strLokaltHk  AS CHARACTER             NO-UNDO.
DEFINE VARIABLE cHkNumSerier AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iCount       AS INTEGER    NO-UNDO.
DEFINE VARIABLE dFraNr       LIKE TGExport.TGExportId NO-UNDO.
DEFINE VARIABLE dTilNr       LIKE TGExport.TGExportId NO-UNDO.
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
        FIND LAST TGExport WHERE 
          TGExport.TGExportId >= dFraNr AND
          TGExport.TGExportId <= dTilNr USE-INDEX TGExportId NO-LOCK NO-ERROR.
        IF NOT AVAIL TGExport OR (AVAIL TGExport AND TGExport.TGExportId < dTilNr) THEN DO:
            ASSIGN dTGExportId = IF AVAIL TGExport THEN TGExport.TGExportId + 1 ELSE dFraNr.
            LEAVE HKLOOP.
        END.
    END.
END.
ELSE BUTIKKLOOP: DO:
    FOR EACH Butiker NO-LOCK:
        ASSIGN dFraNr = Butiker.Butik * 10000000 + 1
               dTilNr = Butiker.Butik * 10000000 + 9999999.
        FIND LAST TGExport WHERE 
          TGExport.TGExportId >= dFraNr AND
          TGExport.TGExportId <= dTilNr USE-INDEX TGExportId NO-LOCK NO-ERROR.

        IF NOT AVAIL TGExport OR (AVAIL TGExport AND TGExport.TGExportId < dTilNr) THEN DO:
            ASSIGN dTGExportId = IF AVAIL TGExport THEN TGExport.TGExportId + 1 ELSE dFraNr.
            LEAVE BUTIKKLOOP.
        END.
    END.
END.*/
