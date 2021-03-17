DEFINE VARIABLE dDato    AS DATE       NO-UNDO.
DEFINE VARIABLE dFraDato AS DATE       NO-UNDO.
DEFINE VARIABLE dTilDato AS DATE       NO-UNDO.
DEFINE VARIABLE dDatasettId LIKE bonghode.datasettid NO-UNDO.
DEFINE VARIABLE dPrStyck  AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dBongDato AS DATE       NO-UNDO.
DEFINE VARIABLE cButiker AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iButik AS INTEGER    NO-UNDO.
DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
DEFINE BUFFER bufBongLinje FOR BongLinje.
DEFINE BUFFER bufBongHode FOR BongHode.
DEFINE BUFFER bufDatasett FOR Datasett.

/* ASSIGN dFraDato = DATE(1,1,2004)    */
/*        dTilDato = DATE(12,31,2004). */
FOR EACH butiker NO-LOCK:
    cButiker = cButiker + (IF cbutiker <> "" THEN "," ELSE "") + STRING(butiker.butik).
END.
/* megadisk: */
DO iCount = 1 TO NUM-ENTRIES(cButiker):
/* FOR EACH butiker NO-LOCK: */
    iButik = INT(ENTRY(iCount,cButiker)).
    DO dDato = DATE(1,1,2004) TO DATE(12,31,2004):
        ASSIGN dDatasettId = 0
               dBongDato   = ?.
/*         DISPLAY dDato ddatasettid WITH FRAME a DOWN. */
/*         DOWN WITH FRAME a.                           */
/*         PAUSE 0.                                     */
        
        FOR EACH bonghode WHERE Bonghode.ButikkNr = iButik AND
                                 Bonghode.GruppeNr = 1     AND
                                 Bonghode.KasseNr  = 11    AND
                                 Bonghode.Dato     = dDato NO-LOCK:
            IF Bonghode.pfFlagg = 1 THEN
                NEXT.
            FOR EACH bonglinje WHERE BongLinje.b_id = BongHode.b_id NO-LOCK:
                IF Bonglinje.strekkode = "31001" OR Bonglinje.strekkode = "31002" THEN DO:
                    /* kan alltid utföras */
                    IF Bonglinje.linjesum > 0 AND BongLinje.Antall <> 0 AND BongLinje.Antall <> ? THEN DO:
                        dPrStyck = bonglinje.linjeSum / ABS(bonglinje.Antall).
                        /* testa om detta redan är sant innan det utförs, och om det redan är gjort - inget datasett */
                        IF Bonglinje.vvarekost / (IF Bonglinje.strekkode = "31001" THEN .95 ELSE .92) <> dPrStyck THEN 
                        DO:
                            FIND bufBonglinje WHERE ROWID(bufBongLinje) = ROWID(BongLinje).
                            ASSIGN bufBonglinje.vvarekost = dPrStyck * (IF bufBonglinje.strekkode = "31001" THEN .95 ELSE .92).
                            IF dDatasettId = 0 THEN DO:
                                    ASSIGN dDatasettId = BongHode.DatasettId
                                           dBongDato   = BongHode.Dato.
                            END.
                        END.
                    END.
                END.
            END.
        END.
        IF NOT dDatasettid > 0 THEN
            NEXT.
        FIND Datasett WHERE Datasett.datasettid = dDatasettId NO-LOCK NO-ERROR.
        IF NOT AVAIL DataSett THEN
            NEXT.
        FOR EACH bufBongHode WHERE bufBongHode.datasettid = Datasett.DatasettId.
            ASSIGN bufBongHode.pfFlagg = 1.
        END.
        FOR EACH pfTenderRevenue WHERE pfTenderRevenue.Store_No = iButik AND pftenderrevenue.DATE = dBongDato: 
            DELETE pfTenderRevenue. 
        END.
        
        FOR EACH pfStatisticsRevenue WHERE pfStatisticsRevenue.Store_No = iButik AND pfStatisticsrevenue.DATE = dBongDato:  
            DELETE pfStatisticsRevenue. 
        END.
        
        FOR EACH pfDaySales_HourExt WHERE pfDaysales_Hourext.Store_No = iButik AND pfDaysales_Hourext.DATE = dBongDato: 
            DELETE pfDaySales_HourExt. 
        END.
        
        FOR EACH pfDaySales WHERE pfDaysales.Store_No = iButik AND pfDaysales.DATE = dBongDato:
            DELETE pfDaySales. 
        END.
        FIND bufDatasett WHERE bufDatasett.datasettid = dDatasettId.
        bufDatasett.pfFlagg = 1.
        RELEASE bufDatasett.
        DISPLAY iButik dDato ddatasettid WITH FRAME a DOWN.
        DOWN WITH FRAME a.
        PAUSE 0.
    END.
END.

