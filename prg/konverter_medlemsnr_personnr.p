/*
konverter_medlemsnr_personnr.p
*/

DEF INPUT-OUTPUT PARAMETER lMedlemsNr LIKE Medlem.MedlemsNr NO-UNDO.

DEF BUFFER bufMedlem FOR Medlem.

DEF VAR lDec AS DEC NO-UNDO.

/* Leser alle medlemmer hvor personnr er angitt og medlemsnr < 10 siffer. */
FOR EACH Medlem EXCLUSIVE-LOCK WHERE
    Medlem.MedlemsNr = lMedlemsNr:
    
    /* Skal bare ta de hvor personner ikke er medlemsnr. */
    IF Medlem.MedlemsNr >=9999999999  THEN
        NEXT.

    /* Fikser eventuell bindestrek. */
    Medlem.PersonNr = REPLACE(TRIM(Medlem.PersonNr),'-','').

    /* Bare norske og svenske personnr */
    IF NOT CAN-DO('10,11',STRING(LENGTH(Medlem.PersonNr))) THEN
        NEXT.

    /* Finnes det medlem med dette nr fra før, gjør vi ingenting. */
    IF CAN-FIND(bufMedlem WHERE
                bufMedlem.MedlemsNr = DEC(Medlem.PersonNr)) THEN
        NEXT.
        
    ASSIGN 
        lDec = DEC(Medlem.PersonNr) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        NEXT.

    /* ----------------- Konvertering av data ------------------------- */
    FOR EACH MedRabResKontr EXCLUSIVE-LOCK WHERE 
        MedRabResKontr.MedlemsNr = Medlem.MedlemsNr:
        MedRabResKontr.MedlemsNr = DEC(Medlem.PersonNr).
    END.
    FOR EACH MedRabSjekk EXCLUSIVE-LOCK WHERE
        MedRabSjekk.MedlemsNr = Medlem.MedlemsNr:
        MedRabSjekk.MedlemsNr = DEC(Medlem.PersonNr).
    END.
    FOR EACH MedKjop EXCLUSIVE-LOCK WHERE 
        MedKjop.MedlemsNr = Medlem.MedlemsNr:
        Medkjop.MedlemsNr = DEC(Medlem.PersonNr).
    END.
    FOR EACH Gavekort EXCLUSIVE-LOCK WHERE
        Gavekort.MedlemsNr = Medlem.MedlemsNr:
        Gavekort.MedlemsNr = DEC(Medlem.PersonNr).
    END.
    FOR EACH KundeBetTrans EXCLUSIVE-LOCK WHERE 
        KundeBetTrans.MedlemsNr = Medlem.MedlemsNr:
        KundeBetTrans.MedlemsNr = DEC(Medlem.PersonNr).
    END.
    FOR EACH KundeKort EXCLUSIVE-LOCK WHERE 
        KundeKort.MedlemsNr = Medlem.MedlemsNr:
        KundeKort.MedlemsNr = DEC(Medlem.PersonNr).
    END.
    FOR EACH MedlemBetTrans EXCLUSIVE-LOCK WHERE 
        MedlemBetTrans.MedlemsNr = Medlem.MedlemsNr:
        MedlemBetTrans.MedlemsNr = DEC(Medlem.PersonNr).
    END.
    FOR EACH MedlemSaldo EXCLUSIVE-LOCK WHERE 
        MedlemSaldo.MedlemsNr = Medlem.MedlemsNr:
        MedlemSaldo.MedlemsNr = DEC(Medlem.PersonNr).
    END.
    FOR EACH MedlemsKort EXCLUSIVE-LOCK WHERE
        MedlemsKort.MedlemsNr = Medlem.MedlemsNr:
        MedlemsKort.MedlemsNr = DEC(Medlem.PersonNr).
    END.
    FOR EACH MedTrans EXCLUSIVE-LOCK WHERE 
        MedTrans.MedlemsNr = Medlem.MedlemsNr:
        MedTrans.MedlemsNr = DEC(Medlem.PersonNr).
    END.
    FOR EACH Tilgode EXCLUSIVE-LOCK WHERE
        Tilgode.MedlemsNr = Medlem.MedlemsNr:
        Tilgode.MedlemsNr = DEC(Medlem.PersonNr).
    END.
    FOR EACH TransLogg EXCLUSIVE-LOCK WHERE
        TransLogg.MedlemsNr = Medlem.MedlemsNr:
        TransLogg.MedlemsNr = DEC(Medlem.PersonNr).
    END.
    FOR EACH Periode NO-LOCK:
        FOR EACH StLinje EXCLUSIVE-LOCK WHERE
            StLinje.StTypeId   = "MEDLEM" AND
            StLinje.PerId      = Periode.PerId AND 
            StLinje.DataObjekt = STRING(Medlem.MedlemsNr,"9999999999999"):
            StLinje.DataObjekt = STRING(Medlem.PersonNr,"9999999999999").
        END.
    END.

    /* Så tar vi det siste til slutt :) */
    ASSIGN
        lMedlemsNr       = DEC(Medlem.PersonNr)
        Medlem.MedlemsNr = DEC(Medlem.PersonNr)
        .
END.

