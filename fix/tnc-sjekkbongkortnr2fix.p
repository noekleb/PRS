CURRENT-WINDOW:WIDTH = 250.

DEF VAR cTekst AS CHAR.
DEF VAR cKunde AS CHAR.


OUTPUT TO "SjekkKort.txt" NO-ECHO.

FOR EACH DataSett NO-LOCK WHERE
    /*DataSett.DataSettId = 157 AND */
    DataSett.FilType = 3: /* Utskriftskopi */
    FOR EACH FilLinje OF DataSett NO-LOCK WHERE
        entry(6,FilLinje.Tekst,"|") <> "":
        FIND Filer OF FilLinje NO-LOCK NO-ERROR.

        ASSIGN
            cTekst = entry(4,FilLinje.Tekst,"|")
            cKunde = left-trim(entry(6,FilLinje.Tekst,"|"),"0")
            .

        FIND FIRST MedlemsKort NO-LOCK WHERE
            MedlemsKort.KortNr = cKunde NO-ERROR.
        IF AVAILABLE MedlemsKort THEN
        DO:
            FIND Medlem OF MedlemsKort.
            IF Medlem.KundeNr <> 0 THEN
                FIND Kunde NO-LOCK WHERE
                Kunde.KundeNr = Medlem.KundeNr NO-ERROR.
        END.
        IF NOT AVAILABLE Medlemskort THEN
            FIND FIRST KundeKort NO-LOCK WHERE
            KundeKort.KortNr = cKunde NO-ERROR.

        FIND FIRST BongHode NO-LOCK WHERE
            BongHode.ButikkNr = INT(entry(1,FilLinje.Tekst,"|")) AND
            BongHode.GruppeNr = INT(entry(2,FilLinje.Tekst,"|")) AND
            BongHode.KasseNr  = INT(entry(3,FilLinje.Tekst,"|")) AND
            BongHode.Dato     = DATE(int(ENTRY(2,cTekst,"/")),
                                     int(ENTRY(3,cTekst,"/")),
                                     int(ENTRY(1,cTekst,"/")) + 2000
                                    ) AND
            BongHode.BongNr   = INT(entry(5,FilLinje.Tekst,"|")) NO-ERROR.
        IF AVAILABLE BongHode THEN
        DO:
            IF BongHode.KundeKort = "" AND 
               BongHode.MedlemsKort = "" THEN
            DISPLAY
                FilLinje.Tekst FORMAT "x(30)"
                cKunde
                "KndKort" WHEN AVAILABLE KundeKort FORMAT "x(6)"
                "Knd" WHEN AVAILABLE Kunde         FORMAT "x(3)"
                "Med" WHEN AVAILABLE Medlemskort   FORMAT "x(3)"
                BongHode.BongNr WHEN AVAILABLE BongHode
                "*Ukjent*" WHEN NOT AVAILABLE BongHode @ BongHOde.KundeKort
                BongHode.KundeKort WHEN AVAILABLE BongHode
                BongHode.KundeNr   WHEN AVAILABLE BongHode
                BongHode.MedlemsKort WHEN AVAILABLE BongHode
                BongHode.MedlemsNr   WHEN AVAILABLE BongHode
                FilLinje.DataSettId
                Filer.FilNavn
                WITH WIDTH 248
                .
        END.
    END.
END.
