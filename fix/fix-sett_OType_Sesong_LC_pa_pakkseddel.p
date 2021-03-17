DEF VAR cOType AS CHAR NO-UNDO.
DEF VAR cSesongkode AS CHAR NO-UNDO.
DEF VAR lLandedCost AS DEC FORMAT "->>,>>>,>>>,>>9.99" NO-UNDO.

CURRENT-WINDOW:WIDTH = 350.
FOR EACH PkSdlHode:
    IF NUM-ENTRIES(MeldingFraLev,CHR(10)) >= 3 THEN 
    DO:
        ASSIGN 
            cOType =  TRIM(ENTRY(2,ENTRY(1,MeldingFraLEv,CHR(10)),' '))
            cSesongkode =  TRIM(ENTRY(2,ENTRY(2,MeldingFraLEv,CHR(10)),' '))
            lLandedCost =  DEC(TRIM(ENTRY(2,ENTRY(3,MeldingFraLEv,CHR(10)),' ')))
            .
        ASSIGN 
            PkSdlHode.OrdreType = cOType
            PkSdlHode.SesongKode = cSesongKode
            PkSdlHode.LandedCost = lLandedCost
            .
        DISPLAY
            PkSdlHode.PkSdlNr
            cOType
            cSesongkode
            lLandedCost
            '|'
            PkSdlHode.OrdreType 
            PkSdlHode.SesongKode
            PkSdlHode.LandedCost

            '|'
            ENTRY(1,MeldingFraLEv,CHR(10)) FORMAT "x(20)" WHEN NUM-ENTRIES(MeldingFraLev,CHR(10)) >= 3
            ENTRY(2,MeldingFraLEv,CHR(10)) FORMAT "x(20)" WHEN NUM-ENTRIES(MeldingFraLev,CHR(10)) >= 3
            ENTRY(3,MeldingFraLEv,CHR(10)) FORMAT "x(20)" WHEN NUM-ENTRIES(MeldingFraLev,CHR(10)) >= 3
            MeldingFraLEv FORMAT "x(100)"
        WITH WIDTH 350.
    END.
END.
