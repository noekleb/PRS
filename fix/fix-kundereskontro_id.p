DEF VAR dFraNr AS DEC NO-UNDO.
DEF VAR dTilNr AS DEC NO-UNDO.

DEF VAR dReskId AS DEC NO-UNDO.

DEF BUFFER bKundereskontr FOR Kundereskontr.
DEF BUFFER bFakturaHode   FOR FakturaHode.

FIND FIRST Butiker NO-LOCK WHERE
    Butiker.Sentrallager = TRUE.

ASSIGN dFraNr = DEC(string(Butiker.Butik) + SUBstring(STRING(YEAR(TODAY)),3,2)) * 10000000 + 1
       dTilNr = DEC(string(Butiker.Butik) + SUBstring(STRING(YEAR(TODAY)),3,2)) * 10000000 + 9999999.

FOR EACH Kundereskontr WHERE 
    Kundereskontr.Reskontro_Id <= 0:

    FIND LAST bKundereskontr WHERE bKundereskontr.Reskontro_Id >= dFraNr AND
                          bKundereskontr.Reskontro_Id <= dTilNr USE-INDEX Kundereskontr NO-LOCK NO-ERROR.
    IF AVAILABLE bKundereskontr THEN
        dReskId = bKundereskontr.REskontro_id + 1.
    ELSE 
        dReskId = DfraNr.

    Kundereskontr.Reskontro_Id = dReskId.

    /*
    DISPLAY
        Kundereskontr
        .
    FOR EACH Kundereskobling WHERE
        Kundereskobling.Reskontro_Id = Kundereskontr.Reskontro_Id:

    END.
    */
END.

ASSIGN dFraNr = DEC(string(Butiker.Butik) + SUBstring(STRING(YEAR(TODAY)),3,2)) * 10000000 + 1
       dTilNr = DEC(string(Butiker.Butik) + SUBstring(STRING(YEAR(TODAY)),3,2)) * 10000000 + 9999999.

FOR EACH FakturaHode WHERE 
    FakturaHode.Faktura_Id <= 0:

    FIND LAST bFakturaHode WHERE bFakturaHode.Faktura_Id >= dFraNr AND
                          bFakturaHode.Faktura_Id <= dTilNr USE-INDEX FaktHode NO-LOCK NO-ERROR.
    IF AVAILABLE bFakturaHode THEN
        dReskId = bFakturaHode.Faktura_id + 1.
    ELSE 
        dReskId = DfraNr.

    FOR EACH FakturaLinje OF FakturaHode:
        ASSIGN
            FakturaLinje.Faktura_Id = dReskId
            .
    END.

    FakturaHode.Faktura_Id = dReskId.

END.






