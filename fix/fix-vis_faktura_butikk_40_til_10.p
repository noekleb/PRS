DEF VAR lMvaKr LIKE FakturaLinje.MvaKr NO-UNDO.
DEF VAR iFraBut AS INT NO-UNDO.
DEF VAR cTekst  AS CHAR NO-UNDO.

DEF BUFFER bufFakturaLinje FOR FakturaLinje.

CURRENT-WINDOW:WIDTH = 350.

DEF STREAM Ut.

OUTPUT STREAM Ut TO VALUE('konv\faktura_m_mva.csv').
FOR EACH FakturaHode NO-LOCK WHERE 
    FakturaHode.ButikkNr = 40 AND 
    FakturaHode.KundeNr = 100105,
    FIRST FakturaLinje OF FakturaHode NO-LOCK:

    ASSIGN
        lMvaKr = 0.
    FOR EACH bufFakturaLinje OF FakturaHode NO-LOCK:
        ASSIGN 
            lMvaKr = lMvaKr + FakturaLinje.MvaKr.
    END.

    FIND LAST PkSdlHode NO-LOCK WHERE
        PkSdlHode.PkSdlNr = STRING(FakturaHode.FakturaNr) NO-ERROR.
    IF AVAILABLE PkSdlHode THEN
    HENTFraBut:
    DO:
        IF NOT ENTRY(2,PkSdlHode.Merknad,CHR(13)) BEGINS 'Overført fra butikk ' THEN
            LEAVE HENTFraBut.
        cTekst = ENTRY(2,PkSdlHode.Merknad,CHR(13)).
        cTekst = ENTRY(1,cTekst,'.').

        iFraBut = INT(ENTRY(4,cTekst,' ')).
    END. /* HENTFraBut */
    ELSE 
        ASSIGN 
            iFraBut = 0
            cTekst  = ''
            .


    PUT STREAM Ut UNFORMATTED
        FakturaHode.KundeNr  ';'
        FakturaHode.ButikkNr  ';'
        FakturaHode.FakturaNr  ';'
        FakturertDato  ';'
        lMvaKr
        SKIP.

    DISPLAY 
    FakturaHode.KundeNr 
    FakturaHode.ButikkNr
    FakturaHode.FakturaNr
    FakturertDato 
    lMvaKr
    ENTRY(2,PkSdlHode.Merknad,CHR(13)) WHEN AVAILABLE PkSdlHode FORMAT "x(50)" 
    PkSdlHode.PkSdlStatus WHEN AVAILABLE PkSdlHode
    PkSdlHode.PkSdlOpphav WHEN AVAILABLE PkSdlHode
    iFraBut 
    cTekst FORMAT "x(30)"
    WITH WIDTH 350.

END.
OUTPUT STREAM Ut CLOSE.
