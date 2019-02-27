DEF VAR iAnt AS INT NO-UNDO.
DEF VAR fLevVerdi AS DEC FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEF VAR cTekst AS CHAR NO-UNDO.
DEF VAR cFil AS CHAR NO-UNDO.

DEF BUFFER bufPkSdlLinje FOR PkSdlLinje.
DEF BUFFER bufPkSdlMottak FOR PkSdlMottak.

DEF STREAM Ut.

CURRENT-WINDOW:WIDTH = 350.

ASSIGN 
    cFil = 'konv\pksdl_opphav5_innlevBut16.csv'
    .

OUTPUT STREAM Ut TO VALUE(cFil).
PUT STREAM Ut UNFORMATTED 
    'PkSdlId;'
    'PkSdlNr;'
    'RegistrertDato;'
    'InnlevertDato;'
    'PkSdlStatus;'
    'ButikkNr;'
    'PkSdlOpphav;'
    'Verdi'
SKIP.

FOR PkSdlHode NO-LOCK WHERE 
    PkSdlHode.PkSdlStatus = 20 AND
    PkSdlHode.PkSdlOpphav = 5, 
    FIRST PkSdlMottak OF PkSdlHode NO-LOCK WHERE
             PkSdlMottak.MottattDato >= 10/01/2017,
    FIRST PkSdlLinje OF PkSdlHode NO-LOCK WHERE
    PkSdlLinje.butikkNr = 16:

    iAnt = iAnt + 1.
    fLevVerdi = 0.
    IF NUM-ENTRIES(PkSdlHode.Merknad,CHR(10)) > 2 THEN
        cTekst = ENTRY(3,PkSdlHode.Merknad,CHR(10)).
    ELSE
        ctekst = ''.
    RUN SumerPkSdl.
    PUT STREAM Ut UNFORMATTED 
        PkSdlHode.PkSdlId ';'
        PkSdlHode.PkSdlNr ';'
        PkSdlHode.RegistrertDato ';'
        PkSdlMottak.MottattDato ';'
        PkSdlHode.PkSdlStatus ';'
        PksdlLinje.ButikkNr ';'
        PksdlHode.PkSdlOpphav ';'
        fLevVerdi
    SKIP.
END.
OUTPUT STREAM Ut CLOSE.

PROCEDURE SumerPkSdl:
    FOR EACH PkSdlPris OF PkSdlHode NO-LOCK,
      EACH bufPkSdlLinje OF PkSdlPris NO-LOCK:
        ASSIGN 
            fLevVerdi    = fLevVerdi + bufPkSdlLinje.AntLevert * PkSdlPris.NyVarekost
            .
    END.
END PROCEDURE.
