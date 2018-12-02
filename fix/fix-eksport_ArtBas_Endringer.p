DEF VAR cFil AS CHAR NO-UNDO.
DEF VAR cBrukerLst AS CHAR NO-UNDO.
DEF VAR iLoop AS INT NO-UNDO.
DEF VAR dDatoFra AS DATE NO-UNDO.
DEF VAR dDatoTil AS DATE NO-UNDO.

DEF STREAM Ut.

CURRENT-WINDOW:WIDTH = 350.

ASSIGN
    cFil       = 'konv\NyeArtikler' + REPLACE(STRING(TODAY),'/','') + '.csv'
    cBrukerLst = 'tomn,Sport1'
    dDatoFra   = 01/01/2017
    dDatoTil   = 12/31/2017
    .

OUTPUT STREAM Ut TO VALUE(cFil) APPEND.
PUT STREAM Ut UNFORMATTED
    'ArtikkelNr;'
    'Beskr;'
    'Modell;'
    'Farge;'
    'RegistrertDato;'
    'RegistrertAv'
    SKIP.
OUTPUT STREAM Ut CLOSE.

FOR EACH ArtBas NO-LOCK WHERE 
    ArtBas.RegistrertDato >= dDatoFra AND
    ArtBas.RegistrertDato <= dDatoTil:
    FIND Farg OF ArtBas NO-LOCK NO-ERROR.

    IF CAN-DO(cBrukerLst,ArtBas.RegistrertAv) THEN
    DO:
        OUTPUT STREAM Ut TO VALUE(cFil) APPEND.
        /*
        DISPLAY
            ArtBas.ArtikkelNr
            ArtBas.Beskr FORMAT "x(40)"
            ArtBas.LevKod
            Farg.FarBeskr
            ArtBas.RegistrertDato
            ArtBas.RegistrertAv

        WITH WIDTH 350.
        */
        PUT STREAM Ut UNFORMATTED 
            ArtBas.ArtikkelNr ';'
            ArtBas.Beskr ';'
            ArtBas.LevKod ';'
            (IF AVAILABLE Farg THEN Farg.FarBeskr ELSE '') ';'
            ArtBas.RegistrertDato ';'
            ArtBas.RegistrertAv
            SKIP.
        OUTPUT STREAM Ut CLOSE.
    END.
END.
