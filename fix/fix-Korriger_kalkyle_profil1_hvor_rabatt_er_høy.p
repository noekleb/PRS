DEF VAR cFil AS CHAR NO-UNDO.
DEF VAR cBehIButikk AS CHAR FORMAT "x(50)" NO-UNDO.
DEF VAR iant AS INT NO-UNDO.

DEF STREAM Ut.

CURRENT-WINDOW:WIDTH = 350.

ASSIGN
    cFil = 'konv\Korriger_kalkyle_profil1_hvor_rabatt_er_hoy' + REPLACE(STRING(TODAY),'/','') + '.csv'
    .

OUTPUT STREAM Ut TO VALUE(cFil).

PUT STREAM Ut UNFORMATTED
    'ArtPris.ArtikkelNr;'
    'ArtPris.ProfilNr;'
    'ArtBas.Besk;'
    'ArtBas.LevKod;'
    'ArtBas.LevFargKod;'
    'ArtBas.Sasong;'
    'ArtPris.Rab1%[1];'
    'ArtPris.Rab1%[2];'
    'ArtBas.RegistrertDato;'
    'BehIButikk'
    SKIP.

KORRIGER:
FOR EACH ArtPris NO-LOCK WHERE
    ArtPris.ArtikkelNr > 0 AND
    ArtPris.ProfilNr = 1 AND
    Artpris.Rab1%[1] > 10,
    FIRST ArtBas OF ArtPris NO-LOCK:

    cBehIButikk = ''.
    FOR EACH Lager OF ArtBas NO-LOCK WHERE 
        Lager.Butik < 99:
        IF Lager.Lagant > 0 THEN
            cBehIButikk = cBehIButikk + 
                          (IF cBehIButikk = '' THEN '' ELSE ',') + 
                          STRING(Lager.Butik).
    END.
    PUT STREAM Ut UNFORMATTED 
        ArtPris.ArtikkelNr ';'
        ArtPris.ProfilNr ';'
        ArtBas.Besk ';'
        ArtBas.LevKod ';'
        ArtBas.LevFargKod ';'
        ArtBas.Sasong ';'
        ArtPris.Rab1%[1] ';'
        ArtPris.Rab1%[2] ';'
        ArtBas.RegistrertDato ';'
        cBehIButikk
    SKIP.

    IF iAnt > 2 THEN
        LEAVE KORRIGER.
END. /* KORRIGER */

OUTPUT STREAM Ut CLOSE.
