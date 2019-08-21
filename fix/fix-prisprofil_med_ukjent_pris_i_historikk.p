CURRENT-WINDOW:WIDTH = 350.

DEF STREAM Ut.


OUTPUT STREAM Ut TO VALUE('konv\prisprofil16_ukjent_pris12082019.csv').

    PUT STREAM Ut UNFORMATTED
        'Beskr;'
        'LevKod;'
        'LevFargKod;'
        'ProfilNr;'
        'EndringsNr;'
        'Pris[Normal];'
        'Tilbud aktivt;'
        'Pris[Tilbud];'
        'Pris[PrisKøPris];'
        'AktiveresDato[Priskø];'
        'RegistrertDato[Priskø]'
    SKIP.

FOR EACH PrisProfil NO-LOCK WHERE 
    PrisProfil.ProfilNr = 16,
    EACH ArtPris NO-LOCK OF PrisProfil,
    FIRST ArtBas OF ArtPris NO-LOCK,
    FIRST hPrisKo NO-LOCK OF ArtPris WHERE 
        HPrisko.Pris = ?:


    PUT STREAM Ut UNFORMATTED
        ArtBas.Beskr ';'
        ArtBas.LevKod ';'
        ArtBAs.LevFargKod ';'
        PrisProfil.ProfilNr ';'
        hPrisKo.EndringsNr ';'
        ArtPris.Pris[1] ';'
        ArtPris.Tilbud ';'
        ArtPris.Pris[2] ';'
        hPrisKo.Pris ';'
        hPrisKo.AktiveresDato ';'
        ArtPris.RegistrertDato
    SKIP.

END.
OUTPUT STREAM Ut CLOSE.
