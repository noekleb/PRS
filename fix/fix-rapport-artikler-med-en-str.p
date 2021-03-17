CURRENT-WINDOW:WIDTH = 200.

DEF VAR iAntStr   AS INT  NO-UNDO.
DEF VAR cStr      AS CHAR NO-UNDO.

DEF VAR cFilNavn AS CHAR NO-UNDO.

ASSIGN
    cFilNavn = "Artikler_med_en_storrelse.csv".

DEF STREAM utFil.

OUTPUT STREAM utFil TO VALUE(cFilNavn).
        
EXPORT STREAM utFil DELIMITER ';'
    'ArtikkelNr'
    'Beskr'
    'LevFarg'
    'LevKod'
    'StrType'
    'AntStr'
    'Str' 
    'RegistrertDato'
    'EndretDato'
    'I suppl.bok'
    .

FOR EACH ArtBas WHERE
    ArtBas.StrType > 2 AND
    ArtBAs.EDato > 1/1/2007
    BREAK BY ArtBAs.Beskr
          BY ArtBAs.LevKod:

    ASSIGN
        iAntStr = 0
        cStr    = ""
        .

    FOR EACH Strekkode OF ArtBas NO-LOCK WHERE
        Strekkode.StrKode > 0
       BREAK BY Strekkode.ArtikkelNr
             BY Strekkode.StrKode:

       FIND FIRST StrKonv OF Strekkode.
       IF FIRST-OF(Strekkode.StrKode) THEN
           ASSIGN
           iAntStr = iAntStr + 1
           cStr = cStr + (IF cStr <> "" THEN ", " ELSE "") + StrKonv.Storl
           .
    END.

    IF iAntStr = 1 THEN
    DO:
        EXPORT STREAM utFil DELIMITER ';'
            ArtBAs.ArtikkelNr
            ArtBAs.Beskr
            ArtBAs.LevFarg
            Artbas.LevKod
            ArtBAs.StrType
            iAntStr
            cStr FORMAT "x(30)"
            ArtBAs.RegistrertDato
            ArtBAs.EDato
            "*" WHEN CAN-FIND(FIRST VareBehLinje WHERE
                              VareBehLinje.VareBehNr  = 9000001 AND
                              VareBehLinje.ArtikkelNr = ArtBAs.ArtikkelNr)
            .
    END.
END.

OUTPUT STREAM utFil CLOSE.
