DEF VAR dOldArtikkelNr AS DEC NO-UNDO.
DEF VAR dNyArtikkelNr AS DEC NO-UNDO.

DEF BUFFER bufPkSdlHode FOR PkSdlHode. 
DEF BUFFER bufPkSdlLinje FOR PkSdlLinje.

/*
FOR EACH bufPkSdlHode NO-LOCK WHERE 
    bufPkSdlHode.PkSdlStatus = 10,
    EACH bufPkSdlLinje OF bufPkSdlHode,
    FIRST ArtBas OF bufPkSdlLinje WHERE SanertDato <> ?:

    DISPLAY 'PAKKSEDDLER:'
        bufPkSdlLinje.ArtikkelNr
        ArtBas.SanertDato
        ENTRY(2,ArtBas.Notat,' ')
        .



    ASSIGN 
        dOldArtikkelNr = bufPkSdlLinje.ArtikkelNr
        dNyArtikkelNr  = dec(ENTRY(2,ArtBas.Notat,' '))
        .

    FOR EACH PksdlHode NO-LOCK WHERE 
        PkSdlHode.PksdlStatus = 10,
        EACH PkSdlLinje OF PkSdlHode EXCLUSIVE-LOCK WHERE 
            PkSdlLinje.ArtikkelNr = dOldArtikkelnr:

        ASSIGN 
            PkSdlLinje.ArtikkelNr = dNyArtikkelnr NO-ERROR.
    END.

    FOR EACH PksdlHode NO-LOCK WHERE 
        PkSdlHode.PksdlStatus = 10,
        EACH PkSdlPris OF PkSdlHode EXCLUSIVE-LOCK WHERE 
            PkSdlPris.ArtikkelNr = dOldArtikkelnr:

        ASSIGN 
            PkSdlPris.ArtikkelNr = dNyArtikkelnr NO-ERROR.
    END.            

END.
*/

FOR EACH KOrdreHode WHERE 
    KOrdreHode.LevStatus < '50',
    EACH KOrdreLinje OF KOrdrEHode WHERE 
        KOrdreLinje.VareNr <> 'BETALT',
    FIRST ArtBas WHERE 
        ArtBas.ArtikkelNr = DEC(KOrdreLinje.VareNr) AND 
        ArtBas.SanertDato <> ?:

    DISPLAY 'KUNDEORDRE:'
        KOrdreLinje.VareNr
        ArtBas.ArtikkelNr
        ENTRY(2,ArtBas.Notat,' ')
        .





END.
