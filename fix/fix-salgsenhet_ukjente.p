CURRENT-WINDOW:WIDTH = 300.


DEF VAR iSalgsEnhId AS INT NO-UNDO.

FIND LAST SalgsEnhet NO-LOCK NO-ERROR.
iSalgsEnhId = SalgsEnhet.SalgsEnhId.

FOR EACH ArtBAs WHERE 
    /*ArtBas.SalgsEnhet > '' AND*/
    NOT CAN-FIND(Salgsenhet WHERE SalgsEnhet.SalgsEnhTekst = ArtBas.SalgsEnhet):
    FIND LEvBAs OF ArtBAs NO-LOCK NO-ERROR.

    DISPLAY
        ArtBAs.ArtikkelNr
        ArtBAs.SalgsEnhet
        ArtBas.Beskr
        ArtBAs.LEvKod
        ArtBAs.LevFargKod
        ArtBAs.LEvNr
        LevBAs.LevNamn WHEN AVAILABLE LEvBas
        ArtBAs.RegistrertDato
        ArtBas.EDato
        WITH WIDTH 300.
    CREATE SAlgsEnhet.
    ASSIGN
        SalgsEnhet.SalgsEnhId = iSalgsEnhId
        SalgsEnhet.SalgsEnhTekst = ArtBAs.SalgsEnhet
        iSalgsEnhId = iSalgsEnhId + 1
        .
    
END.
