DEF VAR iCl AS INT.

{syspara.i 5 1 1 iCl INT}
FIND Butiker NO-LOCK WHERE
    Butiker.Butik = iCl.

FOR EACH ArtBas EXCLUSIVE-LOCK WHERE
    ArtBas.AnbefaltPris = 0 AND artbas.opris = FALSE:
    FIND ArtPris NO-LOCK WHERE
        ArtPris.ArtikkelNr = ArtBAs.ArtikkelNr AND
        ArtPris.ProfilNr = Butiker.ProfilNr NO-ERROR.
    IF AVAILABLE ArtPris THEN
        ASSIGN
        ArtBas.AnbefaltPris = ArtPris.Pris[1]
        .
END.
