CURRENT-WINDOW:WIDTH = 200.
DEF VAR lModellFarge AS DEC FORMAT ">>>>>>>>>>>>9" NO-UNDO. 
DEF VAR cTekst AS CHAR FORMAT "x(20)" NO-UNDO.
DEF VAR iStrTypeId AS INT NO-UNDO.

DEF BUFFER bArtBas FOR ArtBas.
DEF BUFFER bufArtBAs FOR ArtBas.

FOR EACH bArtBas EXCLUSIVE-LOCK WHERE
    bArtBAs.OPris = FALSE  AND
    bArtBas.StrTypeId = 2 /*and
    bArtBas.RegistrertDato >= 01/01/2011*/:
    
    IF bArtBas.ModellFarge > 0 
        THEN lModellFarge = bArtBas.ModellFarge.
    ELSE lModellFarge = bArtBas.ArtikkelNr.

    cTekst = 'Modell ' + STRING(lModellFarge).

   
    FOR EACH Strekkode OF bArtBas NO-LOCK:

        FIND StrKonv NO-LOCK WHERE
            StrKonv.StrKode = Strekkode.StrKode NO-ERROR.
        
        IF AVAILABLE StrKonv THEN 
        RUN opprett_strtype2.p (cTekst, StrKonv.Storl, bArtBas.Vg, OUTPUT iStrTypeId). 
    END.

    IF bArtBas.ModellFarge > 0 AND iStrTypeId > 0 THEN
        FOR EACH bufArtBas WHERE
          BufArtBas.ModellFarge = bArtBas.ModellFarge:
          bufArtBas.StrTypeId = iStrTypeId.
        END.
    ELSE bArtBas.StrTypeId = iStrTypeId.
    
    /*
    DISPLAY
            bArtBas.ArtikkelNr
            bArtBas.ModellFarge
            lModellFarge
            cTekst
            bArtBas.Beskr
            bArtBas.StrTypeId
            bArtBas.RegistrertDato
            WITH WIDTH 200.
    */
END.
