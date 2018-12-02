
FOR EACH ArtBAs WHERE ArtBas.LopNr = ?:
    DISPLAY
        ArtBas.ArtikkelNr
        ArtBas.Vg
        ArtBas.LopNr
        ArtBas.RegistrertDato.

  RUN settlopnr.p (INPUT ArtBas.Vg, INPUT 'N', OUTPUT ArtBas.LopNr).

END.
