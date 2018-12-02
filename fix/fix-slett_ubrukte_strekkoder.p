FOR EACH ArtBas NO-LOCK WHERE
  ArtBas.OPris = FALSE AND
  ArtBas.Pakke = FALSE:
  FOR EACH Strekkode OF ArtBAs:
      FIND StrKonv OF Strekkode.
      IF LENGTH(strekkode.Kode) = 13 AND
        Strekkode.Kode BEGINS '02' THEN 
        DO:
          IF NOT CAN-FIND(FIRST Translogg WHERE
                      Translogg.ArtikkelNr = ArtBas.ArtikkelNr AND
                      TransLogg.Storl      = StrKonv.Storl) THEN
          DELETE Strekkode.
        END.
  END.
END.
