FOR EACH LevBas NO-LOCK:
  IF NOT CAN-FIND(Produsent WHERE
                  Produsent.ProdNr = LEvBas.LEvNR) THEN
  DO:
      CREATE Produsent.
      ASSIGN
          Produsent.ProdNr      = LEvBAs.LevNr
          Produsent.Beskrivelse = LevBas.LEvNamn
          .
  END.
END.

FOR EACH ArtBAs:
    ArtBas.ProdNr = ArtBas.LEvNr.
END.
