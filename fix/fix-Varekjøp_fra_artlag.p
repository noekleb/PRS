DEF VAR iAntPoster AS INT NO-UNDO.
DEF VAR iBatchNr   AS DEC  NO-UNDO.
DEF VAR piSeqNr    AS INT NO-UNDO.
DEF VAR piTransNr  AS INT NO-UNDO.

DEF BUFFER buffTransLogg FOR TransLogg.


CURRENT-WINDOW:WIDTH = 250.
                                   
RUN batchlogg.p ("Varekjøp: ",
                   "Fra butikk 277" +
                   string(today) +
                   " " +
                   string(TIME,"HH:MM") +
                   " " +
                   USERID("dictdb"),
                   OUTPUT iBatchNr).

FOR EACH ArtLag WHERE
  ArtLag.LagAnt > 0:


  iAntPoster = iAntPoster + 1.

  FIND Lager NO-LOCK WHERE
      Lager.ArtikkelNr = ArtLag.ArtikkelNr AND
      Lager.Butik      = ArtLag.Butik.

  FIND ArtBas NO-LOCK WHERE
      ArtBas.ArtikkelNr = ArtLag.ArtikkelNr NO-ERROR.
  IF AVAILABLE ArtBas THEN 
      FIND FIRST ArtPris OF ArtBas NO-LOCK NO-ERROR.

  /* Setter transaksjonsnummer  */
  IF piTransNr = 0 THEN
    DO:
      FIND LAST TransLogg WHERE
        TransLogg.Butik = ArtLag.Butik
        USE-INDEX TransLogg NO-ERROR.
      IF AVAILABLE TransLogg THEN
        piTransNr = TransLogg.TransNr + 1.
      ELSE
        piTransNr = 1.
    END.
  ELSE
    piTransNr = piTransNr + 1.

  TRANSLOGGEN:
  DO:
      /* Oppretter TransLogg */    
      CREATE TransLogg.
      NYTRANSLOGG:
      DO WHILE TRUE ON ERROR UNDO, RETRY:
          ASSIGN TransLogg.Butik        = ArtLag.Butik
                 TransLogg.TransNr      = piTransNr
                 TransLogg.SeqNr        = 1
                 NO-ERROR.
          IF ERROR-STATUS:ERROR THEN
              piTransNr = piTransNr + 1.
          ELSE LEAVE NYTRANSLOGG.
      END. /* NYTRANSLOGG */

      ASSIGN
             TransLogg.BatchNr      = iBatchNr
             TransLogg.TTId         = 5
             TransLogg.TBId         = 1
             TransLogg.ArtikkelNr   = ArtLag.ArtikkelNr
             TransLogg.Vg           = ArtBas.Vg
             TransLogg.LopNr        = ArtBas.LopNr
             TransLogg.Antall       = ArtLag.LagAnt
             TransLogg.Pris         = ArtPris.Pris[1]
             TransLogg.RabKr        = 0
             
             TransLogg.LevNr        = IF AVAILABLE ArtBas
                                        THEN ArtBas.LevNr
                                        ELSE 0
             TransLogg.ForsNr       = 1
             TransLogg.Plukket      = TRUE 
             TransLogg.Dato         = TODAY 
             TransLogg.Tid          = 0
             TransLogg.SelgerNr     = 1
             TransLogg.BestNr       = 0
             TransLogg.Postert      = TRUE
             Translogg.BongTekst    = ArtBas.Beskr
             TransLogg.VVareKost    = Lager.VVareKost
             TransLogg.VVareKost    = IF TransLogg.VVAreKost = ? THEN 0 ELSE Translogg.VVareKost
             TransLogg.SattVVarekost = FALSE
             TransLogg.KalkylePris  = IF AVAILABLE ArtPris
                                        THEN ArtPris.Pris[IF ArtPris.Tilbud THEN 2 ELSE 1]
                                        ELSE Translogg.KalkylePris
             TransLogg.Varekost     = IF AVAILABLE ArtPris
                                        THEN ArtPris.Varekost[IF ArtPris.Tilbud THEN 2 ELSE 1]
                                        ELSE TransLogg.Varekost
             TransLogg.Pris         = TransLogg.Varekost 
             .
        ASSIGN
          TransLogg.Pris          = TransLogg.Varekost
          TransLogg.RabKr         = 0
          TransLogg.SubtotalRab   = 0
          TransLogg.VVareKost     = TransLogg.Varekost
          TransLogg.SattVVarekost = TRUE
          TransLogg.Mva           = 0
          TransLogg.Mva%          = 0
          TransLogg.Storl        = ArtLag.Storl
          TransLogg.TilStorl     = Artlag.Storl
          .
  END. /* TRANSLOGGEN */
  iAntPoster = 0.
END.
