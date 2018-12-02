DEF VAR iAntPoster AS INT NO-UNDO.
DEF VAR iBatchNr   AS DEC  NO-UNDO.

DEF BUFFER buffTransLogg FOR TransLogg.


CURRENT-WINDOW:WIDTH = 250.
                                   
FOR EACH Butiker NO-LOCK:

  RUN batchlogg.p ("KORR Varemottak: ",
                     "Fra butikk " +
                     STRING(Butiker.Butik) + ' ' +
                     string(today) +
                     " " +
                     string(TIME,"HH:MM") +
                     " " +
                     USERID("dictdb"),
                     OUTPUT iBatchNr).

  FOR EACH TransLogg WHERE
    TransLogg.Butik = Butiker.Butik AND
    TransLogg.Dato >= 01/01/2001 AND
    TransLogg.TTId = 5 AND
    TransLogg.BongId > 0:

    iAntPoster = iAntPoster + 1.

    FIND ArtBas NO-LOCK WHERE
        ArtBas.ArtikkelNr = TransLogg.ArtikkelNr NO-ERROR.
    IF AVAILABLE ArtBas THEN 
        FIND FIRST ArtPris OF ArtBas NO-LOCK NO-ERROR.

    /* Motpostering */
    CREATE buffTranslogg.
    BUFFER-COPY TransLogg
        TO buffTranslogg
        ASSIGN
          BuffTransLogg.SeqNr  = TransLogg.SeqNr + 5
          BuffTransLogg.Antall = buffTransLogg.Antall * -1
          .

    /* Nytt varemottak */
    CREATE buffTranslogg.
    BUFFER-COPY TransLogg
        TO buffTranslogg
        ASSIGN
          BuffTransLogg.SeqNr       = TransLogg.SeqNr + 6
          BuffTransLogg.Pris        = ArtPris.VareKost[1]
          BuffTransLogg.Varekost    = ArtPris.VareKost[1]
          BuffTransLogg.VVarekost   = ArtPris.VareKost[1]
          BuffTransLogg.Mva         = 0
          BuffTransLogg.RabKr       = 0
          BuffTransLogg.SubtotalRab = 0
          .
  END.

  MESSAGE Butiker.Butik iAntPoster
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
  iAntPoster = 0.
END.
