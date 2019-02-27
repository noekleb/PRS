DEF VAR iBatchNr   AS DEC  NO-UNDO.
DEF VAR piSeqNr    AS INT NO-UNDO.
DEF VAR piTransNr  AS INT NO-UNDO.
DEF VAR lOvAnt     AS DEC NO-UNDO.

CURRENT-WINDOW:WIDTH = 300.

DEF VAR lAntall AS DEC NO-UNDO.
DEF VAR cLogg AS CHAR NO-UNDO.

cLogg = 'logg-vis_kordrehodelinje' + REPLACE(STRING(TODAY),'/','').

RUN bibl_logg.p (cLogg, 'Start.').

RUN batchlogg.p ("Overfør fra 15 til 16: ",
                   "Fra butikk 15" +
                   string(today) +
                   " " +
                   string(TIME,"HH:MM") +
                   " " +
                   USERID("dictdb"),
                   OUTPUT iBatchNr).

FOR EACH ArtLag NO-LOCK WHERE
    ArtLag.butik = 15 AND
    ArtLAg.LagAnt > 0:

    FIND Lager NO-LOCK WHERE
        Lager.Butik = ArtLag.butik AND
        Lager.ArtikkelNr = ArtLag.ArtikkelNr NO-ERROR.
    FIND ArtBas NO-LOCK WHERE
        ArtBas.ArtikkelNr = ArtLag.ArtikkelNr NO-ERROR.
    FIND butiker NO-LOCK WHERE
        Butiker.Butik = ArtLag.Butik NO-ERROR.

    ASSIGN 
        lAntall = 0
        lOvAnt = 0.

    FOR EACH KORdreLinje NO-LOCK WHERE
        KOrdreLinje.Varenr = STRING(ArtLag.ArtikkelNr), 
        FIRST KOrdreHode OF KOrdreLinje NO-LOCK :

        IF KOrdreHode.EkstOrdreNr BEGINS 'RETUR' THEN NEXT.
        IF KORdreLinje.VAreNr = 'BETALT' THEN NEXT.
        
        IF KORdreHode.LevStatus = '30' THEN
            lAntall = lAntall + KOrdreLinje.Antall.
    END.

    IF lAntall > 0 THEN
        lOvAnt = ArtLag.LagAnt - lAntall.
    ELSE
        lOvant = ArtLag.Lagant.


    IF lOvant > 0 THEN
    FLYTTLAGER:
    DO:
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

        FIND ArtPris OF ArtBas NO-LOCK WHERE
            ArtPris.ProfilNr = Butiker.ProfilNr NO-ERROR.
        IF NOT AVAILABLE ArtPris THEN 
          FIND FIRST ArtPris OF ArtBas NO-ERROR.


  TRANSLOGGEN:
  DO:
      /* Oppretter TransLogg */    
      CREATE TransLogg.
      NYTRANSLOGG:
      DO WHILE TRUE ON ERROR UNDO, RETRY:
          ASSIGN TransLogg.Butik        = ArtLag.Butik
                 TransLogg.TransNr      = piTransNr
                 TransLogg.SeqNr        = 1
                 TransLogg.Ovbutik      = 16
                 NO-ERROR.
          IF ERROR-STATUS:ERROR THEN
              piTransNr = piTransNr + 1.
          ELSE LEAVE NYTRANSLOGG.
      END. /* NYTRANSLOGG */

      ASSIGN
             TransLogg.BatchNr      = iBatchNr
             TransLogg.TTId         = 6
             TransLogg.TBId         = 1
             TransLogg.ArtikkelNr   = ArtLag.ArtikkelNr
             TransLogg.Vg           = ArtBas.Vg
             TransLogg.LopNr        = ArtBas.LopNr
             TransLogg.Antall       = lOvant
             TransLogg.Pris         = Lager.VVareKost
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
             TransLogg.Varekost     = Lager.VVareKost
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

        RUN bibl_logg.p (cLogg, 
                         ';Translogg:;' +
                         STRING(TransLogg.Butik) + ';' +
                         STRING(TransLogg.TransNr)+ ';' +
                         STRING(TransLogg.SeqNr) + ';' +
                         STRING(TransLogg.Ovbutik) + ';' +
                         STRING(Translogg.ArtikkelNr)                         
                         ).

  END. /* TRANSLOGGEN */
    END. /* FLYTTLAGER */
END.
 
RUN bibl_logg.p (cLogg, 'Slutt.').
