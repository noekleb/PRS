/************************************************************
    Program:  x-oppdlagertran2.i
    Created:  TN   21 May 100
Description:

Last change:  TN   21 May 100   11:25 am
************************************************************/

/* Samme håndtering alle transtyper. */
IF "{&Prefix}" = "St" THEN
DO:
    /* Overføring håndteres separat. */
    IF CAN-DO("1,2,4,7,9,10,11", STRING(TransLogg.TTId)) THEN
        ASSIGN
        StLager.vVarekost = StLager.vVarekost + ((TransLogg.Antall * (IF Translogg.VVarekost = ? THEN 0 ELSE TransLogg.vVarekost)) * -1)
        .
    ELSE IF CAN-DO("5", STRING(TransLogg.TTId)) THEN
        ASSIGN
        StLager.vVarekost = StLager.vVarekost + (TransLogg.Antall * (IF Translogg.VVarekost = ? THEN 0 ELSE TransLogg.vVarekost))
        .
END.

CASE TransLogg.TTId:
  WHEN 1 THEN
  DO:
      IF "{&Prefix}" = "St" THEN
      DO:
          ASSIGN
              piPris = (TransLogg.Pris - Translogg.RabKr) - TransLogg.Mva
              StLager.vSnittKostPris = StLager.vSnittKostPris + (TransLogg.Antall * piPris)
              .
      END.
      ASSIGN /* Varesalg */
        {&Prefix}Lager.Lagant     = {&Prefix}Lager.Lagant     - TransLogg.Antall
        {&Prefix}Lager.AntSolgt   = {&Prefix}Lager.AntSolgt   + TransLogg.Antall
        {&Prefix}Lager.SVK        = {&Prefix}Lager.SVK        + (TransLogg.Antall * (IF Translogg.VVarekost = ? THEN 0 ELSE Translogg.VVareKost))
        {&Prefix}Lager.VerdiSolgt = {&Prefix}Lager.VerdiSolgt +
                           (
                            (TransLogg.Pris - Translogg.RabKr) -
                            TransLogg.Mva
                           ) * TransLogg.Antall
        {&Prefix}Lager.AntRab     = {&Prefix}Lager.AntRab +
                           (IF TransLogg.RabKr <> 0
                              THEN TransLogg.Antall
                              ELSE 0)
        {&Prefix}Lager.VerdiRabatt = {&Prefix}Lager.VerdiRabatt +
                           (IF TransLogg.RabKr <> 0
                              THEN TransLogg.Antall * (Translogg.RabKr - RabUMva(wMva%))
                              ELSE 0) NO-ERROR.
  END.
  
  WHEN 2 THEN
      ASSIGN  /* Brekkasje */
        {&Prefix}Lager.Lagant     = {&Prefix}Lager.Lagant     - TransLogg.Antall
        {&Prefix}Lager.BrekkAnt   = {&Prefix}Lager.BrekkAnt   + TransLogg.Antall
        {&Prefix}Lager.BrekkVerdi = {&Prefix}Lager.BrekkVerdi +
                           ((IF Translogg.VVarekost = ? THEN 0 ELSE TransLogg.VVareKost) * TransLogg.Antall).
  
  WHEN 3 THEN
    DO:
      ASSIGN  /* Kundereklamasjon */
        {&Prefix}Lager.ReklAnt    = {&Prefix}Lager.ReklAnt    + TransLogg.Antall
        {&Prefix}Lager.ReklVerdi  = {&Prefix}Lager.ReklVerdi  +
                           ((IF Translogg.VVarekost = ? THEN 0 ELSE TransLogg.VVareKost) * TransLogg.Antall)
        /* Korrigerer rabatt */
        {&Prefix}Lager.AntRab     = {&Prefix}Lager.AntRab +
                           (IF TransLogg.RabKr <> 0
                              THEN TransLogg.Antall
                              ELSE 0)
        {&Prefix}Lager.VerdiRabatt = {&Prefix}Lager.VerdiRabatt +
                           (IF TransLogg.RabKr <> 0
                              THEN TransLogg.Antall * (Translogg.RabKr - RabUMva(wMva%))
                              ELSE 0)
        /* Korrigerer salget ved retur - NB: Antallet er negativt i transaksjonen. */
        {&Prefix}Lager.AntSolgt   = {&Prefix}Lager.AntSolgt   + TransLogg.Antall
        {&Prefix}Lager.SVK        = {&Prefix}Lager.SVK        + (TransLogg.Antall * (IF Translogg.VVarekost = ? THEN 0 ELSE Translogg.VVareKost))
        {&Prefix}Lager.VerdiSolgt = {&Prefix}Lager.VerdiSolgt +
                           (
                            (TransLogg.Pris - Translogg.RabKr) -
                            TransLogg.Mva
                           ) * TransLogg.Antall.
      IF "{&Prefix}" = "St" THEN
      DO:
          ASSIGN
              piPris = (TransLogg.Pris - Translogg.RabKr) - TransLogg.Mva
              StLager.vSnittKostPris = StLager.vSnittKostPris + (TransLogg.Antall * piPris)
              .
      END.
    END.
  WHEN 4 THEN
      ASSIGN  /* Lagerreklamasjon */
        {&Prefix}Lager.Lagant     = {&Prefix}Lager.Lagant     - TransLogg.Antall
        {&Prefix}Lager.ReklLAnt   = {&Prefix}Lager.ReklLAnt   + TransLogg.Antall
        {&Prefix}Lager.ReklLVerdi = {&Prefix}Lager.ReklLVerdi +
                           ((IF Translogg.VVarekost = ? THEN 0 ELSE TransLogg.VVareKost) * TransLogg.Antall).
  WHEN 5 THEN
    DO: /* Varekjøp m/vektet vareverdi. */
      /* Vekting av varekost skal bare gjøres når det finnes noe på lager fra før. */
      IF {&Prefix}Lager.Lagant > 0 AND TransLogg.Antall > 0 THEN
          ASSIGN
            wWork  = ({&Prefix}Lager.Lagant   * (IF {&Prefix}Lager.VVareKost = ? THEN 0 ELSE {&Prefix}Lager.VVareKost))  /* Gammel lagerverdi */
            wWork2 = (TransLogg.Pris * TransLogg.Antall) /* Verdi av innkjøp  */
            wWork3 = (wWork + wWork2) / (({&Prefix}Lager.LagAnt) + (TransLogg.Antall))
            wWork3 = IF wWork3 = ? THEN (IF {&Prefix}Lager.VVareKost = ? THEN 0 ELSE {&Prefix}Lager.VVareKost) ELSE wWork3.
      ELSE DO:
          IF {&Prefix}Lager.Lagant <= 0 AND TransLogg.Antall > 0 THEN
              wWork3 = (IF Translogg.VVarekost = ? THEN 0 ELSE TransLogg.VVareKost).
          ELSE
              wWork3 = (IF {&Prefix}Lager.VVareKost = ? THEN 0 ELSE {&Prefix}Lager.VVareKost).
      END.

      IF "{&Prefix}" <> "St" THEN
          {&Prefix}Lager.VVareKost = wWork3. /* Setter ny vektet snittpris */
      ASSIGN
        {&Prefix}Lager.Lagant    = {&Prefix}Lager.Lagant     + TransLogg.Antall
        {&Prefix}Lager.KjopAnt   = {&Prefix}Lager.KjopAnt    + TransLogg.Antall
        {&Prefix}Lager.KjopVerdi = {&Prefix}Lager.KjopVerdi  + wWork2.
    END.
  WHEN 6 THEN
    DO: /* Overføring */
      /* Henter Lager eller StLager for mottagende butikk. */
      {&FinnLager}

      ASSIGN  /* Trekker ned lager på fra butikk.            */
              /* Alle posteringer skjer med vektet varekost. */
        {&Prefix}Lager.Lagant  = {&Prefix}Lager.Lagant     - TransLogg.Antall
        {&Prefix}Lager.OvAnt   = {&Prefix}Lager.OvAnt      - TransLogg.Antall
        {&Prefix}Lager.OvVerdi = {&Prefix}Lager.OvVerdi    -
                           ((IF Translogg.VVArekost = ? THEN 0 ELSE TransLogg.VVareKost) * TransLogg.Antall).
      IF "{&Prefix}" = "St" THEN
              {&Prefix}Lager.vVarekost = (IF {&Prefix}Lager.VVareKost = ? THEN 0 ELSE {&Prefix}Lager.vVarekost) + ((TransLogg.Antall * (IF Translogg.VVarekost = ? THEN 0 ELSE TransLogg.vVarekost)) * -1).


      /* 27/10-16 For transtype medd TBId = 2, skal det ikke posteres på mottagende butikk.           */
      /* TBId 2 benyttes for 'Varer på vei'. Det legges opp pakkseddel og gjøres varemottak på disse. */
      IF TransLogg.TBId <> 2 THEN 
      MOTTAGENDEBUT:
      DO:
          /* Innleveranse medfører ny vekting av varekost i mottagende butikk */
          IF buf{&Prefix}Lager.Lagant > 0 AND TransLogg.Antal > 0 THEN
              ASSIGN
                wWork  = (buf{&Prefix}Lager.Lagant * buf{&Prefix}Lager.VVareKost)   /* Gammel lagerverdi */
                wWork2 = (TransLogg.VVareKost * TransLogg.Antall) /* Verdi av overføring  */
                wWork3 = (wWork + wWork2) / ((buf{&Prefix}Lager.LagAnt) + (TransLogg.Antall))
                wWork3 = IF wWork3 = ? THEN buf{&Prefix}Lager.VVareKost ELSE wWork3.
          ELSE DO:
              IF buf{&Prefix}Lager.Lagant <= 0 AND TransLogg.Antall > 0 THEN
                  wWork3 = TransLogg.VVareKost.
              ELSE
                  wWork3 = buf{&Prefix}Lager.VVareKost.
          END.
    
          IF "{&Prefix}" <> "St" THEN
              ASSIGN
                  buf{&Prefix}Lager.VVareKost = wWork3 /* Setter ny vektet snittpris */
                  .
          ASSIGN  /* Posterer i mottagende butikk.  */
                  /* Alle posteringer skjer med vektet varekost. */
            buf{&Prefix}Lager.Lagant    = buf{&Prefix}Lager.Lagant     + TransLogg.Antall
            buf{&Prefix}Lager.OvAnt     = buf{&Prefix}Lager.OvAnt      + TransLogg.Antall
            buf{&Prefix}Lager.OvVerdi   = buf{&Prefix}Lager.OvVerdi    +
                                  (TransLogg.VVareKost * TransLogg.Antall).
          IF "{&Prefix}" = "St" THEN
                  buf{&Prefix}Lager.vVarekost = buf{&Prefix}Lager.vVarekost + (TransLogg.Antall * (IF TransLogg.vVarekost = ? THEN 0 ELSE Translogg.VVarekost)).
      END. /* MOTTAGENDEBUT */              
    END. 
  WHEN 7 THEN
      ASSIGN  /* Lagerjustering */
        {&Prefix}Lager.Lagant    = {&Prefix}Lager.Lagant    - TransLogg.Antall
        {&Prefix}Lager.JustAnt   = {&Prefix}Lager.JustAnt   + TransLogg.Antall
        {&Prefix}Lager.JustVerdi = {&Prefix}Lager.JustVerdi +
                           ((IF Translogg.VVArekost = ? THEN 0 ELSE TransLogg.VVareKost) * TransLogg.Antall).
  WHEN 8 THEN
    DO:
      /* Vektet varekost skal bare beregnes i Lager, ikke i StLager. */
      IF "{&Prefix}" <> "St" THEN
      DO:
          ASSIGN
              {&Prefix}Lager.VVareKost = IF {&Prefix}Lager.VVareKost <= 0 THEN ABS(TransLogg.Pris)
                                                    ELSE {&Prefix}Lager.VVareKost - TransLogg.Pris
              .
      END.

      ASSIGN  /* Nedskrivning */
              /* Ingen endring i lagerantall. Kun VVarekost. */
        {&Prefix}Lager.NedVerdi  = {&Prefix}Lager.NedVerdi  +
                          (TransLogg.Pris * TransLogg.Antall)
        {&Prefix}Lager.NedAnt    = {&Prefix}Lager.NedAnt    + TransLogg.Antall.
    END.
  WHEN 9 THEN
  DO:
      ASSIGN  /* Svinn */
        {&Prefix}Lager.LagAnt     = {&Prefix}Lager.LagAnt    - TransLogg.Antall
        {&Prefix}Lager.SvinnAnt   = {&Prefix}Lager.SvinnAnt  + TransLogg.Antall
        {&Prefix}Lager.SvinnVerdi = {&Prefix}Lager.SvinnVerdi  +
                           (TransLogg.Pris * TransLogg.Antall).
  END.
  WHEN 10 THEN
  DO:
      ASSIGN  /* Gjennkjøp */
        {&Prefix}Lager.LagAnt        = {&Prefix}Lager.LagAnt         + (TransLogg.Antall * -1) /* Negativt antall i TransLogg */
        {&Prefix}Lager.GjenkjopAnt   = {&Prefix}Lager.GjenkjopAnt    + TransLogg.Antall
        {&Prefix}Lager.GjenkjopVerdi = {&Prefix}Lager.GjenkjopVerdi  +
                           (
                            (TransLogg.Pris - Translogg.RabKr) -
                            TransLogg.Mva
                           ) * TransLogg.Antall
        {&Prefix}Lager.AntRab     = {&Prefix}Lager.AntRab +
                           (IF TransLogg.RabKr <> 0
                              THEN TransLogg.Antall
                              ELSE 0)
        {&Prefix}Lager.VerdiRabatt = {&Prefix}Lager.VerdiRabatt +
                           (IF TransLogg.RabKr <> 0
                              THEN TransLogg.Antall * (Translogg.RabKr - RabUMva(wMva%))
                              ELSE 0)
        /* Korrigerer salget ved retur - NB: Antallet er negativt i transaksjonen. */
        {&Prefix}Lager.AntSolgt   = {&Prefix}Lager.AntSolgt   + TransLogg.Antall
         {&Prefix}Lager.SVK       = {&Prefix}Lager.SVK        + (TransLogg.Antall * (IF Translogg.VVarekost = ? THEN 0 ELSE Translogg.VVareKost))
        {&Prefix}Lager.VerdiSolgt = {&Prefix}Lager.VerdiSolgt +
                           (
                            (TransLogg.Pris - Translogg.RabKr) -
                            TransLogg.Mva
                           ) * TransLogg.Antall.
      IF "{&Prefix}" = "St" THEN
      DO:
          ASSIGN
              piPris = (TransLogg.Pris - Translogg.RabKr) - TransLogg.Mva
              StLager.vSnittKostPris = StLager.vSnittKostPris + (TransLogg.Antall * piPris)
              .
      END.
  END.
  WHEN 11 THEN
      ASSIGN  /* Internt forbruk */
        {&Prefix}Lager.LagAnt   = {&Prefix}Lager.LagAnt    - TransLogg.Antall
        {&Prefix}Lager.IntAnt   = {&Prefix}Lager.IntAnt    + TransLogg.Antall
        {&Prefix}Lager.IntVerdi = {&Prefix}Lager.IntVerdi  +
                           ((IF Translogg.VVarekost = ? THEN 0 ELSE TransLogg.VVareKost) * TransLogg.Antall).
                           
END CASE.

/* Flagger at transen trekker lager negativ på butikk. */
IF "{&Prefix}" = "" THEN
DO:
    IF Lager.LagAnt < 0 THEN 
        TransLogg.NegLager = 1.
END.

/* Ikke lagerstyrte artikler */
IF ArtBas.Lager = FALSE OR ArtBAs.OPris = TRUE THEN
IKKE_LAGERSTYRT:
DO:
    ASSIGN
        {&Prefix}Lager.LagAnt = 0.
END. /* IKKE_LAGERSTYRT */

