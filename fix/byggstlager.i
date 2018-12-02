/* 20/05/00 COPY assignment */

DO:
  /*
  /* Her beregnes vektet kostpris varer på lager. */
  IF {1}.LagAnt > 0 AND {2}.LagAnt > 0 THEN
  DO:
      ASSIGN
        wWork1 = ({1}.LagAnt * {1}.VVareKost) /* Gammel lagerverdi */
        wWork2 = ({2}.LagAnt * {2}.VVareKost) /* Verdi av tilf›rt lager */
        wWork3 = (wWork1 + wWork2) / ({1}.LagAnt + {2}.LagAnt)
        wWork3 = if wWork3 = ? then MAXIMUM({1}.VVareKost,{2}.VVareKost) else wWork3
        {1}.VVareKost = wWork3 /* Setter ny vektet snittpris */
        .
  END.
  /* Det ligger ingenting fra før. Nytt skal bare inn. */
  ELSE DO:
      IF {1}.LagAnt <= 0 AND {2}.LagAnt > 0 THEN
          {1}.VVareKost = {2}.VVareKost.
  END.
  */ 

  /* Akkumulerer bare lager > 0. Akkumulerer varekost. */
  /* Akkumulerer opp solgte varers kostpris - {1}.vSnittKostPris. */
/*IF {2}.LagAnt > 0 THEN*/
  ASSIGN
    {1}.LagAnt         = {1}.LagAnt         + {2}.LagAnt      
    {1}.VVareKost      = {1}.VVareKost      + ({2}.LagAnt * ABS({2}.vVareKost))
    {1}.vSnittKostPris = {1}.vSnittKostPris + ({2}.AntSolgt * {2}.vVareKost)
    .

  ASSIGN
    {1}.SVK            = {1}.SVK            + {2}.SVK
    {1}.AntSolgt       = {1}.AntSolgt       + {2}.AntSolgt
    {1}.VerdiSolgt     = {1}.VerdiSolgt     + {2}.VerdiSolgt
    {1}.IntAnt         = {1}.IntAnt         + {2}.IntAnt
    {1}.ReklAnt        = {1}.ReklAnt        + {2}.ReklAnt
    {1}.ReklLAnt       = {1}.ReklLAnt       + {2}.ReklLAnt
    {1}.RetLAnt        = {1}.RetLAnt        + {2}.RetLAnt
    {1}.OvAnt          = {1}.OvAnt          + {2}.OvAnt
    {1}.JustAnt        = {1}.JustAnt        + {2}.JustAnt
    {1}.JustVerdi      = {1}.JustVerdi      + {2}.JustVerdi
    {1}.SvinnAnt       = {1}.SvinnAnt       + {2}.SvinnAnt
    {1}.SvinnVerdi     = {1}.SvinnVerdi     + {2}.SvinnVerdi
    {1}.NedAnt         = {1}.NedAnt         + {2}.NedAnt
    {1}.NedVerdi       = {1}.NedVerdi       + {2}.NedVerdi
    {1}.KjopAnt        = {1}.KjopAnt        + {2}.KjopAnt
    {1}.KjopVerdi      = {1}.KjopVerdi      + {2}.KjopVerdi
    {1}.BrekkAnt       = {1}.BrekkAnt       + {2}.BrekkAnt
    {1}.BrekkVerdi     = {1}.BrekkVerdi     + {2}.BrekkVerdi
    {1}.IntVerdi       = {1}.IntVerdi       + {2}.IntVerdi
    {1}.ReklVerdi      = {1}.ReklVerdi      + {2}.ReklVerdi
    {1}.ReklLVerdi     = {1}.ReklLVerdi     + {2}.ReklLVerdi
    {1}.GjenkjopAnt    = {1}.GjenkjopAnt    + {2}.GjenkjopAnt
    {1}.GjenkjopVerdi  = {1}.GjenkjopVerdi  + {2}.GjenkjopVerdi
    {1}.OvVerdi        = {1}.OvVerdi        + {2}.OvVerdi
    {1}.VerdiRabatt    = {1}.VerdiRabatt    + {2}.VerdiRabatt
    {1}.AntRab         = {1}.AntRab         + {2}.AntRab.

/*     assign                                                                       */
/*       wWork1 = ABSOLUTE({1}.LagAnt * {1}.VVareKost) /* Gammel lagerverdi */      */
/*       wWork2 = ABSOLUTE({2}.LagAnt * {2}.VVareKost) /* Verdi av tilf›rt lager */ */
/*       wWork3 = (wWork1 + wWork2) / (ABSOLUTE({1}.LagAnt) + ABSOLUTE({2}.LagAnt)) */
/*       wWork3 = if wWork3 = ? then {1}.VVareKost else wWork3.                     */

END.
