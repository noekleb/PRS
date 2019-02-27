/************************************************************
    Program:  fixinitakkumlager.i
    Created:  TN   16 Feb 100
Description:  Akkumulerer opp data i nye lagertabeller.

Last change:  TN   16 Feb 100   11:01 am
************************************************************/

DO:
  /* Beregner ny varekost */
  assign
    wWork  = {&Prefix}Lager.VVarekost * {&Prefix}Lager.LagAnt /* Gammel lagerverdi */
    wWork2 = Lager.VVarekost          * Lager.LagAnt          /* Verdi av innkjøp  */
    wWork3 = (wWork + wWork2) / (Lager.LagAnt + {&Prefix}Lager.LagAnt)
    wWork3 = if wWork3 = ? then 0 else wWork3.

  ASSIGN
    {&Prefix}Lager.VVarekost      = wWork3
    {&Prefix}Lager.LagAnt         = {&Prefix}Lager.LagAnt         + Lager.LagAnt
    {&Prefix}Lager.SistInnlevert  = ?
    {&Prefix}Lager.AntSolgt       = {&Prefix}Lager.AntSolgt       + Lager.AntSolgt
    {&Prefix}Lager.BrekkAnt       = {&Prefix}Lager.BrekkAnt       + Lager.BrekkAnt
    {&Prefix}Lager.IntAnt         = {&Prefix}Lager.IntAnt         + Lager.IntAnt
    {&Prefix}Lager.ReklAnt        = {&Prefix}Lager.ReklAnt        + Lager.ReklAnt
    {&Prefix}Lager.ReklLAnt       = {&Prefix}Lager.ReklLAnt       + Lager.ReklLAnt
    {&Prefix}Lager.GjenkjopAnt    = {&Prefix}Lager.GjenkjopAnt    + Lager.GjenkjopAnt
    {&Prefix}Lager.RetLAnt        = {&Prefix}Lager.RetLAnt        + Lager.RetLAnt
    {&Prefix}Lager.KjopAnt        = {&Prefix}Lager.KjopAnt        + Lager.KjopAnt
    {&Prefix}Lager.OvAnt          = {&Prefix}Lager.OvAnt          + Lager.OvAnt.

  ASSIGN
    {&Prefix}Lager.JustAnt        = {&Prefix}Lager.JustAnt        + Lager.JustAnt
    {&Prefix}Lager.JustVerdi      = {&Prefix}Lager.JustVerdi      + Lager.JustVerdi
    {&Prefix}Lager.SvinnAnt       = {&Prefix}Lager.SvinnAnt       + Lager.SvinnAnt
    {&Prefix}Lager.SvinnVerdi     = {&Prefix}Lager.SvinnVerdi     + Lager.SvinnVerdi
    {&Prefix}Lager.NedAnt         = {&Prefix}Lager.NedAnt         + Lager.NedAnt
    {&Prefix}Lager.NedVerdi       = {&Prefix}Lager.NedVerdi       + Lager.NedVerdi
    {&Prefix}Lager.VerdiSolgt     = {&Prefix}Lager.VerdiSolgt     + Lager.VerdiSolgt
    {&Prefix}Lager.KjopVerdi      = {&Prefix}Lager.KjopVerdi      + Lager.KjopVerdi
    {&Prefix}Lager.BrekkVerdi     = {&Prefix}Lager.BrekkVerdi     + Lager.BrekkVerdi
    {&Prefix}Lager.IntVerdi       = {&Prefix}Lager.IntVerdi       + Lager.IntVerdi
    {&Prefix}Lager.ReklVerdi      = {&Prefix}Lager.ReklVerdi      + Lager.ReklVerdi
    {&Prefix}Lager.ReklLVerdi     = {&Prefix}Lager.ReklLVerdi     + Lager.ReklLVerdi
    {&Prefix}Lager.GjenkjopVerdi  = {&Prefix}Lager.GjenkjopVerdi  + Lager.GjenkjopVerdi
    {&Prefix}Lager.OvVerdi        = {&Prefix}Lager.OvVerdi        + Lager.OvVerdi
    {&Prefix}Lager.VerdiRabatt    = {&Prefix}Lager.VerdiRabatt    + Lager.VerdiRabatt
    {&Prefix}Lager.AntRab         = {&Prefix}Lager.AntRab         + Lager.AntRab.
END.