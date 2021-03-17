/************************************************************
    Program:  fixstrartlag.i
    Created:  TN   28 Mar 99
Description:

Last change:  TN   28 Mar 99    5:06 pm
************************************************************/

DO:
  ASSIGN
    {1}.salsum      = {1}.salsum      + {2}.salsum
    {1}.salant      = {1}.salant      + {2}.salant
    {1}.kopant      = {1}.kopant      + {2}.kopant
    {1}.kopsum      = {1}.kopsum      + {2}.kopsum
    {1}.lagant      = {1}.lagant      + {2}.lagant
    {1}.bestant     = {1}.bestant     + {2}.bestant
    {1}.overfort    = {1}.overfort    + {2}.overfort
    {1}.primo       = {1}.primo       + {2}.primo
    {1}.retant      = {1}.retant      + {2}.retant
    {1}.rekant      = {1}.rekant      + {2}.rekant.

  ASSIGN
    {1}.invant      = {1}.invant      + {2}.invant
    {1}.AntSolgt    = {1}.AntSolgt    + {2}.AntSolgt
    {1}.BrekkAnt    = {1}.BrekkAnt    + {2}.BrekkAnt
    {1}.IntAnt      = {1}.IntAnt      + {2}.IntAnt
    {1}.ReklAnt     = {1}.ReklAnt     + {2}.ReklAnt
    {1}.ReklLAnt    = {1}.ReklLAnt    + {2}.ReklLAnt
    {1}.GjenkjopAnt = {1}.GjenkjopAnt + {2}.GjenkjopAnt
    {1}.RetLAnt     = {1}.RetLAnt     + {2}.RetLAnt
    {1}.KjopAnt     = {1}.KjopAnt     + {2}.KjopAnt
    {1}.OvAnt       = {1}.OvAnt       + {2}.OvAnt
    {1}.JustAnt     = {1}.JustAnt     + {2}.JustAnt
    {1}.JustVerdi   = {1}.JustVerdi   + {2}.JustVerdi
    {1}.SvinnAnt    = {1}.SvinnAnt    + {2}.SvinnAnt
    {1}.SvinnVerdi  = {1}.SvinnVerdi  + {2}.SvinnVerdi
    {1}.NedAnt      = {1}.NedAnt      + {2}.NedAnt
    {1}.NedVerdi    = {1}.NedVerdi    + {2}.NedVerdi.

END.