/************************************************************
    Program:  c_w_trg.i
    Created:  TN    7 Nov 98
Description:  Create og write trigger for setting av ny og
              endret informasjon.

Last change:  TN    7 Nov 98    9:48 am
************************************************************/
&IF "{&Type}" =  "C" &THEN
  assign
    {&Fil}.RegistrertDato = today
    {&Fil}.RegistrertTid  = time
    {&Fil}.RegistrertAv   = userid("skotex").
&ELSE
  assign
    {&Fil}.EDato    = today
    {&Fil}.ETid     = time
    {&Fil}.BrukerId = userid("skotex").
&ENDIF



