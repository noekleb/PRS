/************************************************************
    Program:  bhandtering.p
    Created:  TN   3 Dex 00
Description:

************************************************************/

DEF VAR wInt  as INT NO-UNDO.
DEF VAR wChar as CHAR NO-UNDO.

/* Skal startes som vedlikehold */
assign
  wChar = "V".

RUN d-bhandtering.w (wInt, wChar).
