/************************************************************
    Program:  bbrukergrp.p
    Created:  TN   16 Dec 99
Description:

Last change:  TN   16 Dec 99   11:53 pm
************************************************************/

DEF VAR wInt  as INT NO-UNDO.
DEF VAR wChar as CHAR NO-UNDO.

/* Skal startes som vedlikehold */
assign
  wChar = "V".

RUN D-bbrukergrp.w (wInt, wChar).
