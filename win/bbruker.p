/************************************************************
    Program:  bbruker.p
    Created:  TN   16 Dec 99
Description:

Last change:  TN   17 Dec 99    0:00 am
************************************************************/

DEF VAR wInt  as INT NO-UNDO.
DEF VAR wChar as CHAR NO-UNDO.

/* Skal startes som vedlikehold */
assign
  wChar = "V".

RUN D-bbruker.w (wInt, wChar).
