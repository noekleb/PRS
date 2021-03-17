/************************************************************
    Program:  vstrtstr.i
    Created:  TN    4 Feb 100
Description:

Last change:  TN    4 Feb 100    9:35 am
************************************************************/

assign
  wStorl = Self:screen-value.
if valid-handle(wLibHandle) then
  run FiksStorl    in wLibHandle (input-output wStorl).
assign
  Self:screen-value = wStorl.

