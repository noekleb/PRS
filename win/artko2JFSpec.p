/************************************************************
    Program:  artko2.p
    Created:  TN   23 May 98
Description:  Starter artikkelkort fra meny.

Last change:  TN   17 Jun 98    8:21 pm
************************************************************/

DEF INPUT PARAMETER parInnData as CHAR NO-UNDO.

FIND FIRST ArtBas NO-LOCK WHERE
  ArtBas.Vg > 0 and
  ArtBas.LopNr > 0 NO-ERROR.
IF AVAILABLE ArtBas then
  run w-vartkorJFSpec.w PERSISTENT (input RECID(ArtBAs), input "ENDRE").
else
  run w-vartkorJFSpec.w PERSISTENT (input ?, input "ENDRE").

