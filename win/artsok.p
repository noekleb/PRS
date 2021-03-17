/************************************************************
    Program:  artsok.p
    Created:  TN   23 May 98
Description:  Starter artikkelliste.

Last change:  TN   17 Jun 98    8:20 pm
************************************************************/

DEF INPUT PARAMETER parInnData as CHAR NO-UNDO.

FIND FIRST ArtBas NO-LOCK WHERE
  ArtBas.Vg > 0 and
  ArtBas.LopNr > 0 NO-ERROR.
IF AVAILABLE ArtBas then
  run w-bartsok.p PERSISTENT.
else
  run w-bartsok.p PERSISTENT.

