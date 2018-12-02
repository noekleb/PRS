/************************************************************
    Program:  gridlager.p
    Created:  TN   19 Apr 99
Description:

Last change:  TN   19 Apr 99   10:56 pm
************************************************************/

FIND FIRST ArtBas NO-LOCK where ArtBas.StrTypeId > 1 NO-ERROR.

if available ArtBas then
  run w-gridlager.w PERSISTENT (input recid(ArtBas), "LAGER").

