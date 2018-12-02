/************************************************************
    Program:  artkor.p
    Created:  TN   23 May 98
Description:  Starter artikkelkortet med f›rste artikkel.

Last change:  TN   17 Jun 98    7:50 pm
************************************************************/
FIND  FIRST ArtBas NO-LOCK WHERE
  ArtBas.Vg > 0 and
  ArtBas.LopNr > 0 NO-ERROR.
IF AVAILABLE ArtBas then
  run w-vartkor PERSISTENT (input recid(ArtBas), "ENDRE").
else
  run w-vartkor PERSISTENT (input ?, "NY").

