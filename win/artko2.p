/************************************************************
    Program:  artko2.p
    Created:  TN   23 May 98
Description:  Starter artikkelkort fra meny.

Last change:  TN   17 Jun 98    8:21 pm
************************************************************/

DEF INPUT  PARAMETER parInnData as CHAR NO-UNDO.
                                                           
DEFINE VAR hMenu AS HANDLE NO-UNDO.

hMenu = SOURCE-PROCEDURE.

FUNCTION EmbedMe RETURNS LOGICAL (INPUT ihArtBas AS HANDLE):
  DYNAMIC-FUNCTION("EmbedMe" IN hMenu,ihArtBas) NO-ERROR.
END FUNCTION.

/* FIND FIRST ArtBas WHERE ArtBas.Opris = FALSE NO-LOCK NO-ERROR. */
FIND LAST ArtBas USE-INDEX artikkelnr NO-LOCK NO-ERROR.

IF NOT AVAIL ArtBas THEN
    FIND FIRST ArtBas NO-LOCK WHERE
      ArtBas.Vg > 0 and
      ArtBas.LopNr > 0 NO-ERROR.
IF AVAILABLE ArtBas then
  run w-vartkor.w PERSISTENT (input RECID(ArtBAs), input "ENDRE").
else
  run w-vartkor.w PERSISTENT (input ?, input "ENDRE").

