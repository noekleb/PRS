/************************************************************
    Program:  bshode.p
    Created:  TN   15 Sep 98
Description:  Starter browser for systemparametre - moduler.

Last change:  TN   30 Oct 99   12:23 pm
************************************************************/
DEF VAR hParent AS HANDLE NO-UNDO.

hParent = SOURCE-PROCEDURE.

FUNCTION EmbedMe RETURNS LOGICAL (INPUT ihProc AS HANDLE):
  IF CAN-DO(hParent:INTERNAL-ENTRIES,"EmbedMe") THEN
    DYNAMIC-FUNCTION("EmbedMe" IN hParent,ihProc).
END FUNCTION.

DEF VAR wSysHId as INT.
RUN w-bsyshode.w persistent (INPUT-OUTPUT wSysHId).
