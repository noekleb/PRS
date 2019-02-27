/************************************************************
    Program:  diahelp.i
    Created:  TN   22 Aug 98
Description:  Felles include for hjelp i dialogbosker.

Last change:  TN   24 Sep 98   10:24 pm
************************************************************/

DO:
  if VALID-HANDLE(wLibHandle) then
    RUN CtrlHjelp IN wLibHandle.
END.
