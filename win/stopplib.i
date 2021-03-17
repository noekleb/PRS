/************************************************************
    Program:  stopplib.i
    Created:  TN   13 Jun 98
Description:  Hvis denne proseduren er startet direkte, skal
              prosedurebibloteket stoppes. Hvis ikke, håndteres
              dette av start.p.

Last change:  TN   29 Mar 99   12:47 pm
************************************************************/

if "{&UIB_is_running}":U <> "":U or
   "{&Stopp}" = "Stopp":U then
  DO:
    IF VALID-HANDLE(wLibHandle) then DELETE PROCEDURE wLibHandle.
    IF VALID-HANDLE(wWindows)   then DELETE PROCEDURE wWindows.
    IF VALID-HANDLE(wWinfunc)   then DELETE PROCEDURE wWinfunc.
    IF VALID-HANDLE(wProExtra)  then DELETE PROCEDURE wProExtra.
  END.
