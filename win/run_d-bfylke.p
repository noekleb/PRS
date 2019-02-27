/************************************************************
    Program:  run_bht-type.p
    Created:  TN   13 May 08
Description:  Starter artikkelkort fra meny.

Last change:  
************************************************************/

DEF INPUT PARAMETER parInnData AS CHAR NO-UNDO.

DEF VAR piChar AS INT NO-UNDO.

RUN d-bfylke.p (INPUT-OUTPUT piChar).

