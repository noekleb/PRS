/************************************************************
    Program:  run_bht-type.p
    Created:  TN   13 May 08
Description:  Starter artikkelkort fra meny.

Last change:  
************************************************************/

DEF INPUT PARAMETER parInnData as CHAR NO-UNDO.

DEF VAR piInt AS INT NO-UNDO.

RUN d-bsasong.p (INPUT-OUTPUT piInt).

