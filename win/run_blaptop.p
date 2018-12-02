/************************************************************
    Program:  run_blaptop.p
    Created:  TN   13 May 08
Description:  Starter artikkelkort fra meny.

Last change:  
************************************************************/

DEF INPUT PARAMETER parInnData as CHAR NO-UNDO.

DEF VAR piType AS INT INITIAL 31 NO-UNDO.

RUN d-blaptop.w (INPUT-OUTPUT piType).

