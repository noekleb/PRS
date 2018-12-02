/************************************************************
    Program:  startpersistent.p
    Created:  TN   15 Sep 98
Description:  Starter søkeliste programmer fra meny persistent.

Last change:  TN    2 Dec 99    5:29 pm
************************************************************/

DEF INPUT PARAMETER wInnData as CHAR NO-UNDO.

DEF VAR wInt  as INT  no-undo.
DEF VAR wChar as CHAR NO-UNDO.
DEF VAR wDate as DATE NO-UNDO.

IF CAN-DO("INT",TRIM(ENTRY(2,wInnData))) then
  RUN VALUE(ENTRY(1,wInnData)) persistent (INPUT-OUTPUT wInt).  ELSE
IF CAN-DO("CHAR",TRIM(ENTRY(2,wInnData))) then
  RUN VALUE(ENTRY(1,wInnData)) PERSISTENT (INPUT-OUTPUT wChar). ELSE
IF CAN-DO("DATE",TRIM(ENTRY(2,wInnData))) then
  RUN VALUE(ENTRY(1,wInnData)) PERSISTENT (INPUT-OUTPUT wDate).
