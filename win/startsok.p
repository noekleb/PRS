/************************************************************
    Program:  startsok.p
    Created:  TN   15 Sep 98
Description:  Starter søkeliste programmer fra meny.

      Input:  Parameter 1: <Program>       Navn på program som skal startes.
                        2: <INT/CHAR/DATE> Variabeltype som skal sendes med.
                        3: PERSISTENT      Normal eller persistent start av program.

Last change:  TN   11 Feb 100   11:29 am
************************************************************/

DEF INPUT PARAMETER wInnData as CHAR NO-UNDO.

DEF VAR wInt  as INT  no-undo.
DEF VAR wChar as CHAR NO-UNDO.
DEF VAR wDate as DATE NO-UNDO.
def var wPers as log  no-undo.

if num-entries(wInnData) >= 3 then
  do:
    if entry(3,wInnData) = "PERSISTENT" then
      wPers = true.
    else
      wPers = false.
  end.
else
  wPers = false.

/* Ordinær oppstart. */
if wPers = false then
  do:
    IF CAN-DO("INT",TRIM(ENTRY(2,wInnData))) then
      RUN VALUE(ENTRY(1,wInnData)) (INPUT-OUTPUT wInt).  ELSE
    IF CAN-DO("CHAR",TRIM(ENTRY(2,wInnData))) then
      RUN VALUE(ENTRY(1,wInnData)) (INPUT-OUTPUT wChar). ELSE
    IF CAN-DO("DATE",TRIM(ENTRY(2,wInnData))) then
      RUN VALUE(ENTRY(1,wInnData)) (INPUT-OUTPUT wDate).
  end.
/* Persistent oppstart */
else do:
    IF CAN-DO("INT",TRIM(ENTRY(2,wInnData))) then
      RUN VALUE(ENTRY(1,wInnData)) persistent (INPUT-OUTPUT wInt).  ELSE
    IF CAN-DO("CHAR",TRIM(ENTRY(2,wInnData))) then
      RUN VALUE(ENTRY(1,wInnData)) persistent (INPUT-OUTPUT wChar). ELSE
    IF CAN-DO("DATE",TRIM(ENTRY(2,wInnData))) then
      RUN VALUE(ENTRY(1,wInnData)) persistent (INPUT-OUTPUT wDate).
end.