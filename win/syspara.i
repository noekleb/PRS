/************************************************************
    Program:  syspara.i
    Created:  TN    5 Oct 98
Description:  Henter og setter parameter 1 fra syspara.

     Endret   Sign Merknad
     -------- ---- -----------------------------------------
     13/11-98 TN   Lagt inn &Type

Last change:  TN   18 Sep 99    9:47 pm
************************************************************/

do:
  FIND {&6}SysPara NO-LOCK where
    {&6}SysPara.SysHId = {1} and
    {&6}SysPara.SysGr  = {2} and
    {&6}SysPara.ParaNr = {3} NO-ERROR.
  if AVAILABLE {&6}SysPara then
    ASSIGN {4} = {5}({&6}SysPara.Parameter1).
end.
