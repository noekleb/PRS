/************************************************************
    Program:  syspara.i
    Created:  TN    11 Jan 99
Description:  Henter og setter parameter 2 fra syspara.

     Endret   Sign Merknad
     -------- ---- -----------------------------------------

Last change:  TN   18 Sep 99    9:51 pm
************************************************************/


DO:
  FIND {&6}SysPara NO-LOCK where
    {&6}SysPara.SysHId = {1} and
    {&6}SysPara.SysGr  = {2} and
    {&6}SysPara.ParaNr = {3} NO-ERROR.
  IF AVAILABLE {&6}SysPara then
    ASSIGN {4} = {5}({&6}SysPara.Parameter2).
END.
