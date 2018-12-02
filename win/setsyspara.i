/************************************************************
    Program:  syspara.i
    Created:  TN    5 Oct 98
Description:  Henter og setter parameter 1 fra syspara.

     Endret   Sign Merknad
     -------- ---- -----------------------------------------
     13/11-98 TN   Lagt inn &Type

Last change:  TN    4 Nov 99    6:40 pm
************************************************************/

DO:
  FIND {&6}SysPara EXCLUSIVE-LOCK where
    {&6}SysPara.SysHId = {1} and
    {&6}SysPara.SysGr  = {2} and
    {&6}SysPara.ParaNr = {3} NO-ERROR.
  if AVAILABLE {&6}SysPara then
    DO:
      ASSIGN  
          {&6}SysPara.Parameter1  = STRING({4})
          {&6}SysPara.Beskrivelse = (IF "{&7}" <> ""
                                       THEN "{&7}"
                                       ELSE {&6}SysPara.Beskrivelse)
          .
      RELEASE {&6}SysPara.
    END.
END.
