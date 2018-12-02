/************************************************************
    Program:  diahelp.i
    Created:  TN   30 Mai 2002
Description:  Felles include for hjelp i dialogbosker - ADM2.

************************************************************/

DO:
  if VALID-HANDLE(h_dproclib) then
      run hjelp IN h_dproclib 
          (STRING(KEYLABEL(LASTKEY) = "CTRL-F1","M/H"),ENTRY(1,THIS-PROCEDURE:FILE-NAME,".")).
END.
