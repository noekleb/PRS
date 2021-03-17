/************************************************************
    Program:  opprettovordre.p
    Created:  TN    1 Sep 100
Description:  Dette programmet oppretter en ny overføringsordre.
              Det returnerer overføringsordrenummeret til
              den kallende rutinen.
              Rutinen har ingen parametre med seg inn.

Last change:  TN    1 Sep 100   10:36 am
************************************************************/

DEF OUTPUT PARAMETER iOvOrdreId as INT NO-UNDO.

DEF BUFFER bufOvOrdre FOR OvOrdre.

do FOR bufOvOrdre TRANSACTION:
  /* Tildeling av unik id håndteres i trigger ..\trg\c_ovordre.p */
  CREATE bufOvORdre.
  assign
    iOvOrdreId = bufOvOrdre.OvOrdreId.
  RELEASE bufOvOrdre.
END. /* TRANSACTION */

