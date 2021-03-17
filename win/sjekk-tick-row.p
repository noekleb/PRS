/************************************************************
    Program:  sjekk-tick-row.p
    Created:  TN   21 Dec 99
Description:

  Teller antall transaksjoner som skal plukkes.
  Er det mer en 150 flagges dette til kallende rutine.

Last change:  TN   28 Dec 99    2:27 am
************************************************************/

DEF OUTPUT PARAMETER wSkriv as LOG NO-UNDO.
DEF input  parameter wLimit as INT NO-UNDO.

DEF VAR wGrpListe  as CHAR NO-UNDO.
DEF VAR wButikkLst as CHAR NO-UNDO.
DEF VAR wAntall    as INT  NO-UNDO.
DEF VAR wLoop      as INT  NO-UNDO.

{syspara.i 200 1 105 wButikkLst}

assign
  wSkriv     = FALSE.


BUTIKKLOOP:
do wLoop = 1 TO NUM-ENTRIES(wButikkLst):
  TRANSLOOP:
  FOR EACH TransLogg no-LOCK where
    TransLogg.Plukket = FALSE and
    TransLogg.Butik = INT(ENTRY(wLoop,wButikkLst)):

    assign
      wAntall = wAntall + 1.

    IF wAntall > wLimit then
      DO:
        wSkriv = TRUE.
        LEAVE BUTIKKLOOP.
      END.
  END. /* TRANSLOOP */
END. /* BUTIKKLOOP */


