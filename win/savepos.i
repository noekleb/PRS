/* Programnavn:    savepos.i
   Dato       :    3/7-98
   Forfatter  :    T.N›kleby
   System     :    SkoTex
   Funksjoner :    Lagrer vindusposisjon

	Last change:  TN    5 Nov 99   11:10 am

*/

IF VALID-HANDLE(wLibHandle) then
  RUN SaveWinPos IN wLibHandle ({&WINDOW-NAME}) NO-ERROR.

