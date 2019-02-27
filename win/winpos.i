/* Programnavn:    winpos.i
   Dato       :    24/08-98
   Forfatter  :    TN
   System     :    SkoTex
   Funksjoner :    - Tar vare p† propath og plasserer evt. vinduet der det sist
                     befant seg.
                   - Legger p† iconfil p† vinduet.


	Last change:  TN    5 Nov 99   11:02 am

*/

DEF VAR wInifil  as CHAR NO-UNDO.

/* Setter vinduets NAVN */
assign
  {&WINDOW-NAME}:NAME = "{&WindowName} ".

if VALID-HANDLE(wLibHandle) then
  /* RUN LoadIconFil IN wLibHandle (input {&WINDOW-NAME}:handle) NO-ERROR. */
  RUN LoadIconFil IN wLibHandle (input current-window) NO-ERROR.

if VALID-HANDLE(wLibHandle) then
  /*RUN GetWinPos IN wLibHandle ({&WINDOW-NAME}) NO-ERROR.*/
  RUN GetWinPos IN wLibHandle (current-window) NO-ERROR.
STATUS INPUT   OFF.
STATUS DEFAULT " ".
