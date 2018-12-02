/************************************************************
    Program:  visbilde.i
    Created:  TN   14 Mar 100
Description:

Last change:  TN   14 Mar 100    5:58 pm
************************************************************/

def input parameter ipMode      as int no-undo.

DEF VAR             ipBildNr    as INT NO-UNDO.
def var             wBildePeker as char no-undo.

assign
  ipBildNr = {&BildNr}.

find BildeRegister no-lock where
  BildeRegister.BildNr = ipBildNr no-error.

assign
  wBildePeker = if available BildeRegister
                  then BildeRegister.FilNavn
                  else "".

if valid-handle(wLibHandle) then
 run HentBildePeker in wLibHandle (input ipBildNr, input ipMode, wBildePeker, output wBildePeker).

/* Nullstiller memory mellom hver lasting av bilde. */
{&BldOcx}:Picbuf:clear(2).

IF ipBildNr <> 0 OR SEARCH(wBildePeker) <> ? THEN
DO:
    assign
      {&BldOcx}:Picbuf:filename  = search(wBildePeker)
      {&BldOcx}:Picbuf:AutoScale = True no-error.
    assign
        wOk = {&BldOcx}:Picbuf:LOAD NO-ERROR.
END.


