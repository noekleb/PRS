def var wSkriv as log  no-undo.
def var wAnt   as int  no-undo.
def var wLimit as int  no-undo.
def var wTekst as char no-undo.
def var wSvar  as log  no-undo.
DEF VAR wLoop  as INT  NO-UNDO.

{plukkliste.i &New = "New"}

assign wSvar = false.
message "Skal plukkliste kjøres?" 
        view-as alert-box question button yes-no title "Bekreft utskrift" update wSvar.
if wSvar <> true then
  return.        

assign wLimit = 0.

run sjekk-tick-row.p (output wSkriv, wLimit).
if wSkriv then
  do:
    run oppr-tick-row.p.
    run skriv-tick-row.p.
  end.


