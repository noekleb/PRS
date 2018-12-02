def var wSkriv as log  no-undo.
def var wAnt   as int  no-undo.
def var wLimit as int  no-undo.
def var wTekst as char no-undo.

{plukkliste.i &New = "New"}

/* Sjekker om plukklisteutskrift skal kjøres. */
{syspara.i 200 1 100 wTekst}
if not can-do("JA,True,Yes",wTekst) then
  return no-apply.

assign wLimit = 0.
{syspara.i 200 1 102 wLimit int}

run sjekk-tick-row.p (output wSkriv, wLimit).
if wSkriv then
  do:
    run oppr-tick-row.p.
    run skriv-tick-row.p.
  end.
