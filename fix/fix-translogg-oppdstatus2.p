def var flagga as logical.
def var i as int.

for each batchlogg where oppdstatus = 3 no-lock.
/*  flagga = false. */
  for each translogg of batchlogg /*no-lock.*/ exclusive-lock.
    if translogg.ttid = 1 or translogg.ttid = 10 then
    do:
     /* flagga = true.*/
      assign translogg.postert = false. /**/
    end.
  end.
/*  if flagga = true then batchlogg.oppdstatus = 3.*/
  i = i + 1.
  if i modulo 100 = 0 then display i.
  pause 0.
end.
