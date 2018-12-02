TRIGGER PROCEDURE FOR CREATE OF Skift.

{trg\c_w_trg.i &Fil=Data.Skift &TYPE=C}

def var trgSkiftId as dec no-undo.
def buffer trgSkift for Data.Skift.

LOOPEN:
do while true:
  trgSkiftId = dec(SUBstring(STRING(year(today),"9999"),3,2) +
               STRING(MONTH(TODAY),"99") + 
                string(NEXT-VALUE(SkiftId,Data),"99999999")).
  if not can-find(first trgSkift where 
                  trgSkift.SkiftId = trgSkiftId) then
    do:
      assign
        Data.Skift.SkiftId  = trgSkiftId
        NO-ERROR.

      if error-status:error = true
        then next LOOPEN.
      else 
        leave LOOPEN.
    end. 
end. /* LOOPEN */


