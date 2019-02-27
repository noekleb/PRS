TRIGGER PROCEDURE FOR CREATE OF BongHode.

def var trgB_Id as dec no-undo.
def buffer trgBongHode for Data.BongHode.

LOOPEN:
do while true:
  trgB_Id = dec(SUBstring(STRING(year(today),"9999"),3,2) +
            STRING(MONTH(TODAY),"99") + 
                string(NEXT-VALUE(BongId,Data),"99999999")).
  if not can-find(first trgBongHode where 
                  trgBongHode.B_Id = trgB_Id) then
    do:
      assign
        Data.BongHode.B_Id  = trgB_Id
        Data.BongHode.ODato = today
        Data.BongHode.OTid  = time
        Data.BongHode.OAv   = userid("Data") no-error.

      if error-status:error = true
        then next LOOPEN.
      else 
        leave LOOPEN.
    end. 
end. /* LOOPEN */



