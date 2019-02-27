TRIGGER PROCEDURE FOR CREATE OF Datasett.

def var trgDataSettId as dec no-undo.

def buffer trgDataSett for Data.DataSett.

LOOPEN:
do while true:
  trgDataSettId = dec(SUBstring(STRING(year(today),"9999"),3,2) +
                    STRING(MONTH(TODAY),"99") + 
                    string(NEXT-VALUE(DataSettId,Data),"99999999")).
  if not can-find(first trgDataSett where 
                  trgDataSett.DataSettId = trgDataSettId) then
    do:
      assign
        Data.DataSett.DataSettId  = trgDataSettId
        /*
        Data.DataSett.ODato = today
        Data.DataSett.OTid  = time
        Data.DataSett.OAv   = userid("Data") 
        */
        no-error.

      if error-status:error = true
        then next LOOPEN.
      else 
        leave LOOPEN.
    end. 
end. /* LOOPEN */


