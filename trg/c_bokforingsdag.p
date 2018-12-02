TRIGGER PROCEDURE FOR CREATE OF Bokforingsdag.

{trg\c_w_trg.i &Fil=Data.Bokforingsdag &TYPE=C}

def var trgBokforingsId as dec no-undo.
def buffer trgBokforingsdag for Data.Bokforingsdag.

LOOPEN:
do while true:
  trgBokforingsId = dec(SUBstring(STRING(year(today),"9999"),3,2) +
               STRING(MONTH(TODAY),"99") + 
                string(NEXT-VALUE(BokforingsId,Data),"99999999")).
  if not can-find(first trgBokforingsdag where 
                  trgBokforingsdag.BokforingsId = trgBokforingsId) then
    do:
      assign
        Data.Bokforingsdag.BokforingsId  = trgBokforingsId
        NO-ERROR.

      if error-status:error = true
        then next LOOPEN.
      else 
        leave LOOPEN.
    end. 
end. /* LOOPEN */


