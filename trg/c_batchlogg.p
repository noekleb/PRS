TRIGGER PROCEDURE FOR CREATE OF SkoTex.BatchLogg.

def var wBatchNr   as int no-undo.
def var trgBatchNr as int no-undo.

def buffer trgBatchLogg for SkoTex.BatchLogg.

find last trgBatchLogg no-lock no-error.
if available trgBatchLogg then
DO:
    wBatchNr = trgBatchLogg.BatchNr.
    RELEASE trgBatchLogg.
END.
else
  wBatchNr = 0.

LOOPEN:
do while true:
  trgBatchNr = NEXT-VALUE(BatchLogg,SkoTex).
  
  /* Hvis sekvensen er ute av synk. */
  if trgBatchNr <= wBatchNr then
    next LOOPEN. /* Nummeret er allerede benyttet. */

  /* Går ut på ledig nummer funnet */
  if not can-find(trgBatchLogg where 
                    trgBatchLogg.BatchNr = trgBatchNr) then
    leave LOOPEN.
end. /* LOOPEN */

assign
  SkoTex.BatchLogg.BatchNr        = trgBatchNr
  SkoTex.BatchLogg.RegistrertDato = today
  SkoTex.BatchLogg.RegistrertTid  = time
  SkoTex.BatchLogg.RegistrertAV   = userid("skotex").


