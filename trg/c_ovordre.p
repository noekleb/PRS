TRIGGER PROCEDURE FOR CREATE OF SkoTex.OvOrdre.

def var trgOvOrdreId  as int no-undo.
def buffer trgOvOrdre for SkoTex.OvOrdre.

LOOPEN:
do while true:
  trgOvOrdreId = NEXT-VALUE(Overforingsordre,SkoTex).
  if not can-find(first trgOvOrdre where trgOvOrdre.OvORdreId =
                  trgOvOrdreId) then
    leave LOOPEN.
end. /* LOOPEN */


assign
  SkoTex.OvOrdre.OvOrdreId      = trgOvOrdreId
  SkoTex.OvOrdre.RegistrertDato = today
  SkoTex.OvOrdre.RegistrertTid  = time
  SkoTex.OvOrdre.RegistrertAV   = userid("skotex").


