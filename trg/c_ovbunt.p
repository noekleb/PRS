TRIGGER PROCEDURE FOR CREATE OF OvBunt.

DEFINE VARIABLE trgBuntNr AS INTEGER NO-UNDO.
DEFINE BUFFER trgOvBunt FOR OvBunt.

FIND LAST trgOvBunt NO-LOCK USE-INDEX BuntNr NO-ERROR.
IF AVAILABLE trgOvbunt THEN 
  trgBuntNr = trgOvbunt.BuntNr + 1.
ELSE 
  trgBuntNr = 1.

ASSIGN 
  OvBunt.BuntNr = trgBuntNr.
  
{trg/c_w_trg.i &Type="C" &Fil="SkoTex.OvBunt"}


