TRIGGER PROCEDURE FOR CREATE OF OvBunt.

DEFINE VARIABLE trgBuntNr AS INTEGER NO-UNDO.
DEFINE BUFFER trgOvBunt FOR OvBunt.

/* TN 3/6-20 Nummerserien fra 1000000000 er reservert for reservasjonsordre.               */
/* Reservasjonsordre er innført for Gant butikkene. Og legges opp lokalt i butikkene.      */
/* Ved opprettelse av reservasjonsordre, overstyres det buntNr som tildeles her i trigger. */
FIND LAST trgOvBunt NO-LOCK WHERE 
  trgOvbunt.buntNr < 1000000000 
  USE-INDEX BuntNr NO-ERROR.
  
IF AVAILABLE trgOvbunt THEN 
  trgBuntNr = trgOvbunt.BuntNr + 1.
ELSE 
  trgBuntNr = 1.

ASSIGN 
  OvBunt.BuntNr = trgBuntNr.
  
{trg/c_w_trg.i &Type="C" &Fil="SkoTex.OvBunt"}


