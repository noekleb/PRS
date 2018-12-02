TRIGGER PROCEDURE FOR CREATE OF Salgsenhet.

{trg\c_w_trg.i &Fil=Salgsenhet &TYPE=C}

DEFINE BUFFER trgSalgsenhet FOR SalgsEnhet.

DEFINE VARIABLE trgSalgsEnhId AS INTEGER NO-UNDO.

FIND LAST trgSalgsEnhet NO-LOCK NO-ERROR.
IF AVAILABLE trgSalgsEnhet
  THEN trgSalgsEnhId = trgSalgsEnhet.SalgsEnhId + 1.
ELSE trgSalgsEnhId = 1.

ASSIGN
  SalgsEnhet.SalgsEnhId = trgSalgsEnhId
  .


