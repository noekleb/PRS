TRIGGER PROCEDURE FOR CREATE OF OnLineLeverandor.

{trg\c_w_trg.i &Fil=OnLineLeverandor &TYPE=C}

DEFINE BUFFER trgOnLineLeverandor FOR OnLineLeverandor.

DEFINE VARIABLE trgOnLineLevnr AS INTEGER NO-UNDO.

FIND LAST trgOnLineLeverandor NO-LOCK NO-ERROR.
IF AVAILABLE trgOnLineLeverandor
  THEN trgOnLineLevnr = OnLineLeverandor.OnLineLevnr + 1.
ELSE trgOnLineLevnr = 1.

ASSIGN
  OnLineLeverandor.OnLineLevnr = trgOnLineLevnr
  .

