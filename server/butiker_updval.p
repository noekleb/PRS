DEF INPUT PARAM  irRowId       AS ROWID NO-UNDO.
DEF INPUT PARAM  icValueFields AS CHAR NO-UNDO.
DEF INPUT PARAM  icValues      AS CHAR NO-UNDO.
DEF INPUT PARAM  icSessionId   AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocError       AS CHAR NO-UNDO.

IF NOT LOGICAL(ENTRY(LOOKUP("Sentrallager",icValueFields),icValues,"|")) THEN DO:
  IF CAN-FIND(FIRST Butiker
              WHERE Butiker.clButikkNr = INT(ENTRY(LOOKUP("Butik",icValueFields),icValues,"|"))
                AND Butiker.clButikkNr NE Butiker.Butik) THEN
    ocError = "Butikken må være sentrallager så lenge det fins tilknyttede butikker".
  ELSE DO:
    FIND Butiker WHERE Butiker.Butik = INT(ENTRY(LOOKUP("clButikkNr",icValueFields),icValues,"|"))
         AND Butiker.Sentrallager
         NO-LOCK NO-ERROR.
    IF NOT AVAIL Butiker THEN
      ocError = "Ugyldig sentrallager".
  END.
END.
