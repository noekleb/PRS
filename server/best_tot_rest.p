DEF INPUT PARAM  irBuffer      AS ROWID NO-UNDO.
DEF INPUT PARAM  icSessionId   AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn      AS CHAR NO-UNDO.

FOR FIRST BestHode FIELDS(TotAntPar TotInnLev TotMakulert) NO-LOCK
    WHERE ROWID(BestHode) = irBuffer:
  ocReturn = STRING(TotAntPar - TotInnLev - TotMakulert).
END.
