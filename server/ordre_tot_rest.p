DEF INPUT PARAM  irBuffer      AS ROWID NO-UNDO.
DEF INPUT PARAM  icSessionId   AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn      AS CHAR NO-UNDO.

DEF VAR fTotAnt AS DEC NO-UNDO.

FOR FIRST Ordre FIELDS(OrdreNr) NO-LOCK
    WHERE ROWID(Ordre) = irBuffer
    ,EACH BestHode FIELDS(TotAntPar TotInnLev TotMakulert) NO-LOCK
          OF Ordre:
  fTotAnt = fTotAnt + TotAntPar - TotInnLev - TotMakulert.
END.

ocReturn = STRING(fTotAnt).
