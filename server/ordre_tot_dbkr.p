DEF INPUT PARAM  irBuffer      AS ROWID NO-UNDO.
DEF INPUT PARAM  icSessionId   AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn      AS CHAR NO-UNDO.

DEF VAR fTotAnt AS DEC NO-UNDO.

FOR FIRST Ordre FIELDS(OrdreNr) NO-LOCK
    WHERE ROWID(Ordre) = irBuffer
    ,EACH BestHode FIELDS(TotDbKr) NO-LOCK
          OF Ordre:
  fTotAnt = fTotAnt + TotDbKr.
END.

ocReturn = STRING(fTotAnt).
