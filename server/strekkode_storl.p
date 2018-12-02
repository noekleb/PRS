DEF INPUT PARAM  irBuffer      AS ROWID NO-UNDO.
DEF INPUT PARAM  icSessionId   AS CHAR  NO-UNDO.
DEF OUTPUT PARAM ocReturn      AS CHAR  NO-UNDO.

FIND Strekkode WHERE ROWID(Strekkode) = irBuffer NO-LOCK NO-ERROR.
IF AVAIL Strekkode THEN 
  FOR FIRST ArtBas FIELDS(StrTypeId) NO-LOCK
      WHERE ArtBas.ArtikkelNr = Strekkode.ArtikkelNr
     ,FIRST StrType OF ArtBas NO-LOCK:
    IF NOT CAN-DO(StrType.Fordeling,STRING(Strekkode.StrKode)) THEN
      ocReturn = "skiprow".
    ELSE 
      ocReturn = TRIM(ENTRY(LOOKUP(STRING(Strekkode.StrKode),StrType.Fordeling),StrType.AlfaFordeling)).
  END.
