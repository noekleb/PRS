DEF INPUT  PARAM irVarebehLinjeTrans AS ROWID NO-UNDO.
DEF INPUT  PARAM icSessionId         AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocValue             AS CHAR NO-UNDO.

FIND VarebehLinjeTrans WHERE ROWID(VarebehLinjeTrans) = irVarebehLinjeTrans NO-LOCK NO-ERROR.
IF AVAIL VarebehLinjeTrans THEN DO:
  FIND VarebehLinje 
       WHERE VarebehLinje.ArtikkelNr = VarebehLinjeTrans.Artikkelnr
         AND VarebehLinje.VarebehNr  = VarebehLinjeTrans.VarebehNr
       NO-LOCK NO-ERROR.
  IF AVAIL VarebehLinje THEN DO:
    ocValue = STRING((VarebehLinjeTrans.Bestilt1 +
                      VarebehLinjeTrans.Bestilt2 + 
                      VarebehLinjeTrans.Bestilt3 +
                      VarebehLinjeTrans.Bestilt4) * VarebehLinje.Varekost).
  END.
END.
