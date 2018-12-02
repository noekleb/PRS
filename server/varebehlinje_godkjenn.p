DEF INPUT PARAM  icRowId       AS CHAR NO-UNDO.
DEF INPUT PARAM  icValueFields AS CHAR NO-UNDO.
DEF INPUT PARAM  icValues      AS CHAR NO-UNDO.
DEF INPUT PARAM  icSessionId   AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocError       AS CHAR NO-UNDO.

FIND VarebehLinje WHERE ROWID(VarebehLinje) = TO-ROWID(icRowid) NO-LOCK NO-ERROR.
IF AVAIL VarebehLinje THEN 
  FOR EACH VarebehLinjeTrans EXCLUSIVE-LOCK
      WHERE VarebehLinjeTrans.VarebehNr  = VarebehLinje.VarebehNr
        AND VarebehLinjeTrans.ButikkNr   = INT(ENTRY(LOOKUP("UpdButikknr",icValueFields),icValues,"|"))
        AND VarebehLinjeTrans.ArtikkelNr = VarebehLinje.ArtikkelNr
        AND (bestilt1 > 0 OR bestilt2 > 0 OR bestilt3 > 0 OR bestilt4 > 0)
        :
    VarebehLinjeTrans.GodkjentBestilling = LOGICAL(ENTRY(LOOKUP("GodkjentBest",icValueFields),icValues,"|")).
  END.

