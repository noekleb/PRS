DEF INPUT PARAM  icRowId       AS CHAR NO-UNDO.
DEF INPUT PARAM  icValueFields AS CHAR NO-UNDO.
DEF INPUT PARAM  icValues      AS CHAR NO-UNDO.
DEF INPUT PARAM  icSessionId   AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocError       AS CHAR NO-UNDO.

DEF BUFFER bVarebehLinjeTrans FOR VarebehLinjeTrans.

FIND VarebehLinjeTrans WHERE ROWID(VarebehLinjeTrans) = TO-ROWID(icRowid) NO-LOCK NO-ERROR.
IF AVAIL VarebehLinjeTrans THEN 
  FOR EACH bVarebehLinjeTrans EXCLUSIVE-LOCK
      WHERE bVarebehLinjeTrans.VarebehNr  = VarebehLinjeTrans.VarebehNr
        AND bVarebehLinjeTrans.ButikkNr   = VarebehLinjeTrans.ButikkNr
        AND bVarebehLinjeTrans.ArtikkelNr = VarebehLinjeTrans.ArtikkelNr
        :
    IF VarebehLinjeTrans.Bestilt1 > 0 OR VarebehLinjeTrans.Bestilt2 > 0 OR
       VarebehLinjeTrans.Bestilt3 > 0 OR VarebehLinjeTrans.Bestilt4 > 0 THEN
      ASSIGN bVarebehLinjeTrans.GodkjentBestilling   = ENTRY(LOOKUP("levnrlist",icValueFields),icValues,"|") = "*"
             bVarebehLinjeTrans.RegistrertBestilling = YES.
    ELSE
      ASSIGN bVarebehLinjeTrans.GodkjentBestilling   = NO
             bVarebehLinjeTrans.RegistrertBestilling = NO.
  END.

