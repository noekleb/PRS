DEF INPUT PARAM  ihVarebehLinjeTrans AS HANDLE NO-UNDO.
DEF INPUT PARAM  icAction            AS CHAR NO-UNDO.
DEF INPUT PARAM  icSessionId         AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocError             AS CHAR NO-UNDO.

DEF VAR cValues AS CHAR NO-UNDO.
DEF VAR cFields AS CHAR NO-UNDO.

cFields = DYNAMIC-FUNCTION("getCurrentValueFields" IN SOURCE-PROCEDURE).
cValues = DYNAMIC-FUNCTION("getCurrentValues" IN SOURCE-PROCEDURE).

FIND VarebehLinjeTrans EXCLUSIVE-LOCK
     WHERE ROWID(VarebehLinjeTrans) = ihVarebehLinjeTrans:ROWID
     NO-ERROR.
IF AVAIL VarebehLinjeTrans THEN DO:
  IF VarebehLinjeTrans.Bestilt1 > 0 OR VarebehLinjeTrans.Bestilt2 > 0 OR
     VarebehLinjeTrans.Bestilt3 > 0 OR VarebehLinjeTrans.Bestilt4 > 0 THEN
    ASSIGN VarebehLinjeTrans.GodkjentBestilling   = ENTRY(LOOKUP("levnrlist",cFields),cValues,"|") = "*"
           VarebehLinjeTrans.RegistrertBestilling = YES.
  ELSE
    ASSIGN VarebehLinjeTrans.GodkjentBestilling   = NO
           VarebehLinjeTrans.RegistrertBestilling = NO.
END.
ELSE ocError = "Bestillingslinje ikke tilgjengelig for oppdatering".


