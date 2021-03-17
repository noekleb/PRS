DEF INPUT PARAM  irBuffer      AS ROWID NO-UNDO.
DEF INPUT PARAM  icParam       AS CHAR  NO-UNDO.
DEF INPUT PARAM  icSessionId   AS CHAR  NO-UNDO.
DEF OUTPUT PARAM ocReturn      AS CHAR  NO-UNDO INIT "no".

DEF VAR iButikkNr AS INT NO-UNDO.

IF icParam = "" THEN RETURN.
ELSE iButikkNr = INT(ENTRY(1,icParam,"¤")).

FIND VareBehLinje WHERE ROWID(VareBehLinje) = irBuffer NO-LOCK NO-ERROR.
IF AVAIL VarebehLinje THEN DO:
  ocReturn = STRING(CAN-FIND(FIRST VareBehLinjeTrans OF VarebehLinje WHERE
                          VarebehLinjeTrans.ButikkNr = iButikkNr AND
                          (Bestilt1 > 0 OR
                           Bestilt2 > 0 OR
                           Bestilt3 > 0 OR
                           Bestilt4 > 0)
                    )).
  IF NUM-ENTRIES(icParam,"¤") > 1 THEN DO:
    IF ENTRY(2,icParam,"¤") = "1" AND ocReturn = "no" THEN ocReturn = "skiprow".
    ELSE IF ENTRY(2,icParam,"¤") = "2" AND ocReturn = "yes" THEN ocReturn = "skiprow".
  END.  
END.
