DEF INPUT PARAM  irBuffer      AS ROWID NO-UNDO.
DEF INPUT PARAM  icParam       AS CHAR  NO-UNDO.
DEF INPUT PARAM  icSessionId   AS CHAR  NO-UNDO.
DEF OUTPUT PARAM ocReturn      AS CHAR  NO-UNDO INIT "no".

DEF VAR ix     AS INT NO-UNDO.
DEF VAR bMatch AS LOG NO-UNDO.

IF icParam = "" THEN RETURN.

FIND VareBehLinje WHERE ROWID(VareBehLinje) = irBuffer NO-LOCK NO-ERROR.
IF AVAIL VarebehLinje THEN DO ix = 1 TO NUM-ENTRIES(icParam,"¤"):
  IF CAN-DO(VarebehLinje.LinjeMerknad,ENTRY(ix,icParam,"¤")) THEN DO:
    bMatch = TRUE.
    LEAVE.
  END.
END.
IF NOT bMatch THEN ocReturn = "skiprow".
