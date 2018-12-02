DEF INPUT  PARAM irVarebehHode       AS ROWID NO-UNDO.
DEF INPUT  PARAM icVarebehHodeFilter AS CHAR NO-UNDO.
DEF INPUT  PARAM icSessionId         AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocValue             AS CHAR NO-UNDO.

DEF VAR ix      AS INT NO-UNDO.
DEF VAR bFunnet AS LOG NO-UNDO.

IF icVarebehHodeFilter NE "" THEN DO:
  FOR FIRST VarebehHode FIELDS(VarebehNr)
      WHERE ROWID(VarebehHode) = irVarebehHode NO-LOCK:
    DO ix = 1 TO NUM-ENTRIES(icVarebehHodeFilter,"¤"):
      IF CAN-FIND(FIRST VarebehLinje OF VarebehHode WHERE VarebehLinje.LevNr = INT(ENTRY(ix,icVarebehHodeFilter,"¤"))) THEN DO:
        bFunnet = TRUE.
        LEAVE.
      END.
    END.
  END.
  IF NOT bFunnet THEN ocValue = "skiprow".
END.
