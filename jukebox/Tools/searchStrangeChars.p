DEF VAR ix AS INT NO-UNDO.
DEF VAR iAsc AS INT NO-UNDO.
DEF VAR cStrangeList AS CHAR NO-UNDO.

DEF TEMP-TABLE ttStrange
    FIELD cStrangeText AS CHAR FORMAT "x(256)"
    FIELD cStrangeChar AS CHAR
    FIELD iStrangeAsc  AS INT.


FOR EACH val_ues NO-LOCK:
  DO ix = 1 TO LENGTH(val_ues.value_text):
    iAsc = ASC(SUBSTR(val_ues.value_text,ix,1)).
    IF iAsc > 138 OR iAsc < 33 THEN DO:
      IF NOT CAN-FIND(FIRST ttStrange WHERE ttStrange.iStrangeAsc = iAsc) THEN DO:
        CREATE ttStrange.
        ASSIGN ttStrange.cStrangeText = val_ues.value_text
               ttStrange.cStrangeChar = CHR(iAsc)
               ttStrange.iStrangeAsc  = iAsc.
      END.
    END.
  END.
END.

OUTPUT TO c:\temp\strangeChar2.csv.
FOR EACH ttStrange:
  EXPORT DELIMITER ";" ttStrange.
END.
OUTPUT CLOSE.
