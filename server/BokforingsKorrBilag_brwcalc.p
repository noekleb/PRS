/* BokforingsKorrBilag_brwcalc.p */

PROCEDURE BokforingsKorrBilag_Type:
  DEF INPUT  PARAM irRecid  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.
  
    FIND BokforingsKorrBilag NO-LOCK
        WHERE ROWID(BokforingsKorrBilag) = irRecid
        NO-ERROR.
    IF AVAILABLE BokforingsKorrBilag THEN 
    DO:
      CASE BokforingsKorrBilag.TTId:
          WHEN  52 THEN ocValue = 'KredK'.
          WHEN  58 THEN ocValue = 'Bank'.
          WHEN  61 THEN ocValue = 'Innbet'.
          WHEN  62 THEN ocValue = 'Utbet'.
          WHEN 800 THEN ocValue = 'BPose'.
      END CASE.
    END.
    ELSE ocValue = ''.

END PROCEDURE.
