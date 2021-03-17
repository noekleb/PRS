DEFINE VARIABLE obOk AS LOG NO-UNDO.

PROCEDURE Strekkode_Storl:
  DEF INPUT  PARAM irStrekkode  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.
  
  FIND Strekkode NO-LOCK
    WHERE ROWID(Strekkode) = irStrekkode
    NO-ERROR.
  IF AVAILABLE Strekkode THEN
  DO:
    FIND StrKonv OF Strekkode NO-LOCK NO-ERROR.
    IF AVAILABLE StrKonv THEN 
      ocValue = Strkonv.Storl.
    ELSE 
      ocValue = ''.
  END.
  ELSE 
    ocValue = ''.
END PROCEDURE. 
