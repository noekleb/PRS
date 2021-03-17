DEF INPUT PARAM irPkSdlLinje  AS ROWID NO-UNDO.
DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.

FIND PkSdlLinje WHERE ROWID(PkSdlLinje) = irPkSdlLinje NO-LOCK NO-ERROR.
IF AVAIL PkSdlLinje THEN DO:
  
  FIND FIRST Strekkode NO-LOCK 
       WHERE Strekkode.Kode = PkSdlLinje.Kode NO-ERROR.
       
  IF TRIM(PkSdlLinje.Kode) = '' OR NOT AVAILABLE Strekkode THEN 
      ocValue = 'No'.
  ELSE ocValue = IF Strekkode.ArtikkelNr <> PkSdlLinje.ArtikkelNr THEN 'Yes' ELSE 'No'.
END.
ELSE ocValue = 'No'.
