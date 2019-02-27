DEF INPUT PARAM irPkSdlLinje  AS ROWID NO-UNDO.
DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.

FIND PkSdlLinje WHERE ROWID(PkSdlLinje) = irPkSdlLinje NO-LOCK NO-ERROR.
IF AVAIL PkSdlLinje THEN DO:
  FIND FIRST Strekkode NO-LOCK 
       WHERE Strekkode.ArtikkelNr = PkSdlLinje.ArtikkelNr
         AND Strekkode.StrKode    = PkSdlLinje.StrKode
         AND NOT Strekkode.Kode   BEGINS "02" 
       NO-ERROR.
   ocValue = STRING(AVAIL Strekkode).
END.
