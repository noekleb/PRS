DEF INPUT  PARAM irVarebokLinje  AS ROWID NO-UNDO.
DEF INPUT  PARAM icSessionId     AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn        AS CHAR NO-UNDO.

DEF VAR fArtikkelnr AS DEC NO-UNDO.
DEF VAR fVareboknr  AS DEC NO-UNDO.
DEF VAR fMessenr    AS DEC NO-UNDO.

DEF BUFFER bVarebokLinje FOR VarebokLinje.

FIND bVarebokLinje WHERE ROWID(bVarebokLinje) = irVarebokLinje NO-LOCK NO-ERROR.
IF NOT AVAIL bVarebokLinje THEN RETURN.

FIND FIRST VarebokHode OF bVarebokLinje NO-LOCK NO-ERROR.
IF NOT AVAIL VarebokHode THEN RETURN.

ASSIGN fArtikkelnr = bVarebokLinje.ArtikkelNr
       fVarebokNr  = bVarebokLinje.VarebokNr
       fMessenr    = VarebokHode.MesseNr
       .

FOR EACH VareBokHode FIELDS() NO-LOCK
    WHERE VareBokHode.MesseNr   = fMessenr
      AND VareBokHode.VareBokNr NE fVareBokNr,
    FIRST VareBokLinje FIELDS(VareBokNr) OF VareBokHode NO-LOCK
          WHERE VareBokLinje.ArtikkelNr = fArtikkelNr
    :
  ocReturn = ocReturn + STRING(VareBokHode.VareBokNr) + ",".
END.
ocReturn = TRIM(ocReturn,",").
