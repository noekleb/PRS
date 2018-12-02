DEF INPUT  PARAM irVarebehLinje  AS ROWID NO-UNDO.
DEF INPUT  PARAM icSessionId     AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn        AS CHAR NO-UNDO.

DEF VAR fArtikkelnr AS DEC NO-UNDO.
DEF VAR fVarebehnr  AS DEC NO-UNDO.
DEF VAR fMessenr    AS DEC NO-UNDO.

DEF BUFFER bVarebehLinje FOR VarebehLinje.

FIND bVarebehLinje WHERE ROWID(bVarebehLinje) = irVarebehLinje NO-LOCK NO-ERROR.
IF NOT AVAIL bVarebehLinje THEN RETURN.

FIND FIRST VarebehHode OF bVarebehLinje NO-LOCK NO-ERROR.
IF NOT AVAIL VarebehHode THEN RETURN.

ASSIGN fArtikkelnr = bVarebehLinje.ArtikkelNr
       fVarebehNr  = bVarebehLinje.VarebehNr
       fMessenr    = VarebehHode.MesseNr
       .

FOR EACH VarebehHode NO-LOCK
    WHERE VarebehHode.MesseNr   = fMessenr
      AND VarebehHode.VarebehNr NE fVarebehNr,
    FIRST VarebehLinje OF VarebehHode NO-LOCK
          WHERE VarebehLinje.ArtikkelNr = fArtikkelNr
    :
  ocReturn = ocReturn + STRING(VarebehHode.VarebehNr) + ",".
END.
ocReturn = TRIM(ocReturn,",").
