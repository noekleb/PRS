DEF INPUT PARAM irVarebokLinje AS ROWID NO-UNDO.
DEF INPUT  PARAM icSessionId   AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocValue       AS CHAR NO-UNDO.

DEF VAR oiWeek AS INT NO-UNDO.
DEFINE VARIABLE dStart AS DATE NO-UNDO.

FIND VarebokLinje WHERE ROWID(VarebokLinje) = irVarebokLinje NO-LOCK NO-ERROR.
IF AVAIL VarebokLinje THEN 
  DO:
    RUN weeknum.p (VarebokLinje.LevDato1,OUTPUT oiWeek).
    ocValue = STRING(oiWeek).
  END.

/*
  FOR FIRST ArtBas FIELDS(LevDato1) NO-LOCK
      WHERE ArtBas.ArtikkelNr = VarebokLinje.ArtikkelNr:
  
    RUN weeknum.p (ArtBas.LevDato1,OUTPUT oiWeek).
    ocValue = STRING(oiWeek).

  END.
*/

IF ocValue = ? THEN ocValue = "".
