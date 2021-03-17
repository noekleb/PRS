/* Sjekk om leverandør eksisterer i varebok
----------------------------------------------------------------------------------------------------------------------*/      
DEF INPUT  PARAM irLevBas         AS ROWID NO-UNDO.
DEF INPUT  PARAM icVarebokNr      AS CHAR NO-UNDO.
DEF INPUT  PARAM icSessionId      AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocValue          AS CHAR NO-UNDO.

DEF VAR bFunnet AS LOG NO-UNDO.

FIND LevBas NO-LOCK
     WHERE ROWID(LevBas) = irLevBas
     NO-ERROR.
IF NOT AVAIL LevBas THEN RETURN.

FOR FIRST VarebokLinje NO-LOCK
    WHERE VarebokLinje.LevNr = LevBas.LevNr
      AND VarebokLinje.VarebokNr = DEC(icVarebokNr)
      AND VarebokLinje.ArtikkelNr > 0:
  bFunnet = TRUE.
END.

IF NOT bFunnet THEN
  ocValue = "skiprow".
