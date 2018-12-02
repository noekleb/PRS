DEF INPUT  PARAM ifKundeNr   AS DEC  NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocValue     AS CHAR NO-UNDO.

DEF VAR fSaldo  AS DEC NO-UNDO.

FOR EACH Kundereskontr FIELDS(Saldo)
    WHERE Kundenr = ifKundeNr 
      AND Saldo   NE 0
      AND ForfallsDato LT TODAY
    NO-LOCK:
  fSaldo = fSaldo + Kundereskontr.Saldo.
END.

ocValue = STRING(fSaldo).
