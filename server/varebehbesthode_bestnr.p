DEF INPUT  PARAM irVarebehBestHode AS ROWID NO-UNDO.
DEF INPUT  PARAM icParam           AS CHAR  NO-UNDO.
DEF INPUT  PARAM icSessionId       AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocValue           AS CHAR NO-UNDO.

DEF VAR iBestStat     AS INT  NO-UNDO.
DEF VAR cButikkListe  AS CHAR NO-UNDO.
DEF VAR bMatch        AS LOG  NO-UNDO.

IF icParam = "" THEN RETURN.

ASSIGN iBestStat   = INT(ENTRY(1,icParam,"¤"))
       cButikkListe  = REPLACE(ENTRY(2,icParam,"¤"),CHR(1),",").

FIND VarebehBestHode WHERE ROWID(VarebehBestHode) = irVarebehBestHode NO-LOCK NO-ERROR.
IF AVAIL VarebehBestHode THEN DO:
  IF CAN-FIND(BestHode OF VarebehBestHode) THEN DO:
    IF iBestStat NE 0 THEN DO:
      FOR FIRST BestHode FIELDS(BestStat) OF VarebehBestHode NO-LOCK
          WHERE (IF iBestStat = 4 THEN BestHode.BestStat = iBestStat AND BestHode.BekreftetDato = ?
                 ELSE IF iBestStat = 44 THEN BestHode.BestStat = 4 AND BestHode.BekreftetDato NE ?
                 ELSE BestHode.BestStat = iBestStat):
        ocValue = STRING(VarebehBestHode.BestNr).
      END.
      IF ocValue = "" THEN ocValue = "skiprow".
    END.
    ELSE ocValue = STRING(VarebehBestHode.BestNr).

    IF cButikkListe NE "*" AND ocValue NE "skiprow" THEN DO:
      FOR EACH BestLinje NO-LOCK
          WHERE BestLinje.BestNr = VarebehBestHode.BestNr
            AND CAN-DO(cButikkListe,STRING(BestLinje.Butik)):
        bMatch = YES.
      END.
      IF NOT bMatch THEN ocValue = "skiprow".
    END.
  END.
END.
