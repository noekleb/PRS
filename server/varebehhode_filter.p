/* Bruk av parametere:
   entry(1,"¤"): Butikkliste
   entry(2,"¤"): Leverandørliste
   Dersom en av listene er ulik * og det fins en messe knyttet til vareh.boken
   så skal posten vises bare hvis "åpningstiden" for registrering er nå
----------------------------------------------------------------------------------------------------------------------*/      
DEF INPUT  PARAM irVarebehhode AS ROWID NO-UNDO.
DEF INPUT  PARAM icParam       AS CHAR NO-UNDO.
DEF INPUT  PARAM icSessionId   AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocValue       AS CHAR NO-UNDO.

DEF VAR cButikkListe AS CHAR NO-UNDO.
DEF VAR cLevNrListe  AS CHAR NO-UNDO INIT "*".
DEF VAR cUserId      AS CHAR NO-UNDO.

cButikkListe = REPLACE(ENTRY(1,icParam,"¤"),"&",",").
IF NUM-ENTRIES(icParam,"¤") > 1 THEN
  cLevNrListe  = REPLACE(ENTRY(2,icParam,"¤"),"&",",").
IF NUM-ENTRIES(icParam,"¤") > 2 THEN
  cUserId  = ENTRY(3,icParam,"¤").

IF cButikkListe NE "*" OR cLevNrListe NE "*" THEN DO:
  FIND FIRST Varebehhode WHERE ROWID(Varebehhode) = irVarebehhode NO-LOCK NO-ERROR.
  IF AVAIL VarebehHode THEN DO:
    FIND FIRST Messe OF VarebehHode NO-LOCK NO-ERROR.
    IF AVAIL Messe THEN DO:
      IF (PubliserStartDato > TODAY OR TODAY > PubliserStoppDato) OR PubliserStartDato = ? OR PubliserStoppDato = ? OR Messe.MesseType NE 1 THEN ocValue = "skiprow".
      ELSE IF cButikkListe NE "*" AND 
              CAN-FIND(FIRST MesseForButikk OF Messe) AND
              NOT CAN-FIND(FIRST MesseForButikk OF Messe WHERE CAN-DO(cButikkListe,STRING(MesseForButikk.ButikkNr)))
              THEN ocValue = "skiprow".
    END.
  END.
END.


IF ocValue NE "skiprow" AND cButikkListe NE "*" AND AVAIL Varebehhode AND cUserId NE "" THEN DO:
  FIND FIRST bruker NO-LOCK
       WHERE bruker.BrukerID = cUserId
       NO-ERROR.
  IF AVAIL bruker THEN DO:
    IF NOT CAN-FIND(FIRST VarebhBrukerGrp
                    WHERE VarebhBrukerGrp.VareBehNr = Varebehhode.VareBehNr
                      AND VarebhBrukerGrp.BrGrpNr   = bruker.BrGrpNr)
       AND CAN-FIND(FIRST VareBhBrukerGrp
                    WHERE VareBhBrukerGrp.VareBehNr = Varebehhode.VareBehNr) THEN
      ocValue = "skiprow".
  END.
END.
