/* Tildeling av fakturanummer 
   Parametere:  <"idlist"/"query">: Angir om en liste med fakturanumre skal prosesseres eller det kommer en where-betingelse
                Det kan også sendes en temp-tabell med fakturaer.
   Opprettet: 01.05.05 av BHa                  
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR hQuery      AS HANDLE NO-UNDO.
DEF VAR ix          AS INT    NO-UNDO.
DEF VAR cIdList     AS CHAR   NO-UNDO.
DEF VAR hBuffer     AS HANDLE NO-UNDO.

DEF VAR pdForsteDato AS DATE NO-UNDO.
DEF VAR pdDatoSiste  AS DATE NO-UNDO.
DEF VAR plSaldo      AS DEC  NO-UNDO.
DEF VAR plTotKjop    AS DEC  NO-UNDO.
DEF VAR plBetaling   AS DEC  NO-UNDO.

DEF BUFFER bKunde FOR Kunde.

IF ENTRY(1,icParam,"|") = "idlist" THEN DO:
  cIdList = ENTRY(2,icParam,"|").
  DO ix = 1 TO NUM-ENTRIES(cIdList):
    FIND FIRST bKunde
         WHERE bKunde.KundeNr = DEC(ENTRY(ix,cIdList))
         EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
    IF AVAIL bKunde AND NOT LOCKED bKunde THEN DO:
      RUN BeregnKundesaldo (OUTPUT obOK).
      IF NOT obOk THEN LEAVE.
    END.
  END.
END.
/* WHERE betingelse: */
ELSE IF ihBuffer = ? THEN DO:
  hBuffer = BUFFER Kunde:HANDLE.
  CREATE QUERY hQuery.
  hQuery:SET-BUFFERS(hBuffer).
  hQuery:QUERY-PREPARE("FOR EACH Kunde NO-LOCK " + ENTRY(2,icParam)).
  hQuery:QUERY-OPEN().

  hQuery:GET-FIRST().
  REPEAT WHILE NOT hQuery:QUERY-OFF-END:
    FIND FIRST bKunde
         WHERE bKunde.KundeNr = DEC(hBuffer:BUFFER-FIELD("KundeNr"):BUFFER-VALUE)
         EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
    IF AVAIL bKunde AND NOT LOCKED bKunde THEN DO:
      RUN BeregnKundesaldo (OUTPUT obOK).
      RELEASE bKunde.
      IF NOT obOk THEN LEAVE.
    END.

    hQuery:GET-NEXT().
  END.
  DELETE OBJECT hQuery.
END.
/* Temp-tabell: */
ELSE DO:
  CREATE QUERY hQuery.
  hQuery:SET-BUFFERS(ihBuffer).
  hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " BY Dato").
  hQuery:QUERY-OPEN().

  hQuery:GET-FIRST().
  REPEAT WHILE NOT hQuery:QUERY-OFF-END:
    FIND FIRST bKunde
         WHERE bKunde.KundeNr = DEC(ihBuffer:BUFFER-FIELD("KundeNr"):BUFFER-VALUE)
         EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
    IF AVAIL bKunde AND NOT LOCKED bKunde THEN DO:
      RUN BeregnKundesaldo (OUTPUT obOK).
      RELEASE bKunde.
      IF NOT obOk THEN LEAVE.
    END.

    hQuery:GET-NEXT().
  END.
  DELETE OBJECT hQuery.
END.

IF ocReturn = "" THEN obOk = TRUE.

PROCEDURE BeregnKundesaldo:
  DEF OUTPUT PARAM obOK AS LOG NO-UNDO INIT TRUE.

  DEFINE BUFFER bufKundeSaldo FOR KundeSaldo.

  ASSIGN
      bKunde.ForsteKjop = ?
      bKunde.SisteKjop  = ?
      bKunde.KundeSaldo = 0
      plTotKjop         = 0
      plBetaling        = 0
      .

  /* Tar bort gammel saldopost.                                   */
  /* Kundesaldo benyttes ikke lenger. Saldo oppdateres mot kunde. */
  FOR EACH KundeSaldo NO-LOCK WHERE
      KundeSaldo.KundeNr   = bKunde.KundeNr:
      DO FOR bufKundeSaldo:
        FIND bufKundeSaldo EXCLUSIVE-LOCK WHERE 
          ROWID(bufKundeSaldo) = ROWID(KundeSaldo) NO-ERROR NO-WAIT.  
        IF AVAILABLE bufKundeSaldo AND NOT LOCKED bufKundeSaldo THEN   
          DELETE bufKundeSaldo.
      END.
  END.
  
  BLOKKEN:
  DO:
    ASSIGN
      pdForsteDato = ?
      pdDatoSiste  = ?
      plSaldo      = 0
      .

    /* Summerer kundereskontro DEBET. */
    FOR EACH FakturaHode NO-LOCK WHERE
      FakturaHode.KundeNr  = bKunde.KundeNr AND
      FakturaHode.FakturaNr = ?:
      ASSIGN
          plTotKjop    = plTotKjop + FakturaHode.Totalt
          .
    END.

    /* Summerer kundereskontro DEBET. */
    FOR EACH Kundereskontr NO-LOCK WHERE
      Kundereskontr.KundeNr  = bKunde.KundeNr AND
      Kundereskontr.Saldo   > 0:
      ASSIGN
          plTotKjop    = plTotKjop + Kundereskontr.Saldo
          .
    END.

    /* Summerer kundereskontro KREDIT. */
    FOR EACH Kundereskontr NO-LOCK WHERE
      Kundereskontr.KundeNr  = bKunde.KundeNr AND
      Kundereskontr.Saldo   < 0:
      ASSIGN
          plBetaling = plBetaling + abs(Kundereskontr.Saldo)
          .
    END.

    ASSIGN
      bKunde.KundeSaldo = plTotKjop - plBetaling.

    FIND FIRST KundeTrans NO-LOCK WHERE
      KundeTrans.KundeNr = bKunde.KundeNr NO-ERROR.
    IF AVAILABLE KundeTrans THEN
      bKunde.ForsteKjop  = KundeTrans.Dato.
    FIND LAST KundeTrans NO-LOCK WHERE
      KundeTrans.KundeNr = bKunde.KundeNr NO-ERROR.
    IF AVAILABLE KundeTrans THEN
      bKunde.SisteKjop  = KundeTrans.Dato.
  
  END. /* BLOKKEN */



  DO:

  END.
END PROCEDURE.
