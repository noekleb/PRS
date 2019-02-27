/* Tildeling av fakturanummer 
   Parametere:  ENTRY(1,"|"): idlist/query: Angir om ENTRY(2) er en liste med fakturanumre skal prosesseres eller det kommer en where-betingelse
                ENTRY(2,"|"): liste eller where betingelse
                ENTRY(3,"|"): purretrinn
                Det kan også sendes en temp-tabell med fakturaer.
   Opprettet: 01.05.05 av BHa              
   Endret:    29.09.05 av BHa
            - Rettet retur av fakturaid-liste    
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
DEF VAR cKundeNrLst AS CHAR   NO-UNDO.
DEF VAR cFaktIdList AS CHAR   NO-UNDO.

DEF BUFFER bFakturaHode   FOR FakturaHode.
DEF BUFFER bKundereskontr FOR Kundereskontr.

FIND FIRST PurreTrinn
     WHERE PurreTrinn.PurreTrinn = INT(ENTRY(3,icParam,"|"))
     NO-LOCK NO-ERROR.
IF NOT AVAIL Purretrinn THEN DO:
  ocReturn = "Det mangler oppsett i Purretrinn-tabell for purretrinn " + ENTRY(3,icParam,"|").
  RETURN.
END.

IF ENTRY(1,icParam,"|") = "idlist" THEN DO:
  cIdList = ENTRY(2,icParam,"|").
  DO ix = 1 TO NUM-ENTRIES(cIdList):
    FIND FIRST Kundereskontr
         WHERE Kundereskontr.Reskontro_id = DEC(ENTRY(ix,cIdList))
         EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL Kundereskontr THEN DO ON ERROR UNDO, LEAVE:
      RUN ProduserFaktura (OUTPUT obOK).
      IF NOT obOk THEN LEAVE.
      IF NOT CAN-DO(cKundeNrLst,STRING(Kundereskontr.KundeNr)) THEN
        cKundeNrLst = cKundeNrLst + STRING(Kundereskontr.KundeNr) + ",".
    END.
  END.
END.
/* WHERE betingelse: */
ELSE IF ihBuffer = ? THEN DO:
  hBuffer = BUFFER FakturaHode:HANDLE.
  CREATE QUERY hQuery.
  hQuery:SET-BUFFERS(hBuffer).
  hQuery:QUERY-PREPARE("FOR EACH FakturaHode NO-LOCK " + ENTRY(2,icParam)).
  hQuery:QUERY-OPEN().

  hQuery:GET-FIRST().
  REPEAT WHILE NOT hQuery:QUERY-OFF-END:
    FIND FIRST Kundereskontr
         WHERE Kundereskontr.Reskontro_id = DEC(hBuffer:BUFFER-FIELD("Reskontro_id"):BUFFER-VALUE)
         EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL Kundereskontr THEN DO ON ERROR UNDO, LEAVE:
      RUN ProduserFaktura (OUTPUT obOK).
      IF NOT obOk THEN UNDO, LEAVE.
      IF NOT CAN-DO(cKundeNrLst,STRING(Kundereskontr.KundeNr)) THEN
        cKundeNrLst = cKundeNrLst + STRING(Kundereskontr.KundeNr) + ",".
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
    FIND FIRST Kundereskontr
         WHERE Kundereskontr.Reskontro_id = DEC(ihBuffer:BUFFER-FIELD("Reskontro_id"):BUFFER-VALUE)
         EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL Kundereskontr THEN DO ON ERROR UNDO, LEAVE:
      RUN ProduserFaktura (OUTPUT obOK).
      IF NOT obOk THEN UNDO, LEAVE.
      IF NOT CAN-DO(cKundeNrLst,STRING(Kundereskontr.KundeNr)) THEN
        cKundeNrLst = cKundeNrLst + STRING(Kundereskontr.KundeNr) + ",".
    END.

    hQuery:GET-NEXT().
  END.
  DELETE OBJECT hQuery.
END.

RUN beregn_kunde_saldo.p ("idlist|" + TRIM(cKundeNrLst,","),?,icSessionId,OUTPUT ocReturn,OUTPUT obOk).

IF ocReturn = "" THEN
  ASSIGN ocReturn = TRIM(cFaktIdList,",")
         obOk     = TRUE.

PROCEDURE ProduserFaktura:
  DEF OUTPUT PARAM obOK AS LOG NO-UNDO INIT TRUE.

  DEF VAR ix AS INT NO-UNDO.

  FIND FIRST FakturaHode
       WHERE FakturaHode.FakturaNr = Kundereskontr.FakturaNr
         AND FakturaHode.KundeNr   = Kundereskontr.KundeNr
       NO-LOCK NO-ERROR.

  IF NOT AVAIL FakturaHode THEN DO:
    ASSIGN ocReturn = "Finner ikke faktura " + STRING(Kundereskontr.FakturaNr) + " for kunde " + STRING(Kundereskontr.KundeNr) + " (!!)".
           obOk     = FALSE.
    RETURN.
  END.

  FIND FIRST Butiker WHERE Butiker.butik = FakturaHode.ButikkNr NO-LOCK NO-ERROR.
  IF NOT AVAIL Butiker THEN DO:
    ASSIGN ocReturn = "Faktura " + STRING(Faktura_id) + " mangler butikkangivelse".
           obOk     = FALSE.
    RETURN.
  END.

  FIND Bilagstype NO-LOCK WHERE
      Bilagstype.Bilagstype = 10 NO-ERROR.
  IF NOT AVAIL Bilagstype THEN DO:
    ASSIGN ocReturn = "Bilagstype 10, purring mangler i database".
           obOk     = FALSE.
    RETURN.
  END.
  FIND Kunde NO-LOCK WHERE
      Kunde.KundeNr = FakturaHode.KundeNr NO-ERROR.

  ASSIGN Kundereskontr.PurreTrinn     = INT(ENTRY(3,icParam,"|"))
         Kundereskontr.SistePurreDato = TODAY
         .

  CREATE bFakturaHode.
  BUFFER-COPY FakturaHode EXCEPT Faktura_id Totalt TO bFakturaHode.
  ASSIGN bFakturaHode.BilagsType    = 10
         bFakturaHode.FakturertDato = TODAY
         bFakturaHode.ForfallsDato  = TODAY
         bFakturaHode.FNotat        = PurreTrinn.PurreNotat
         bFakturaHode.BTTekst       = Bilagstype.BTTekst
         bFakturaHode.Totalrabatt%  = 0
         bFakturaHode.TotalrabattKr = 0
         bFakturaHode.MvaKr         = 0
         bFakturaHode.Mva           = 0
         bFakturaHode.NettoPris     = 0
         bFakturaHode.TotaltVolum   = 0
         bFakturaHode.AvgFriSalg    = 0
         bFakturaHode.AvgPlSalg     = 0
         bFakturaHode.AvrundingKr   = 0
         bFakturaHode.BetTekst      = ""
         bFakturaHode.Totalt        = Kundereskontr.Saldo
         .

  CREATE FakturaLinje.
  ASSIGN FakturaLinje.Faktura_Id     = bFakturaHode.Faktura_id
         FakturaLinje.Varetekst      = "Faktura " + STRING(FakturaHode.FakturaNr) 
         FakturaLinje.EkstRefTekst   = "Oppr.forfallsdato: " + STRING(FakturaHode.ForfallsDato)
                                     + ". Oppr.beløp: " + STRING(FakturaHode.Totalt) + ", herav MVA: " + STRING(FakturaHode.MvaKr) 
         FakturaLinje.LeveringsDato  = FakturaHode.LeveringsDato
         FakturaLinje.NettoLinjeSum  = Kundereskontr.Saldo
         FakturaLinje.LinjeSum       = Kundereskontr.Saldo
         FakturaLinje.FakturaLinjeNr = 1
         .

  FIND Bilagstype NO-LOCK WHERE
      Bilagstype.Bilagstype = 11 NO-ERROR.
            
  IF Butiker.PurreGebyr NE 0 AND Kunde.Purregebyr AND AVAIL BilagsType THEN DO:
    ix = 1.
    /* Legger først på evt tidligere purregebyrer: */
    FOR EACH bKundereskontr NO-LOCK
        WHERE bKundereskontr.FakturaNr = FakturaHode.FakturaNr
          AND bKundereskontr.BilagsType = 11:
      ix = ix + 1.
      CREATE FakturaLinje.
      ASSIGN FakturaLinje.Faktura_Id     = bFakturaHode.Faktura_id
             FakturaLinje.Varetekst      = (IF AVAILABLE Bilagstype
                                            THEN Bilagstype.BTTEkst
                                            ELSE "Purregebyr") + ", " + STRING(bKundereskontr.FakturertDato)
             FakturaLinje.NettoLinjeSum  = bKundereskontr.Belop
             FakturaLinje.LinjeSum       = bKundereskontr.Belop
             FakturaLinje.FakturaLinjeNr = ix
             FakturaLinje.Opphav         = 21
             bFakturaHode.Totalt         = bFakturaHode.Totalt + FakturaLinje.NettoLinjeSum
             .
    END.

    ix = ix + 1.
    CREATE FakturaLinje.
    ASSIGN FakturaLinje.Faktura_Id     = bFakturaHode.Faktura_id
           FakturaLinje.Varetekst      = (IF AVAILABLE Bilagstype
                                            THEN Bilagstype.BTTEkst
                                            ELSE "Purregebyr") + ", " + STRING(TODAY)
           FakturaLinje.NettoLinjeSum  = Butiker.PurreGebyr
           FakturaLinje.LinjeSum       = Butiker.PurreGebyr
           FakturaLinje.FakturaLinjeNr = ix
           FakturaLinje.Opphav         = 21
           bFakturaHode.Totalt         = bFakturaHode.Totalt + FakturaLinje.NettoLinjeSum
           .

    CREATE bKundereskontr.
    BUFFER-COPY bFakturaHode TO bKundereskontr.
    ASSIGN bKundereskontr.Belop         = Butiker.PurreGebyr
           bKundereskontr.Saldo         = Butiker.PurreGebyr
           bKundereskontr.FakturertDato = TODAY
           bKundereskontr.ForfallsDato  = TODAY
           bKundereskontr.BilagsType    = 11
           bKundereskontr.BArtNr        = Kundereskontr.BArtNr
           .
  END.
  ELSE IF Butiker.PurreGebyr NE 0 AND NOT AVAIL BilagsType THEN DO:
    ASSIGN ocReturn = "Bilagstype 11, purregebyr mangler i database".
           obOk     = FALSE.
    RETURN.
  END.

  cFaktIdList = cFaktIdList + STRING(bFakturaHode.Faktura_id) + ",".
END PROCEDURE.
