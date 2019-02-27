/* Tildeling av fakturanummer 
   Parametere:  <"idlist"/"query">: Angir om en liste med fakturanumre skal prosesseres eller det kommer en where-betingelse
                Det kan også sendes en temp-tabell med fakturaer.
   Opprettet: 01.05.05 av BHa                  
-----------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER icParam     AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ihBuffer    AS HANDLE NO-UNDO.
DEFINE INPUT  PARAMETER icSessionId AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER ocReturn    AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER obOK        AS LOG NO-UNDO.

DEFINE VARIABLE hQuery      AS HANDLE NO-UNDO.
DEFINE VARIABLE ix          AS INTEGER    NO-UNDO.
DEFINE VARIABLE cIdList     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE hBuffer     AS HANDLE NO-UNDO.
DEFINE VARIABLE cKundeNrLst AS CHARACTER   NO-UNDO.
DEFINE VARIABLE dFakturaDato AS DATE  NO-UNDO.
DEFINE VARIABLE hJbApi AS HANDLE NO-UNDO.
DEFINE VARIABLE iOrdreOpphav AS INTEGER NO-UNDO.

DEFINE BUFFER bFakturaHode FOR FakturaHode.

dFakturaDato = TODAY.
IF NUM-ENTRIES(icParam,'|') > 2 THEN
    dFakturaDato = DATE(ENTRY(3,icParam,'|')).
IF dFakturadato = ? THEN dFakturaDato = TODAY.
IF NUM-ENTRIES(icParam,'|') <= 4 THEN 
    iOrdreOpphav = INT(ENTRY(4,icParam,'|')) NO-ERROR.

IF ENTRY(1,icParam,"|") = "idlist" THEN DO:
  cIdList = ENTRY(2,icParam,"|").
  DO ix = 1 TO NUM-ENTRIES(cIdList):
    FIND FIRST bFakturaHode
         WHERE bFakturaHode.Faktura_id = DEC(ENTRY(ix,cIdList))
           AND bFakturaHode.FakturaNr  = ?
         EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE bFakturaHode THEN DO:
/*       RUN update_fakturahode.p (DEC(ENTRY(ix,cIdList)),"INIT","",1). */
     /* sista parameter har inte tidigare använts */
     /* sätter den till 9 när vi fakturerar       */
     /* i update_f... så skapas faktavgift vid samlefaktura endast vid prodution av faktura */
      RUN update_fakturahode.p (DEC(ENTRY(ix,cIdList)),"INIT","",9). 
      RUN ProduserFaktura (OUTPUT obOK).
      IF NOT obOk THEN LEAVE.
      IF NOT CAN-DO(cKundeNrLst,STRING(bFakturaHode.KundeNr)) THEN
        cKundeNrLst = cKundeNrLst + STRING(bFakturaHode.KundeNr) + ",".
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
    FIND FIRST bFakturaHode
         WHERE bFakturaHode.Faktura_id = DEC(hBuffer:BUFFER-FIELD("Faktura_id"):BUFFER-VALUE)
           AND bFakturaHode.FakturaNr  = ?
         EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE bFakturaHode THEN DO:
/*       RUN update_fakturahode.p (DEC(hBuffer:BUFFER-FIELD("Faktura_id"):BUFFER-VALUE),"INIT","",1). */
      RUN update_fakturahode.p (DEC(hBuffer:BUFFER-FIELD("Faktura_id"):BUFFER-VALUE),"INIT","",9). 
      RUN ProduserFaktura (OUTPUT obOK).
      IF NOT obOk THEN LEAVE.
      IF NOT CAN-DO(cKundeNrLst,STRING(bFakturaHode.KundeNr)) THEN
        cKundeNrLst = cKundeNrLst + STRING(bFakturaHode.KundeNr) + ",".
    END.

    hQuery:GET-NEXT().
  END.
  DELETE OBJECT hQuery.
  IF ocReturn = "" THEN
    RUN beregn_kunde_saldo.p ("idlist|" + cIdList,?,icSessionId,OUTPUT ocReturn,OUTPUT obOk).
END.
/* Temp-tabell: */
ELSE DO:
  CREATE QUERY hQuery.
  hQuery:SET-BUFFERS(ihBuffer).
  hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " BY Dato").
  hQuery:QUERY-OPEN().

  hQuery:GET-FIRST().
  REPEAT WHILE NOT hQuery:QUERY-OFF-END:
    FIND FIRST bFakturaHode
         WHERE bFakturaHode.Faktura_id = DEC(ihBuffer:BUFFER-FIELD("Faktura_id"):BUFFER-VALUE)
           AND bFakturaHode.FakturaNr  = ?
         EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE bFakturaHode THEN DO:
/*       RUN update_fakturahode.p (DEC(ihBuffer:BUFFER-FIELD("Faktura_id"):BUFFER-VALUE),"INIT","",1). */
      RUN update_fakturahode.p (DEC(ihBuffer:BUFFER-FIELD("Faktura_id"):BUFFER-VALUE),"INIT","",9). 
      RUN ProduserFaktura (OUTPUT obOK).
      IF NOT obOk THEN LEAVE.
      IF NOT CAN-DO(cKundeNrLst,STRING(bFakturaHode.KundeNr)) THEN
        cKundeNrLst = cKundeNrLst + STRING(bFakturaHode.KundeNr) + ",".
    END.

    hQuery:GET-NEXT().
  END.
  DELETE OBJECT hQuery.
END.

IF ocReturn = "" THEN
  RUN beregn_kunde_saldo.p ("idlist|" + TRIM(cKundeNrLst,","),?,icSessionId,OUTPUT ocReturn,OUTPUT obOk).

IF ocReturn = "" THEN obOk = TRUE.

PROCEDURE ProduserFaktura:
  DEFINE OUTPUT PARAMETER obOK AS LOG NO-UNDO INIT TRUE.

  DEFINE VARIABLE lDec AS DECIMAL FORMAT "->>>>>>>>>>>>9" NO-UNDO.
  DEFINE VARIABLE lSumPos AS DECIMAL NO-UNDO.
  DEFINE VARIABLE lSumNeg AS DECIMAL NO-UNDO.
  
  DEFINE VARIABLE fFakturaNr  AS DECIMAL    NO-UNDO.

  DEFINE BUFFER bufKunderesKontr FOR KunderesKontr.
  
  RUN getfakturanr.p (bFakturaHode.BilagsType,OUTPUT fFakturaNr,bFakturaHode.ButikkNr).

  IF fFakturaNr = ? OR fFakturaNr = 0 THEN DO:
    ASSIGN obOK = FALSE
           ocReturn = "Ugyldig fakturanr".
    RETURN.      
  END.

  IF bFakturaHode.BilagsType = 1 THEN DO:
    FIND FIRST BetalingsBetingelser
         OF bFakturaHode NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Betalingsbetingelser THEN DO:
      FIND FIRST Betalingsbetingelser NO-LOCK NO-ERROR.
      IF NOT AVAILABLE Betalingsbetingelser THEN DO:
        ASSIGN ocReturn = "Betalingsbetingelser mangler i systemoppsett".
               obOk     = FALSE.
        RETURN.
      END.
      ELSE bFakturaHode.betbet = BetalingsBetingelser.betbet.
    END.
  END.

  ASSIGN bFakturaHode.FakturaNr       = fFakturaNr
         bFakturaHode.FakturertDato   = dFakturadato
         bFakturaHode.ProduksjonsDato = TODAY
         bFakturaHode.ForfallsDato    = TODAY + IF AVAILABLE BetalingsBetingelser THEN Betalingsbetingelser.AntKredittDager ELSE 0
         .

  /* KID */
  lDec = fFakturaNr.
  RUN bibl_getchk.p (INPUT-OUTPUT lDec).
  bFakturaHode.KID = DEC(STRING(fFakturaNr,'>>>>>>>>>>>>9') + STRING(lDec)).

  ASSIGN 
    lSumPos = 0
    lSumNeg = 0
    .

  /* Kundeordre fra nettbutikk. */
  IF iOrdreOpphav = 10 THEN     
  FOR EACH FakturaLinje OF bFakturaHode NO-LOCK:
      IF FakturaLinje.Linjesum > 0 THEN 
        lSumPos = lSumPos + FakturaLinje.Linjesum.
      ELSE 
        lSumNeg = lSumNeg + Abs(FakturaLinje.Linjesum).
  END.
  
  /* Faktura reskontropost. */      
  CREATE Kundereskontr.
  BUFFER-COPY bFakturaHode TO Kundereskontr.
  ASSIGN 
    Kundereskontr.Belop      = bFakturahode.Totalt
    Kundereskontr.Saldo      = bFakturahode.Totalt
    Kundereskontr.BartNr     = 1
    Kundereskontr.BilagsType = bFakturaHode.BilagsType
    .
    
  /* Posterer reskontro for nettbutikk ordre. */
  IF iOrdreOpphav = 10 THEN 
  DO:
      ASSIGN 
        Kundereskontr.Belop = lSumPos
        Kundereskontr.Saldo = lSumPos - lSumNeg
        .
      CREATE bufKunderesKontr.
      BUFFER-COPY 
        Kundereskontr
        EXCEPT Kundereskontr.Reskontro_Id Kundereskontr.BilagsType
        TO bufKundereskontr
        ASSIGN 
            bufKundereskontr.BilagsType = 3
            bufKundereskontr.Belop = lSumNEG
            bufKundereskontr.Saldo = lSumPos - lSumNeg
            . 
      /* Kobler faktura mot betaling. */
      CREATE KundeResKobling.
      ASSIGN 
        KundeResKobling.DReskontro_Id = Kundereskontr.Reskontro_Id
        KundeResKobling.KReskontro_Id = bufKundereskontr.Reskontro_Id
        KundeResKobling.Belop         = Kundereskontr.Belop
        KundeResKobling.Dato          = Kundereskontr.FakturertDato
        .
        
  END.       
    
END PROCEDURE.
