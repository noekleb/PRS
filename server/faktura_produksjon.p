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
DEF VAR cKundeNrLst AS CHAR   NO-UNDO.
DEF VAR dFakturaDato AS DATE  NO-UNDO.

DEF BUFFER bFakturaHode FOR FakturaHode.

dFakturaDato = TODAY.
IF NUM-ENTRIES(icParam,'|') > 2 THEN
    dFakturaDato = DATE(ENTRY(3,icParam,'|')).
IF dFakturadato = ? THEN dFakturaDato = TODAY.

IF ENTRY(1,icParam,"|") = "idlist" THEN DO:
  cIdList = ENTRY(2,icParam,"|").
  DO ix = 1 TO NUM-ENTRIES(cIdList):
    FIND FIRST bFakturaHode
         WHERE bFakturaHode.Faktura_id = DEC(ENTRY(ix,cIdList))
           AND bFakturaHode.FakturaNr  = ?
         EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL bFakturaHode THEN DO:
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
    IF AVAIL bFakturaHode THEN DO:
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
    IF AVAIL bFakturaHode THEN DO:
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
  DEF OUTPUT PARAM obOK AS LOG NO-UNDO INIT TRUE.

  DEF VAR lDec AS DEC FORMAT "->>>>>>>>>>>>9" NO-UNDO.

  DEF VAR fFakturaNr  AS DEC    NO-UNDO.

  RUN getfakturanr.p (bFakturaHode.BilagsType,OUTPUT fFakturaNr,bFakturaHode.ButikkNr).

  IF fFakturaNr = ? OR fFakturaNr = 0 THEN DO:
    ASSIGN obOK = FALSE
           ocReturn = "Ugyldig fakturanr".
    RETURN.      
  END.

  IF bFakturaHode.BilagsType = 1 THEN DO:
    FIND FIRST BetalingsBetingelser
         OF bFakturaHode NO-LOCK NO-ERROR.
    IF NOT AVAIL Betalingsbetingelser THEN DO:
      FIND FIRST Betalingsbetingelser NO-LOCK NO-ERROR.
      IF NOT AVAIL Betalingsbetingelser THEN DO:
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
         bFakturaHode.ForfallsDato    = TODAY + IF AVAIL BetalingsBetingelser THEN Betalingsbetingelser.AntKredittDager ELSE 0
         .

  /* KID */
  lDec = fFakturaNr.
  RUN bibl_getchk.p (INPUT-OUTPUT lDec).
  bFakturaHode.KID = DEC(STRING(fFakturaNr,'>>>>>>>>>>>>9') + STRING(lDec)).
        
  CREATE Kundereskontr.
  BUFFER-COPY bFakturaHode TO Kundereskontr.
  ASSIGN Kundereskontr.Belop      = bFakturahode.Totalt
         Kundereskontr.Saldo      = bFakturahode.Totalt
         Kundereskontr.BartNr     = 1
         Kundereskontr.BilagsType = bFakturaHode.BilagsType
         .
END PROCEDURE.
