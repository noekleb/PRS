/* Frisker opp fakturainformasjonen 
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
         EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL bFakturaHode THEN DO:
      RUN update_fakturahode.p (DEC(ENTRY(ix,cIdList)),"INIT","",1). 
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
         EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL bFakturaHode THEN DO:
      RUN update_fakturahode.p (DEC(hBuffer:BUFFER-FIELD("Faktura_id"):BUFFER-VALUE),"INIT","",1). 
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
    FIND FIRST bFakturaHode
         WHERE bFakturaHode.Faktura_id = DEC(ihBuffer:BUFFER-FIELD("Faktura_id"):BUFFER-VALUE)
         EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL bFakturaHode THEN DO:
      RUN update_fakturahode.p (DEC(hBuffer:BUFFER-FIELD("Faktura_id"):BUFFER-VALUE),"INIT","",1). 
    END.

    hQuery:GET-NEXT().
  END.
  DELETE OBJECT hQuery.
END.

/*IF ocReturn = "" THEN obOk = TRUE.*/
obOk = TRUE.
