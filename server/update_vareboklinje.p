/* Oppdatering av vareboklinje ved endring av en celle 

   Endret 17.09.07: Merknadskode skal kopieres fra varebok til varebok, dermed er kalkuleringsfunksjone flyttet nederst
                    iom at det er der kopieringen foretas                                                                             
-----------------------------------------------------------------------------*/

DEF INPUT  PARAM icVareboklinjeRowid AS CHAR NO-UNDO.
DEF INPUT  PARAM icFields            AS CHAR NO-UNDO.
DEF INPUT  PARAM icValues            AS CHAR NO-UNDO.
DEF INPUT  PARAM icSessionId         AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn            AS CHAR NO-UNDO.

DEF VAR hVBLbuffer AS HANDLE NO-UNDO.
DEF VAR bOK        AS LOG    NO-UNDO.
DEF VAR ix         AS INT    NO-UNDO.
DEF VAR cYY        AS CHAR   NO-UNDO.
DEF VAR cWW        AS CHAR   NO-UNDO.
DEF VAR oiWeek     AS INT    NO-UNDO.
DEF VAR dFirst     AS DATE   NO-UNDO.
DEF VAR cNyMerknad AS CHAR   NO-UNDO. 

hVBLbuffer = BUFFER VarebokLinje:HANDLE.

bOK = hVBLbuffer:FIND-BY-ROWID(TO-ROWID(icVareboklinjeRowid),EXCLUSIVE-LOCK,NO-WAIT) NO-ERROR.
IF bOk THEN DO TRANSACTION:

  IF icFields BEGINS "LevUke" THEN 
  LEVERINGSUKE:
  DO:
    IF (icValues = '0' OR icValues = '000000') THEN 
    DO:
        hVBLbuffer:BUFFER-FIELD("LevDato" + SUBSTR(icFields,LENGTH(icFields))):BUFFER-VALUE = ?.
        RETURN.
    END.
    ELSE DO:
        ASSIGN cYY = SUBSTR(icValues,1,4)
               cWW = SUBSTR(icValues,5)
               .
        IF INT(cYY) < YEAR(TODAY) OR INT(cYY) > YEAR(TODAY) + 10 OR cWW < "01" OR cWW > "53" THEN DO:
          ocReturn = "Ugyldig angivelse av uke".
          RETURN.
        END.
        DO ix = 1 TO 5:      
          RUN weeknum.p (DATE(1,ix,INT(cYY)), OUTPUT oiWeek).
          IF SUBSTR(STRING(oiWeek),5) = "01" THEN LEAVE.
        END.
        dFirst = DATE(1,ix,INT(cYY)) + INT(cWW) * 7 - 7 NO-ERROR.
    
        IF NOT ERROR-STATUS:ERROR THEN DO:
          hVBLbuffer:BUFFER-FIELD("LevDato" + SUBSTR(icFields,LENGTH(icFields))):BUFFER-VALUE = dFirst.
          IF icFields = "LevUke1" THEN DO:
            FIND FIRST ArtBas WHERE ArtBas.Artikkelnr = DEC(hVBLbuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) EXCLUSIVE-LOCK NO-ERROR.
            IF AVAIL ArtBas THEN
              ArtBas.LevDato1 = dFirst.
            ELSE ocReturn = "Kunne ikke oppdatere leveringsdato. Artikkelregister er låst".
          END.
        END.
        ELSE ocReturn = "Feil i konvertering av uke til dato".
        RETURN.
    END.
  END. /* LEVERINGSUKE */
  ELSE IF icFields = "FrittTillegg" THEN DO:
    FIND FIRST ArtBas WHERE ArtBas.Artikkelnr = DEC(hVBLbuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL ArtBas THEN
      ArtBas.FrittTillegg = IF icValues = "J" THEN TRUE ELSE FALSE.
    ELSE ocReturn = "Kunne ikke oppdatere fritt tillegg. Artikkelregister er låst".
    RETURN.
  END.
/*----- Skal ikke lenger gjøres. 
  ELSE IF icFields = "Kjedevare" THEN DO:
    FIND FIRST ArtBas WHERE ArtBas.Artikkelnr = DEC(hVBLbuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL ArtBas THEN
      ASSIGN
        ArtBas.KjedeVare = IF icValues = "J" THEN TRUE ELSE FALSE.
    ELSE DO:
      ocReturn = "Kunne ikke oppdatere kjedevare. Artikkelregister er låst".
      RETURN.
    END.
  END.
----*/
/* ------- Skal ikke lenger gjøres   
  ELSE IF icFields = "Gjennomfaktureres" THEN DO:
    FIND FIRST ArtBas WHERE ArtBas.Artikkelnr = DEC(hVBLbuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL ArtBas THEN
      ArtBas.Gjennomfaktureres = IF icValues = "J" THEN TRUE ELSE FALSE.
    ELSE DO:
      ocReturn = "Kunne ikke oppdatere gjennomfakturering. Artikkelregister er låst".
      RETURN.
    END.
  END.
------ */
  ELSE IF icFields = "LevFargKod" THEN DO:
    FIND FIRST ArtBas WHERE ArtBas.Artikkelnr = DEC(hVBLbuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL ArtBas THEN
      ArtBas.LevFargKod = icValues.
    ELSE ocReturn = "Kunne ikke oppdatere leverandørens fargeangivelse. Artikkelregister er låst".
    RETURN.
  END.
  ELSE IF icFields = "Beskr" THEN DO:
    FIND FIRST ArtBas WHERE ArtBas.Artikkelnr = DEC(hVBLbuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL ArtBas THEN
      ArtBas.Beskr = icValues.
    ELSE ocReturn = "Kunne ikke artikkelens beskrivelse. Artikkelregister er låst".
    RETURN.
  END.
  ELSE IF icFields = "Innkjopspris" THEN DO:
    FIND FIRST ArtBas WHERE ArtBas.Artikkelnr = DEC(hVBLbuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL ArtBas THEN
      ASSIGN 
      ArtBas.Katalogpris   = DEC(icValues)
      ArtBas.KjedeRab%     = DEC(hVBLbuffer:BUFFER-FIELD("KjedeRab%"):BUFFER-VALUE)
      ArtBas.KjedeInnkPris = DEC(hVBLbuffer:BUFFER-FIELD("KjedeInnkPris"):BUFFER-VALUE).
    ELSE ocReturn = "Kunne ikke artikkelens engros/katalog pris. Artikkelregister er låst".
/*     RETURN.  */
  END.
  ELSE IF icFields = "AnbefaltPris" THEN DO:
    FIND FIRST ArtBas WHERE ArtBas.Artikkelnr = DEC(hVBLbuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL ArtBas THEN
      ArtBas.Anbefaltpris = DEC(icValues).
    ELSE ocReturn = "Kunne ikke artikkelens anbefalte/veiledende pris. Artikkelregister er låst".
    RETURN.
  END.
  ELSE IF icFields = "KjedeInnkPris" THEN DO:
    FIND FIRST ArtBas WHERE ArtBas.Artikkelnr = DEC(hVBLbuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL ArtBas THEN
      ASSIGN 
      ArtBas.KjedeInnkPris = DEC(icValues)
      ArtBas.KjedeRab%     = DEC(hVBLbuffer:BUFFER-FIELD("KjedeRab%"):BUFFER-VALUE).
    ELSE ocReturn = "Kunne ikke artikkelens kjede innkj. pris. Artikkelregister er låst".
/*     RETURN.  */
  END.
  ELSE IF icFields = "KjedeRab%" THEN DO:
    FIND FIRST ArtBas WHERE ArtBas.Artikkelnr = DEC(hVBLbuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL ArtBas THEN
      ASSIGN 
      ArtBas.KjedeRab% = DEC(icValues)
      ArtBas.KjedeInnkPris = DEC(hVBLbuffer:BUFFER-FIELD("KjedeInnkPris"):BUFFER-VALUE).
    ELSE ocReturn = "Kunne ikke artikkelens kjederabatt. Artikkelregister er låst".
/*     RETURN.  */
  END.

  /* Oppdatering av vareboklinje */
  /* TN 17/3-11 Denne oppdaterer feltet som er valgt uansett om det er valgt eller ikke !!!*/
  IF hVBLbuffer:BUFFER-FIELD(icFields):DATA-TYPE = "DECIMAL" THEN
    hVBLbuffer:BUFFER-FIELD(icFields):BUFFER-VALUE = DEC(icValues).
  ELSE IF hVBLbuffer:BUFFER-FIELD(icFields):DATA-TYPE = "LOGICAL" THEN 
    DO:
    hVBLbuffer:BUFFER-FIELD(icFields):BUFFER-VALUE = IF icValues = "J" THEN TRUE ELSE FALSE.
    END.
  ELSE 
    hVBLbuffer:BUFFER-FIELD(icFields):BUFFER-VALUE = icValues.

/*   RUN vareboklinje_kalkuler.p (hVBLbuffer,icFields). */
  
  IF CAN-DO("supRab%,supVarekost",icFields) THEN DO:
    FIND FIRST ArtBas WHERE ArtBas.Artikkelnr = DEC(hVBLbuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL ArtBas THEN
      ArtBas.supRab% = hVBLbuffer:BUFFER-FIELD("supRab%"):BUFFER-VALUE.
    ELSE ocReturn = "Kunne ikke artikkelens suppleringsrabatt (VPI). Artikkelregister er låst".
  END.
  ELSE IF CAN-DO("forhRab%,Varekost",icFields) THEN DO:
    FIND FIRST ArtBas WHERE ArtBas.Artikkelnr = DEC(hVBLbuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL ArtBas THEN
      ArtBas.forhRab% = hVBLbuffer:BUFFER-FIELD("forhRab%"):BUFFER-VALUE.
    ELSE ocReturn = "Kunne ikke artikkelens forhåndsrabatt (VPI). Artikkelregister er låst".
  END.
  ELSE IF CAN-DO("Sortimentkoder,Kampanjeuker,Kampanjestotte,Lagerkoder",icFields) THEN DO:
    cNyMerknad = (IF hVBLbuffer:BUFFER-FIELD("Sortimentkoder"):BUFFER-VALUE NE "" THEN 
                    hVBLbuffer:BUFFER-FIELD("Sortimentkoder"):BUFFER-VALUE + ","
                  ELSE "") + 
                 (IF hVBLbuffer:BUFFER-FIELD("Kampanjeuker"):BUFFER-VALUE NE "" THEN 
                    hVBLbuffer:BUFFER-FIELD("Kampanjeuker"):BUFFER-VALUE + ","
                  ELSE "") +
                 (IF hVBLbuffer:BUFFER-FIELD("Kampanjestotte"):BUFFER-VALUE NE "" THEN 
                    hVBLbuffer:BUFFER-FIELD("Kampanjestotte"):BUFFER-VALUE + ","
                  ELSE "") +
                 (IF hVBLbuffer:BUFFER-FIELD("Lagerkoder"):BUFFER-VALUE NE "" THEN 
                    hVBLbuffer:BUFFER-FIELD("Lagerkoder"):BUFFER-VALUE + ","
                  ELSE "").
     IF cNyMerknad NE "" THEN DO:       
       cNyMerknad = SUBSTR(cNyMerknad,1,LENGTH(cNyMerknad) - 1).
       hVBLbuffer:BUFFER-FIELD("LinjeMerknad"):BUFFER-VALUE = cNyMerknad.
       FIND FIRST VarebokHode NO-LOCK 
            WHERE VarebokHode.VareBokNr = DEC(hVBLbuffer:BUFFER-FIELD("Vareboknr"):BUFFER-VALUE)
            NO-ERROR.
       IF AVAIL VarebokHode THEN DO: 
         FIND FIRST Messe EXCLUSIVE-LOCK 
              WHERE Messe.MesseNr = VarebokHode.MesseNr
              NO-ERROR.
         IF AVAIL Messe AND LOOKUP(cNyMerknad,Messe.Oppmerking,"¤") = 0 THEN
           Messe.Oppmerking = Messe.Oppmerking + (IF Messe.Oppmerking NE "" THEN "¤" ELSE "") + cNyMerknad.
       END.
     END.
     ELSE hVBLbuffer:BUFFER-FIELD("LinjeMerknad"):BUFFER-VALUE = cNyMerknad.
  END.

  RUN vareboklinje_kalkuler.p (hVBLbuffer,icFields).

  /* Disse må friskes opp etter at det er kalkulert. */
  IF CAN-DO("Innkjopspris,KjedeRab%,KjedeInnkPris",icFields) THEN DO:
    FIND FIRST ArtBas WHERE ArtBas.Artikkelnr = DEC(hVBLbuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL ArtBas THEN
      ASSIGN 
      ArtBas.KjedeRab%     = DEC(hVBLbuffer:BUFFER-FIELD("KjedeRab%"):BUFFER-VALUE)
      ArtBas.KjedeInnkPris = DEC(hVBLbuffer:BUFFER-FIELD("KjedeInnkPris"):BUFFER-VALUE).
  END.

END.
ELSE 
  ocReturn = "Vareboklinje ikke tilgj. for oppdatering".

